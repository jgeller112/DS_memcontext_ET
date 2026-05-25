suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(tibble)
})

screen_w_px <- 1920
screen_h_px <- 1080

pic_x_min <- 610;  pic_x_max <- 1310
pic_y_min <- 265;  pic_y_max <- 815
pic_x_mid <- (pic_x_min + pic_x_max) / 2

extract_pid <- function(path) {
  pid <- str_extract(basename(path), "^DS\\d+[_-]\\d+")
  if (is.na(pid)) pid <- tools::file_path_sans_ext(basename(path))
  pid
}

read_behavioral <- function(path, n_practice = 3) {
  # Encoding starts with `n_practice` practice trials that have no
  # eye-tracking data — the Tobii msg file's onset events start at the
  # first *real* trial. Drop those rows here so behavioral trial 1
  # aligns with ET trial 1; otherwise the (participant, trial) join
  # silently shifts every Condition/emo/location label.
  read_csv(path, show_col_types = FALSE) |>
    filter(!is.na(Background), !is.na(Object), !is.na(Condition)) |>
    slice(-seq_len(n_practice)) |>
    mutate(
      participant = extract_pid(path),
      trial       = row_number(),
      phase       = "encoding"
    ) |>
    select(
      participant, trial, Background, Object, Condition, List,
      mouse_clicked_name = mouse.clicked_name, phase
    )
}

read_recognition_behavioral <- function(path) {
  read_csv(path, show_col_types = FALSE) |>
    filter(!is.na(back.started)) |>
    slice_head(n = 90) |>
    mutate(
      participant = extract_pid(path),
      trial       = row_number(),
      phase       = "recognition"
    ) |>
    select(
      participant, trial, Background, Composite, Object, Condition,
      stimulus_status, List, response, accuracy, phase
    )
}

# The recognition CSV holds a second old/new block: object recognition. Those
# rows are the ones where the *real* object routine ran (`object.started`
# populated) and are disjoint from the background-recognition rows. Practice
# trials are automatically excluded: they run under different routine columns
# (`object_prac_*`, `object_again`) and never set `object.started`, so this
# filter keeps the 90 scored trials only (verified: 60 old / 30 foils, no
# practice overlap). Old objects were seen at encoding and carry a
# "<emo>-<location>" Condition; new objects are foils with Condition
# "foil_<emo>". `emo` (neg / neu) is derived from Condition so memory can be
# broken down by emotion. stimulus_status / response / accuracy are the same
# signal-detection columns used for background recognition, so
# recognition_accuracy() works unchanged on this output.
read_object_recognition <- function(path, n_trials = 90) {
  read_csv(path, show_col_types = FALSE) |>
    filter(!is.na(object.started)) |>
    slice_head(n = n_trials) |>
    mutate(
      participant = extract_pid(path),
      trial       = row_number(),
      phase       = "object_recognition",
      emo = case_when(
        str_detect(Condition, "neg") ~ "neg",
        str_detect(Condition, "neu") ~ "neu",
        TRUE                         ~ NA_character_
      )
    ) |>
    select(
      participant, trial, Object, Condition, emo,
      stimulus_status, List, response, accuracy, phase
    )
}

read_gaze <- function(path) {
  read_csv(path, show_col_types = FALSE) |>
    select(
      system_time_stamp,
      left_gaze_point_on_display_area_x,
      left_gaze_point_on_display_area_y,
      right_gaze_point_on_display_area_x,
      right_gaze_point_on_display_area_y
    ) |>
    mutate(
      participant = extract_pid(path),
      time_ms     = system_time_stamp / 1000,
      left_x_px   = left_gaze_point_on_display_area_x  * screen_w_px,
      left_y_px   = left_gaze_point_on_display_area_y  * screen_h_px,
      right_x_px  = right_gaze_point_on_display_area_x * screen_w_px,
      right_y_px  = right_gaze_point_on_display_area_y * screen_h_px,
      avg_x_px    = rowMeans(cbind(left_x_px, right_x_px), na.rm = TRUE),
      avg_y_px    = rowMeans(cbind(left_y_px, right_y_px), na.rm = TRUE)
    ) |>
    select(
      participant, system_time_stamp, time_ms,
      left_x_px, left_y_px, right_x_px, right_y_px,
      avg_x_px, avg_y_px
    )
}

read_msg <- function(path) {
  read_csv(path, show_col_types = FALSE) |>
    mutate(participant = extract_pid(path)) |>
    select(participant, system_time_stamp, msg)
}

parse_validation <- function(msg_df) {
  msg_df |>
    filter(str_detect(msg, "^validation data quality")) |>
    mutate(
      Dev_L  = as.numeric(str_match(msg, "Dev_L:\\s*([0-9.]+)")[, 2]),
      Dev_R  = as.numeric(str_match(msg, "Dev_R:\\s*([0-9.]+)")[, 2]),
      RMS_L  = as.numeric(str_match(msg, "RMS_L:\\s*([0-9.]+)")[, 2]),
      RMS_R  = as.numeric(str_match(msg, "RMS_R:\\s*([0-9.]+)")[, 2]),
      LOSS_L = as.numeric(str_match(msg, "LOSS_L:\\s*([0-9.]+)")[, 2]),
      LOSS_R = as.numeric(str_match(msg, "LOSS_R:\\s*([0-9.]+)")[, 2]),
      SD_L   = as.numeric(str_match(msg, "SD_L:\\s*([0-9.]+)")[, 2]),
      SD_R   = as.numeric(str_match(msg, "SD_R:\\s*([0-9.]+)")[, 2])
    ) |>
    select(participant, system_time_stamp,
           Dev_L, Dev_R, RMS_L, RMS_R, LOSS_L, LOSS_R, SD_L, SD_R)
}

recognition_accuracy <- function(data, groupvars = "participant",
                                 status_col   = "stimulus_status",
                                 accuracy_col = "accuracy",
                                 old_label    = "old",
                                 new_label    = "new") {
  data |>
    group_by(across(all_of(groupvars))) |>
    summarise(
      n_old     = sum(.data[[status_col]] == old_label, na.rm = TRUE),
      n_new     = sum(.data[[status_col]] == new_label, na.rm = TRUE),
      n_hit     = sum(.data[[status_col]] == old_label &
                        .data[[accuracy_col]] == 1, na.rm = TRUE),
      n_cr      = sum(.data[[status_col]] == new_label &
                        .data[[accuracy_col]] == 1, na.rm = TRUE),
      n_total   = sum(!is.na(.data[[accuracy_col]])),
      n_correct = sum(.data[[accuracy_col]] == 1, na.rm = TRUE),
      .groups   = "drop"
    ) |>
    mutate(
      n_fa     = n_new - n_cr,
      n_miss   = n_old - n_hit,
      accuracy = n_correct / n_total,
      hit_rate = (n_hit + 0.5) / (n_old + 1),
      fa_rate  = (n_fa  + 0.5) / (n_new + 1),
      d_prime  = qnorm(hit_rate) - qnorm(fa_rate),
      c_bias   = -0.5 * (qnorm(hit_rate) + qnorm(fa_rate))
    )
}

downsample_gaze <- function(dataframe, bin.length = 1000 / 120,
                            timevar = "timestamp",
                            aggvars = c("participant", "trial", "time_bin")) {
  dataframe <- dataframe |>
    mutate(time_bin = floor(.data[[timevar]] / bin.length) * bin.length)

  if (identical(aggvars, "none")) return(dataframe)

  dataframe |>
    group_by(across(all_of(aggvars))) |>
    summarize(
      left_x_px  = mean(left_x_px,  na.rm = TRUE),
      left_y_px  = mean(left_y_px,  na.rm = TRUE),
      right_x_px = mean(right_x_px, na.rm = TRUE),
      right_y_px = mean(right_y_px, na.rm = TRUE),
      avg_x_px   = mean(avg_x_px,   na.rm = TRUE),
      avg_y_px   = mean(avg_y_px,   na.rm = TRUE),
      .groups    = "drop"
    )
}

extract_msg_events <- function(msg_df) {
  msg_df |>
    filter(str_detect(msg, "^(onset|offset)_")) |>
    mutate(
      event = str_extract(msg, "^(onset|offset)"),
      stim  = str_remove(msg, "^(onset|offset)_")
    ) |>
    select(participant, system_time_stamp, msg, event, stim)
}

# Split a `<background>.<object>.<emo>.<location>.<ext>` composite filename
# into its component columns. Some stims use `_` as the delimiter — those
# are normalized to `.` first. NA inputs propagate to NA in every output
# column, so it's safe to call on Composite columns that include "new"-lure
# rows where no composite was encoded.
decompose_composite <- function(df, col = "stim",
                                into = c("background", "object",
                                         "emo", "location")) {
  src <- df[[col]]
  norm <- if_else(
    !is.na(src) & str_count(src, fixed(".")) < 4L,
    str_replace_all(coalesce(src, ""), "_", "."),
    src
  )
  parts <- str_split_fixed(coalesce(norm, ""), fixed("."), length(into) + 1L)
  for (i in seq_along(into)) {
    df[[into[i]]] <- if_else(is.na(norm), NA_character_, parts[, i])
  }
  df
}

build_trial_stim_encoding <- function(msg_events) {
  msg_events |>
    filter(event == "onset") |>
    group_by(participant) |>
    arrange(system_time_stamp, .by_group = TRUE) |>
    mutate(trial = row_number()) |>
    ungroup() |>
    select(participant, trial, stim) |>
    decompose_composite(col = "stim") |>
    select(participant, trial, stim, background, object, emo, location)
}

build_trial_stim_recognition <- function(msg_events) {
  msg_events |>
    filter(event == "onset") |>
    group_by(participant) |>
    arrange(system_time_stamp, .by_group = TRUE) |>
    mutate(trial = row_number()) |>
    ungroup() |>
    select(participant, trial, stim)
}

# Per-participant nearest-sample as-of merge: attach each message to the
# gaze sample whose system_time_stamp is closest. Vectorized via
# findInterval; no event-only rows are introduced. Adds `match_offset_us`
# (matched sample ts − msg ts) so you can see how far each message slid.
nearest_msg_to_gaze <- function(gaze, msg_events) {
  if (nrow(gaze) == 0) return(gaze)
  gaze <- gaze |> arrange(system_time_stamp)
  if (nrow(msg_events) == 0) {
    return(gaze |>
             mutate(msg = NA_character_, event = NA_character_,
                    stim = NA_character_, match_offset_us = NA_real_))
  }
  msg_events <- msg_events |> arrange(system_time_stamp)
  sample_ts  <- gaze$system_time_stamp
  idx_lo     <- findInterval(msg_events$system_time_stamp, sample_ts)
  idx_lo[idx_lo == 0L] <- 1L
  idx_hi     <- pmin(idx_lo + 1L, length(sample_ts))
  d_lo       <- abs(sample_ts[idx_lo] - msg_events$system_time_stamp)
  d_hi       <- abs(sample_ts[idx_hi] - msg_events$system_time_stamp)
  nearest    <- ifelse(d_hi < d_lo, idx_hi, idx_lo)

  msg_matched <- msg_events |>
    mutate(matched_ts      = sample_ts[nearest],
           match_offset_us = sample_ts[nearest] - system_time_stamp) |>
    group_by(matched_ts) |>
    slice_min(abs(match_offset_us), n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(matched_ts, msg, event, stim, match_offset_us)

  gaze |>
    left_join(msg_matched, by = c("system_time_stamp" = "matched_ts"))
}

assign_gaze_to_trials <- function(gaze, msg_events, trial_stim,
                                  decompose_stim = TRUE,
                                  trial_dur_ms = 5000) {
  # Run the nearest-sample merge per participant — you can't match a
  # message in subject A against a gaze sample in subject B.
  joined <- gaze |>
    group_by(participant) |>
    group_modify(~ nearest_msg_to_gaze(
      .x,
      filter(msg_events, participant == .y$participant)
    )) |>
    ungroup() |>
    group_by(participant) |>
    arrange(system_time_stamp, .by_group = TRUE) |>
    mutate(
      is_onset      = !is.na(event) & event == "onset",
      is_offset     = !is.na(event) & event == "offset",
      onset_run     = cumsum(is_onset),
      offset_before = lag(cumsum(is_offset), default = 0),
      in_trial      = onset_run > offset_before,
      trial         = if_else(in_trial, onset_run, NA_integer_)
    ) |>
    ungroup() |>
    select(-stim) |>
    left_join(trial_stim, by = c("participant", "trial")) |>
    filter(!is.na(trial)) |>
    group_by(participant, trial) |>
    mutate(time_ms = time_ms - min(time_ms)) |>
    ungroup()

  # Truncate each trial to a fixed window (default 5000 ms). PsychoPy's
  # back-task pic is nominally on screen for 5 s; trimming here means every
  # downstream analysis sees the same window.
  if (!is.null(trial_dur_ms) && is.finite(trial_dur_ms)) {
    joined <- joined |> filter(time_ms <= trial_dur_ms)
  }

  if (decompose_stim) {
    joined |>
      select(participant, trial, time_ms, msg, event, match_offset_us,
             stim, background, object, emo, location,
             left_x_px, left_y_px, right_x_px, right_y_px,
             avg_x_px, avg_y_px)
  } else {
    joined |>
      select(participant, trial, time_ms, msg, event, match_offset_us,
             stim,
             left_x_px, left_y_px, right_x_px, right_y_px,
             avg_x_px, avg_y_px)
  }
}

# Missing-data QC on the trial-assigned gaze. A sample is "missing" when
# the binocular average is NA on either axis (i.e., both eyes were lost
# or one eye dropped and the other never recovered). Returns one row per
# (participant, trial). Designed to run on the 5-s-truncated `gaze_trial`
# coming out of assign_gaze_to_trials(), so prop_missing reflects loss
# within the analyzed window.
trial_missing <- function(gaze_trial, trial_dur_ms = 5000) {
  gaze_trial |>
    group_by(participant, trial) |>
    summarise(
      n_samples    = n(),
      n_missing    = sum(is.na(avg_x_px) | is.na(avg_y_px)),
      prop_missing = n_missing / n_samples,
      duration_ms  = suppressWarnings(
        max(time_ms, na.rm = TRUE) - min(time_ms, na.rm = TRUE)
      ),
      .groups = "drop"
    ) |>
    mutate(
      trial_dur_ms = trial_dur_ms,
      duration_ms  = if_else(is.finite(duration_ms), duration_ms, NA_real_)
    )
}

# Per-subject missing-data rollup. Mean / median / max prop_missing across
# that subject's trials, plus a count of trials with >50% missing — a
# common threshold for flagging unusable trials.
subject_missing <- function(trial_missing_df, bad_trial_threshold = 0.50) {
  trial_missing_df |>
    group_by(participant) |>
    summarise(
      n_trials               = n(),
      total_samples          = sum(n_samples,  na.rm = TRUE),
      total_missing          = sum(n_missing,  na.rm = TRUE),
      mean_prop_missing      = mean(prop_missing,   na.rm = TRUE),
      median_prop_missing    = median(prop_missing, na.rm = TRUE),
      max_prop_missing       = max(prop_missing,    na.rm = TRUE),
      n_trials_over_thresh   = sum(prop_missing > bad_trial_threshold,
                                   na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(bad_trial_threshold = bad_trial_threshold)
}

label_aoi <- function(fix_df) {
  fix_df |>
    mutate(
      x_pic = x - pic_x_min,
      y_pic = y - pic_y_min,
      AOI = case_when(
        x_pic >= 0   & x_pic <  350 & y_pic >= 0 & y_pic <= 550 ~ "Left",
        x_pic >= 350 & x_pic <= 700 & y_pic >= 0 & y_pic <= 550 ~ "Right",
        TRUE ~ "Outside"
      )
    )
}

run_ivt_per_trial <- function(gaze_pp, ivt_params = list()) {
  if (!requireNamespace("kollaR", quietly = TRUE)) {
    stop("kollaR is not installed. Install with: install.packages('kollaR') or remotes::install_github('jonaskdahl/kollaR')")
  }
  defaults <- list(
    velocity.filter.ms    = 20,
    velocity.threshold    = 30,
    min.fixation.duration = 60,
    merge.ms.threshold    = 75,
    distance.threshold    = 0.5,
    one_degree            = 40
  )
  p <- modifyList(defaults, ivt_params)

  safe_ivt <- possibly(function(d) {
    kollaR::algorithm_ivt(
      d, xcol = "avg_x_px", ycol = "avg_y_px",
      velocity.filter.ms    = p$velocity.filter.ms,
      velocity.threshold    = p$velocity.threshold,
      min.fixation.duration = p$min.fixation.duration,
      merge.ms.threshold    = p$merge.ms.threshold,
      distance.threshold    = p$distance.threshold,
      one_degree            = p$one_degree
    )
  }, otherwise = NULL)

  gaze_pp |>
    nest(.by = c(participant, trial)) |>
    mutate(ivt = map(data, safe_ivt))
}

ivt_fixations <- function(ivt_results) {
  ivt_results |>
    filter(!map_lgl(ivt, is.null)) |>
    mutate(fixations = map(ivt, "fixations")) |>
    select(participant, trial, fixations) |>
    unnest(fixations)
}

ivt_saccades <- function(ivt_results) {
  ivt_results |>
    filter(!map_lgl(ivt, is.null)) |>
    mutate(saccades = map(ivt, "saccades")) |>
    select(participant, trial, saccades) |>
    unnest(saccades)
}

preprocess_120hz <- function(gaze_trial) {
  if (!requireNamespace("kollaR", quietly = TRUE)) {
    stop("kollaR is not installed.")
  }
  # With the nearest-sample merge in assign_gaze_to_trials(), every row in
  # gaze_trial is a real gaze sample (event info, if any, is merged onto
  # the sample). No need to filter out event rows — they don't exist.
  gaze_120hz <- gaze_trial |>
    rename(timestamp = time_ms) |>
    downsample_gaze(bin.length = 1000 / 120) |>
    rename(timestamp = time_bin)

  gaze_120hz |>
    group_by(participant, trial) |>
    group_modify(~ kollaR::preprocess_gaze(
      .x, xcol = "avg_x_px", ycol = "avg_y_px"
    )) |>
    ungroup()
}

# Stack encoding and recognition AOI-labeled fixations into one long frame
# keyed on (participant, Background), restricted to backgrounds that
# appear in *both* phases per participant. `recognition_scope` controls
# which recognition trials count:
#   - "old_correct" (default): stimulus_status == "old" & accuracy == 1.
#     Matches the eyesim reinstatement scope.
#   - "all": every recognition trial, regardless of status/accuracy.
# Pairing is computed *after* the recognition filter, so "all" can yield
# more paired Backgrounds than "old_correct" (e.g., when the participant
# missed an old item the Background still appears in recognition).
build_fixations_long <- function(enc_fix, rec_fix,
                                 recognition_scope = c("old_correct", "all")) {
  recognition_scope <- match.arg(recognition_scope)
  keep_cols <- c("participant", "Background", "Condition", "List",
                 "AOI", "x", "y", "duration", "onset")
  enc_long <- enc_fix |>
    mutate(phase = "encoding") |>
    select(all_of(keep_cols), phase)
  rec_filtered <- if (recognition_scope == "old_correct") {
    rec_fix |> filter(stimulus_status == "old", accuracy == 1)
  } else {
    rec_fix
  }
  rec_long <- rec_filtered |>
    mutate(phase = "recognition") |>
    select(all_of(keep_cols), phase)

  paired_keys <- inner_join(
    distinct(enc_long, participant, Background),
    distinct(rec_long, participant, Background),
    by = c("participant", "Background")
  )

  bind_rows(enc_long, rec_long) |>
    semi_join(paired_keys, by = c("participant", "Background")) |>
    arrange(participant, Background, phase, onset)
}

# Per-(participant, Background, AOI) side-by-side comparison. Outputs
# encoding_* and recognition_* metric columns so the table is readable
# at a glance. Outside fixations dropped.
summarise_per_background <- function(fixations_long) {
  fixations_long |>
    filter(AOI != "Outside") |>
    group_by(participant, Background, Condition, phase, AOI) |>
    summarise(
      n_fix       = n(),
      mean_dur    = mean(duration, na.rm = TRUE),
      total_dwell = sum(duration,  na.rm = TRUE),
      .groups     = "drop"
    ) |>
    pivot_wider(
      names_from  = phase,
      values_from = c(n_fix, mean_dur, total_dwell),
      names_glue  = "{phase}_{.value}",
      values_fill = list(n_fix = 0L, mean_dur = NA_real_, total_dwell = 0)
    )
}

# Roll the per-Background paired summary up to (participant, Condition,
# AOI). One row per cell with mean-across-Backgrounds metrics, encoding
# vs recognition side by side.
summarise_per_condition <- function(fixations_long) {
  per_bg <- fixations_long |>
    filter(AOI != "Outside") |>
    group_by(participant, Background, Condition, phase, AOI) |>
    summarise(
      n_fix       = n(),
      mean_dur    = mean(duration, na.rm = TRUE),
      total_dwell = sum(duration,  na.rm = TRUE),
      .groups     = "drop"
    )

  per_bg |>
    group_by(participant, Condition, phase, AOI) |>
    summarise(
      n_backgrounds         = n_distinct(Background),
      mean_n_fix            = mean(n_fix,       na.rm = TRUE),
      mean_fix_duration     = mean(mean_dur,    na.rm = TRUE),
      mean_total_dwell_time = mean(total_dwell, na.rm = TRUE),
      .groups               = "drop"
    ) |>
    pivot_wider(
      names_from  = phase,
      values_from = c(n_backgrounds, mean_n_fix,
                      mean_fix_duration, mean_total_dwell_time),
      names_glue  = "{phase}_{.value}"
    )
}

# Gaze reinstatement (eyesim). Tests whether the recognition-phase fixation
# pattern reinstates the encoding-phase pattern for the same Background
# *within participant*. Mirrors the qmd eyesim workflow:
#   1. eye_table() keyed on participant + phase + Background + Condition,
#      with fixations clipped to the picture box (relative coords).
#   2. density_by() — Gaussian fixation-density map per group (sigma px).
#   3. template_similarity() comparing each recognition density map to the
#      *same* participant+Background encoding map (match_on = pair_id =
#      "Background::Condition"), benchmarked against a within-participant
#      permutation null. `eye_sim_diff` is the observed-minus-permuted
#      similarity in Fisher-z space — the per-pair reinstatement effect.
# `fixations_long` must come from build_fixations_long(); already restricted
# to Backgrounds present in both phases. Returns the per-pair similarity
# table (eye_sim, perm_sim, eye_sim_diff + grouping columns).
run_reinstatement <- function(fixations_long, sigma = 80,
                              permutations = 1000, seed = 1234,
                              method = "spearman") {
  if (!requireNamespace("eyesim", quietly = TRUE)) {
    stop("eyesim is not installed. Install with: remotes::install_github('bbuchsbaum/eyesim')")
  }
  fixations_long_eye <- fixations_long |>
    mutate(pair_id = paste(Background, Condition, sep = "::"))

  eyetab <- eyesim::eye_table(
    x = "x", y = "y",
    duration = "duration", onset = "onset",
    groupvar = c("participant", "phase", "Background", "Condition", "pair_id"),
    data    = fixations_long_eye,
    clip_bounds     = c(pic_x_min, pic_x_max, pic_y_min, pic_y_max),
    relative_coords = TRUE
  )

  eyedens <- eyesim::density_by(
    eyetab,
    groups  = c("participant", "phase", "Background", "Condition", "pair_id"),
    sigma   = sigma,
    xbounds = c(pic_x_min, pic_x_max),
    ybounds = c(pic_y_min, pic_y_max)
  )

  enc_dens <- eyedens |> filter(phase == "encoding")
  rec_dens <- eyedens |> filter(phase == "recognition")

  set.seed(seed)
  eyesim::template_similarity(
    ref_tab      = enc_dens,
    source_tab   = rec_dens,
    match_on     = "pair_id",   # recognition→encoding for same Background
    permute_on   = "participant",
    method       = method,
    permutations = permutations
  )
}

# Roll the per-pair reinstatement table up to one row per Condition: mean
# observed similarity, mean permuted similarity, and the mean/SD of the
# Fisher-z difference (the corrected reinstatement effect).
reinstatement_by_condition <- function(reinstatement) {
  reinstatement |>
    group_by(Condition) |>
    summarise(
      mean_eye_sim      = mean(eye_sim,      na.rm = TRUE),
      mean_perm_sim     = mean(perm_sim,     na.rm = TRUE),
      mean_eye_sim_diff = mean(eye_sim_diff, na.rm = TRUE),
      sd_eye_sim_diff   = sd(eye_sim_diff,   na.rm = TRUE),
      n_pairs           = n(),
      .groups           = "drop"
    )
}

# Gaze reinstatement via Left/Right discriminability (AUC) — the approach
# used in the emotional-memory eye-tracking literature this study is modeled
# on. Objects were placed Left or Right of the scene; if the gaze pattern
# carries that spatial information, fixations on right-placed trials sit
# further right than on left-placed trials. For each (participant, phase,
# emo) we score every trial's lateral gaze bias and compute the AUC for
# discriminating right- from left-placed trials (Wilcoxon–Mann–Whitney
# statistic = P(score_right > score_left)). AUC = 0.5 is chance (no spatial
# reinstatement), 1.0 is perfect separation. Encoding AUC indexes perceptual
# looking-at-the-object; recognition AUC is the reinstatement measure.
#
# `bias` picks the per-trial lateral score: "dwell" = (right−left dwell time)
# / total, "count" = same on fixation counts; both in [-1, 1]. A trial's side
# (location) comes from the `Condition` label ("<emo>-<location>"). Bootstrap
# over trials gives a 95% CI per cell. Note: this is the 1-D (horizontal)
# reduction of the paper's 2-D-KDE AUC — appropriate here because the only
# spatial manipulation is left vs. right.
run_auc_reinstatement <- function(fixations_long,
                                  bias = c("dwell", "count"),
                                  boot = 1000, seed = 1234) {
  bias <- match.arg(bias)
  set.seed(seed)

  per_trial <- fixations_long |>
    filter(AOI %in% c("Left", "Right"),
           !is.na(Condition), str_detect(Condition, "-")) |>
    separate_wider_delim(Condition, delim = "-",
                         names = c("emo", "location"),
                         cols_remove = FALSE, too_many = "merge") |>
    group_by(participant, phase, emo, location, Background) |>
    summarise(
      dwell_left  = sum(duration[AOI == "Left"],  na.rm = TRUE),
      dwell_right = sum(duration[AOI == "Right"], na.rm = TRUE),
      n_left      = sum(AOI == "Left"),
      n_right     = sum(AOI == "Right"),
      .groups     = "drop"
    ) |>
    mutate(
      total = if (bias == "dwell") dwell_left + dwell_right else n_left + n_right,
      score = if (bias == "dwell")
        (dwell_right - dwell_left) / total
      else
        (n_right - n_left) / total
    ) |>
    filter(total > 0, location %in% c("left", "right"))

  # Wilcoxon–Mann–Whitney AUC; positive class = object on the right.
  auc_mw <- function(score, pos) {
    keep  <- is.finite(score)
    score <- score[keep]; pos <- pos[keep]
    np <- sum(pos); nn <- sum(!pos)
    if (np == 0 || nn == 0) return(NA_real_)
    r <- rank(score)
    (sum(r[pos]) - np * (np + 1) / 2) / (np * nn)
  }

  per_trial |>
    group_by(participant, phase, emo) |>
    group_modify(function(d, key) {
      pos <- d$location == "right"
      auc <- auc_mw(d$score, pos)
      bs  <- replicate(boot, {
        i <- sample(nrow(d), replace = TRUE)
        auc_mw(d$score[i], pos[i])
      })
      tibble(
        n_trials = nrow(d),
        n_left   = sum(!pos),
        n_right  = sum(pos),
        auc      = auc,
        auc_lo   = unname(quantile(bs, 0.025, na.rm = TRUE)),
        auc_hi   = unname(quantile(bs, 0.975, na.rm = TRUE))
      )
    }) |>
    ungroup()
}

# Roll the per-(participant, phase, emo) AUC table up to one row per
# phase × emo: number of participants and the mean/SD AUC across them.
auc_by_condition <- function(auc_tbl) {
  auc_tbl |>
    group_by(phase, emo) |>
    summarise(
      n_participants = n_distinct(participant),
      mean_auc       = mean(auc, na.rm = TRUE),
      sd_auc         = sd(auc,   na.rm = TRUE),
      .groups        = "drop"
    )
}
