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
# appear in *both* phases per participant (so every Background here is
# paired). Recognition is pre-filtered to old + correct so the scope
# matches the eyesim reinstatement pipeline.
build_fixations_long <- function(enc_fix, rec_fix) {
  keep_cols <- c("participant", "Background", "Condition", "List",
                 "AOI", "x", "y", "duration", "onset")
  enc_long <- enc_fix |>
    mutate(phase = "encoding") |>
    select(all_of(keep_cols), phase)
  rec_long <- rec_fix |>
    filter(stimulus_status == "old", accuracy == 1) |>
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
