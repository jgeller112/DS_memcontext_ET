suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(markdown) # rsconnect misses this through shiny::includeMarkdown()
})

source(file.path("R", "pipeline.R"))

options(shiny.maxRequestSize = 2 * 1024^3)

read_stim_image <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("jpg", "jpeg")) {
    if (!requireNamespace("jpeg", quietly = TRUE)) {
      stop("Install 'jpeg' to view JPG stim images.")
    }
    jpeg::readJPEG(path)
  } else if (ext == "png") {
    if (!requireNamespace("png", quietly = TRUE)) {
      stop("Install 'png' to view PNG stim images.")
    }
    png::readPNG(path)
  } else {
    stop("Unsupported image type: .", ext)
  }
}

flip_y <- function(y) screen_h_px - y

aoi_plot <- function(fix_df, img_array, stim_name, title,
                     color_by = c("AOI", "participant")) {
  color_by <- match.arg(color_by)
  base <- ggplot() +
    annotation_raster(img_array,
      xmin = pic_x_min, xmax = pic_x_max,
      ymin = flip_y(pic_y_max), ymax = flip_y(pic_y_min)
    ) +
    annotate("rect",
      xmin = pic_x_min, xmax = pic_x_mid,
      ymin = flip_y(pic_y_max), ymax = flip_y(pic_y_min),
      fill = NA, color = "#1f77b4", linewidth = 1.2
    ) +
    annotate("rect",
      xmin = pic_x_mid, xmax = pic_x_max,
      ymin = flip_y(pic_y_max), ymax = flip_y(pic_y_min),
      fill = NA, color = "#d62728", linewidth = 1.2
    ) +
    annotate("text",
      x = (pic_x_min + pic_x_mid) / 2, y = flip_y(pic_y_min) + 20,
      label = "Left", color = "#1f77b4", fontface = "bold"
    ) +
    annotate("text",
      x = (pic_x_mid + pic_x_max) / 2, y = flip_y(pic_y_min) + 20,
      label = "Right", color = "#d62728", fontface = "bold"
    )

  if (color_by == "AOI") {
    base <- base +
      geom_point(
        data = fix_df,
        aes(x = x, y = flip_y(y), color = AOI, size = duration),
        alpha = 0.75
      ) +
      scale_color_manual(values = c(
        Left = "#1f77b4", Right = "#d62728",
        Outside = "gray50"
      ))
  } else {
    base <- base +
      geom_point(
        data = fix_df,
        aes(
          x = x, y = flip_y(y),
          color = participant, size = duration
        ),
        alpha = 0.75
      )
  }

  base +
    scale_size_continuous(range = c(2, 8), guide = "none") +
    coord_fixed(xlim = c(0, screen_w_px), ylim = c(0, screen_h_px)) +
    labs(
      title = title, subtitle = stim_name,
      x = "screen x (px)", y = "screen y, flipped (px)"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title    = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(face = "bold", size = 14),
      axis.title    = element_text(face = "bold", size = 16),
      axis.text     = element_text(face = "bold", size = 14),
      legend.title  = element_text(face = "bold", size = 14),
      legend.text   = element_text(size = 13)
    )
}

heatmap_plot <- function(fix_df, img_array, stim_name, title) {
  # stat_density_2d_filled() drops `weight` (kde2d doesn't accept it), so
  # expand each fixation into ~duration/50ms copies. The unweighted KDE
  # then ends up duration-weighted by construction.
  pts <- fix_df |>
    filter(!is.na(x), !is.na(y),
           is.finite(duration), duration > 0) |>
    mutate(n_rep = pmax(1L, as.integer(round(duration / 50)))) |>
    tidyr::uncount(n_rep)

  ggplot() +
    annotation_raster(img_array,
      xmin = pic_x_min, xmax = pic_x_max,
      ymin = flip_y(pic_y_max), ymax = flip_y(pic_y_min)
    ) +
    stat_density_2d_filled(
      data = pts,
      mapping = aes(x = x, y = flip_y(y)),
      contour_var = "ndensity",
      alpha = 0.55,
      breaks = seq(0.1, 1, by = 0.1)
    ) +
    scale_fill_viridis_d(option = "magma", direction = -1, guide = "none") +
    annotate("rect",
      xmin = pic_x_min, xmax = pic_x_mid,
      ymin = flip_y(pic_y_max), ymax = flip_y(pic_y_min),
      fill = NA, color = "#1f77b4", linewidth = 1.2
    ) +
    annotate("rect",
      xmin = pic_x_mid, xmax = pic_x_max,
      ymin = flip_y(pic_y_max), ymax = flip_y(pic_y_min),
      fill = NA, color = "#d62728", linewidth = 1.2
    ) +
    coord_fixed(xlim = c(0, screen_w_px), ylim = c(0, screen_h_px)) +
    labs(
      title = title, subtitle = stim_name,
      x = "screen x (px)", y = "screen y, flipped (px)"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title    = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(face = "bold", size = 14),
      axis.title    = element_text(face = "bold", size = 16),
      axis.text     = element_text(face = "bold", size = 14)
    )
}

okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7", "#000000")

# Build a bar graph of group means, used by both the encoding and recognition
# Summary plots tabs. `df` should already carry whichever categorical columns
# the X / fill / facet specs reference; bars show the mean of `y_var` per group.
barplot_summary <- function(df, y_var, x_var, fill_var, facet_var) {
  y_label <- c(
    mean_total_dwell_time = "Mean total dwell time (ms)",
    mean_n_fixations      = "Mean number of fixations",
    mean_fix_duration     = "Mean fixation duration (ms)",
    n_trials              = "Number of trials"
  )[y_var]

  df <- df |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(c("AOI", "location", "emo", "Condition", "on_object")),
      as.factor
    ))

  use_fill  <- !is.null(fill_var) && fill_var != "_none"
  use_facet <- !is.null(facet_var) && facet_var != "_none"

  group_vars <- unique(c(x_var, if (use_fill) fill_var, if (use_facet) facet_var))
  bars <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(.value = mean(.data[[y_var]], na.rm = TRUE),
                     .groups = "drop")

  mapping <- if (use_fill) {
    ggplot2::aes(x = .data[[x_var]], y = .data[[".value"]],
                 fill = .data[[fill_var]])
  } else {
    ggplot2::aes(x = .data[[x_var]], y = .data[[".value"]])
  }

  p <- ggplot2::ggplot(bars, mapping) +
    ggplot2::geom_col(
      color = "black", linewidth = 0.6, alpha = 0.9,
      position = ggplot2::position_dodge(width = 0.75)
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(face = "bold", size = 14),
      axis.text  = ggplot2::element_text(face = "bold", size = 14)
    ) +
    ggplot2::labs(y = y_label, x = x_var,
                  fill = if (use_fill) fill_var else NULL)

  if (use_fill) {
    p <- p + ggplot2::scale_fill_manual(values = okabe_ito)
  }
  if (use_facet) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet_var]]), ncol = 1)
  }
  p
}

# Object-memory sensitivity (d') by emotion, one dot per participant. d' = 0
# is chance discrimination of old objects from foils; higher = better memory.
object_plot <- function(acc_emo) {
  df <- acc_emo |>
    dplyr::filter(!is.na(emo), is.finite(d_prime)) |>
    dplyr::mutate(emo = as.factor(emo))

  ggplot2::ggplot(df, ggplot2::aes(x = emo, y = d_prime, fill = emo)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    ggplot2::geom_boxplot(
      outlier.shape = NA, alpha = 0.7, color = "black",
      linewidth = 0.6, width = 0.6
    ) +
    ggplot2::geom_point(
      shape = 21, color = "black", size = 2.6, stroke = 0.5, alpha = 0.85,
      position = ggplot2::position_jitter(width = 0.12, height = 0),
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(values = okabe_ito) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(face = "bold", size = 14),
      axis.text  = ggplot2::element_text(face = "bold", size = 14)
    ) +
    ggplot2::labs(
      x = "Emotion", y = "Object memory (d′)",
      title = "Object recognition memory by emotion"
    )
}

stage_uploaded <- function(fileinput) {
  if (is.null(fileinput) || nrow(fileinput) == 0) {
    return(character())
  }
  staged <- file.path(tempdir(), fileinput$name)
  ok <- file.copy(fileinput$datapath, staged, overwrite = TRUE)
  staged[ok]
}

dl_csv <- function(id, label = "Download CSV") {
  downloadButton(id, label, class = "btn-sm")
}

phase_ui <- function(ns_id, phase_label) {
  ns <- NS(ns_id)
  card(
    card_header(paste(phase_label, "uploads")),
    layout_columns(
      col_widths = c(4, 4, 4),
      fileInput(ns("behavioral"),
        "Behavioral CSV(s)",
        multiple = TRUE, accept = ".csv"
      ),
      fileInput(ns("gaze"),
        "Gaze CSV(s) (*_gaze.csv)",
        multiple = TRUE, accept = ".csv"
      ),
      fileInput(ns("msg"),
        "Msg CSV(s) (*_msg.csv)",
        multiple = TRUE, accept = ".csv"
      )
    ),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      actionButton(ns("run_behavioral"), "Run behavioral",
        class = "btn-primary btn-sm"
      ),
      actionButton(ns("run_validation"), "Parse validation",
        class = "btn-sm"
      ),
      actionButton(ns("run_fixations"), "Detect fixations (slow)",
        class = "btn-warning btn-sm"
      ),
      actionButton(ns("run_all"), "Run everything",
        class = "btn-success btn-sm"
      )
    ),
    verbatimTextOutput(ns("status"))
  )
}

phase_outputs_encoding <- function(ns_id) {
  ns <- NS(ns_id)
  navset_card_tab(
    nav_panel(
      "Behavioral",
      dl_csv(ns("dl_behavioral")),
      DTOutput(ns("tbl_behavioral"))
    ),
    nav_panel(
      "Validation (calibration)",
      dl_csv(ns("dl_validation")),
      DTOutput(ns("tbl_validation"))
    ),
    nav_panel(
      "Trial events",
      dl_csv(ns("dl_msg_events")),
      DTOutput(ns("tbl_msg_events"))
    ),
    nav_panel(
      "Missing data — per trial",
      dl_csv(ns("dl_trial_missing")),
      DTOutput(ns("tbl_trial_missing"))
    ),
    nav_panel(
      "Missing data — per subject",
      dl_csv(ns("dl_subject_missing")),
      DTOutput(ns("tbl_subject_missing"))
    ),
    nav_panel(
      "Fixations",
      dl_csv(ns("dl_fixations")),
      DTOutput(ns("tbl_fixations"))
    ),
    nav_panel(
      "AOI dwell (per trial)",
      dl_csv(ns("dl_fix_summary")),
      DTOutput(ns("tbl_fix_summary"))
    ),
    nav_panel(
      "Emo × location × AOI",
      dl_csv(ns("dl_emoloc")),
      DTOutput(ns("tbl_emoloc"))
    ),
    nav_panel(
      "Summary plots",
      card(
        card_header("Bar graphs — encoding summary (group means)"),
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          selectInput(ns("plot_y"), "Y metric",
            choices = c(
              "Total dwell time (ms)" = "mean_total_dwell_time",
              "Number of fixations" = "mean_n_fixations",
              "Mean fixation duration (ms)" = "mean_fix_duration",
              "Number of trials" = "n_trials"
            )
          ),
          selectInput(ns("plot_x"), "X grouping",
            choices = c(
              "AOI"        = "AOI",
              "Location"   = "location",
              "Emotion"    = "emo",
              "Condition"  = "Condition",
              "On-object"  = "on_object"
            ),
            selected = "Condition"
          ),
          selectInput(ns("plot_fill"), "Fill / color",
            choices = c(
              "None"       = "_none",
              "AOI"        = "AOI",
              "Location"   = "location",
              "Emotion"    = "emo",
              "Condition"  = "Condition",
              "On-object"  = "on_object"
            ),
            selected = "AOI"
          ),
          selectInput(ns("plot_facet"), "Facet (wrap)",
            choices = c(
              "None"       = "_none",
              "AOI"        = "AOI",
              "Location"   = "location",
              "Emotion"    = "emo",
              "Condition"  = "Condition",
              "On-object"  = "on_object"
            ),
            selected = "_none"
          )
        ),
        plotOutput(ns("plot_summary"), height = "560px")
      )
    ),
    nav_panel(
      "Fixation viewer",
      card(
        card_header("Overlay fixations on stim image"),
        layout_columns(
          col_widths = c(6, 2, 2, 2),
          fileInput(ns("stim_images"),
            "Upload stim image(s) (jpg/png)",
            multiple = TRUE,
            accept = c(".jpg", ".jpeg", ".png")
          ),
          selectInput(ns("viz_mode"), "Mode",
            choices = c(
              "Single trial — points"       = "single",
              "All participants — points"   = "all",
              "All participants — heatmap"  = "heatmap"
            )
          ),
          selectInput(ns("viz_participant"), "Participant",
            choices = NULL
          ),
          selectInput(ns("viz_pick"), "Trial / stim",
            choices = NULL
          )
        ),
        plotOutput(ns("viz_plot"), height = "820px")
      )
    )
  )
}

phase_outputs_recognition <- function(ns_id) {
  ns <- NS(ns_id)
  tagList(
    card(
      card_header("Background recognition trial scope"),
      radioButtons(ns("scope"), NULL,
        choices = c(
          "Old + correct"             = "old_correct",
          "Old + correct + incorrect" = "old_all",
          "All trials"                = "all"
        ),
        selected = "old_correct", inline = TRUE
      ),
      tags$small(tags$em(
        "Applies to the AOI summary and AOI × Condition rollup tabs ",
        "below (and the Summary plots tab). ", tags$b("Old + correct"),
        " = recognized old scenes; ", tags$b("Old + correct + incorrect"),
        " = all old scenes; ", tags$b("All trials"), " = old + new (foils). ",
        "The raw Fixations table and the Combined panel are independent."
      ))
    ),
    navset_card_tab(
    nav_panel(
      "Behavioral",
      dl_csv(ns("dl_behavioral")),
      DTOutput(ns("tbl_behavioral"))
    ),
    nav_panel(
      "Accuracy (HR / FAR / d′ / c)",
      dl_csv(ns("dl_acc")),
      DTOutput(ns("tbl_acc"))
    ),
    nav_panel(
      "Accuracy by Condition",
      dl_csv(ns("dl_acc_cond")),
      DTOutput(ns("tbl_acc_cond"))
    ),
    nav_panel(
      "Validation (calibration)",
      dl_csv(ns("dl_validation")),
      DTOutput(ns("tbl_validation"))
    ),
    nav_panel(
      "Trial events",
      dl_csv(ns("dl_msg_events")),
      DTOutput(ns("tbl_msg_events"))
    ),
    nav_panel(
      "Missing data — per trial",
      dl_csv(ns("dl_trial_missing")),
      DTOutput(ns("tbl_trial_missing"))
    ),
    nav_panel(
      "Missing data — per subject",
      dl_csv(ns("dl_subject_missing")),
      DTOutput(ns("tbl_subject_missing"))
    ),
    nav_panel(
      "Fixations",
      dl_csv(ns("dl_fixations")),
      DTOutput(ns("tbl_fixations"))
    ),
    nav_panel(
      "AOI summary (old + correct)",
      dl_csv(ns("dl_fix_summary")),
      DTOutput(ns("tbl_fix_summary"))
    ),
    nav_panel(
      "AOI × Condition rollup",
      dl_csv(ns("dl_fix_by_cond")),
      DTOutput(ns("tbl_fix_by_cond"))
    ),
    nav_panel(
      "Summary plots",
      card(
        card_header("Bar graphs — recognition summary (group means)"),
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          selectInput(ns("plot_y"), "Y metric",
            choices = c(
              "Total dwell time (ms)"       = "mean_total_dwell_time",
              "Number of fixations"         = "mean_n_fixations",
              "Mean fixation duration (ms)" = "mean_fix_duration",
              "Number of trials"            = "n_trials"
            )
          ),
          selectInput(ns("plot_x"), "X grouping",
            choices = c(
              "AOI"        = "AOI",
              "Location"   = "location",
              "Emotion"    = "emo",
              "Condition"  = "Condition",
              "On-object"  = "on_object"
            ),
            selected = "Condition"
          ),
          selectInput(ns("plot_fill"), "Fill / color",
            choices = c(
              "None"       = "_none",
              "AOI"        = "AOI",
              "Location"   = "location",
              "Emotion"    = "emo",
              "Condition"  = "Condition",
              "On-object"  = "on_object"
            ),
            selected = "AOI"
          ),
          selectInput(ns("plot_facet"), "Facet (wrap)",
            choices = c(
              "None"       = "_none",
              "AOI"        = "AOI",
              "Location"   = "location",
              "Emotion"    = "emo",
              "Condition"  = "Condition",
              "On-object"  = "on_object"
            ),
            selected = "_none"
          )
        ),
        plotOutput(ns("plot_summary"), height = "560px")
      )
    ),
    nav_panel(
      "Fixation viewer",
      card(
        card_header("Overlay fixations on stim image"),
        layout_columns(
          col_widths = c(6, 2, 2, 2),
          fileInput(ns("stim_images"),
            "Upload stim image(s) (jpg/png)",
            multiple = TRUE,
            accept = c(".jpg", ".jpeg", ".png")
          ),
          selectInput(ns("viz_mode"), "Mode",
            choices = c(
              "Single trial — points"       = "single",
              "All participants — points"   = "all",
              "All participants — heatmap"  = "heatmap"
            )
          ),
          selectInput(ns("viz_participant"), "Participant",
            choices = NULL
          ),
          selectInput(ns("viz_pick"), "Trial / stim",
            choices = NULL
          )
        ),
        plotOutput(ns("viz_plot"), height = "820px")
      )
    )
  )
  )
}

encodingServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(
      behavioral = NULL, validation = NULL, msg_events = NULL,
      fixations = NULL, fix_summary = NULL,
      emoloc = NULL,
      trial_missing = NULL, subject_missing = NULL,
      status = "Upload files, then click a Run button."
    )

    output$status <- renderText(rv$status)

    run_behavioral <- function() {
      paths <- stage_uploaded(input$behavioral)
      req(length(paths) > 0)
      rv$status <- sprintf("Loading %d behavioral file(s)…", length(paths))
      tryCatch(
        {
          rv$behavioral <- map(paths, read_behavioral) |> list_rbind()
          rv$status <- sprintf(
            "Behavioral: %d rows from %d participant(s).",
            nrow(rv$behavioral),
            n_distinct(rv$behavioral$participant)
          )
        },
        error = function(e) {
          rv$status <- paste("Behavioral load failed:", conditionMessage(e))
        }
      )
    }

    run_validation <- function() {
      mpaths <- stage_uploaded(input$msg)
      req(length(mpaths) > 0)
      rv$status <- "Parsing validation + msg events…"
      tryCatch(
        {
          msg_all <- map(mpaths, read_msg) |> list_rbind()
          rv$validation <- parse_validation(msg_all)
          rv$msg_events <- extract_msg_events(msg_all)
          rv$status <- sprintf(
            "Validation: %d blocks; %d trial events.",
            nrow(rv$validation), nrow(rv$msg_events)
          )
        },
        error = function(e) {
          rv$status <- paste("Validation parse failed:", conditionMessage(e))
        }
      )
    }

    run_fixations <- function() {
      gpaths <- stage_uploaded(input$gaze)
      mpaths <- stage_uploaded(input$msg)
      req(length(gpaths) > 0, length(mpaths) > 0)
      withProgress(message = "Detecting fixations (I-VT)…", value = 0, {
        tryCatch(
          {
            incProgress(0.1, detail = "Loading gaze samples")
            gaze <- map(gpaths, read_gaze) |> list_rbind()
            msg <- map(mpaths, read_msg) |> list_rbind()
            msg_events <- extract_msg_events(msg)
            rv$msg_events <- msg_events
            rv$validation <- parse_validation(msg)

            incProgress(0.2, detail = "Assigning samples to trials")
            trial_stim <- build_trial_stim_encoding(msg_events)
            gaze_trial <- assign_gaze_to_trials(gaze, msg_events, trial_stim,
              decompose_stim = TRUE, trial_dur_ms = 5000
            )

            rv$trial_missing   <- trial_missing(gaze_trial)
            rv$subject_missing <- subject_missing(rv$trial_missing)

            incProgress(0.2, detail = "Downsampling to 120 Hz + preprocessing")
            gaze_pp <- preprocess_120hz(gaze_trial)

            incProgress(0.3, detail = "Running I-VT per trial")
            ivt_results <- run_ivt_per_trial(gaze_pp)
            fixations <- ivt_fixations(ivt_results)

            incProgress(0.15, detail = "Merging with behavioral + labeling AOIs")
            if (is.null(rv$behavioral)) {
              bpaths <- stage_uploaded(input$behavioral)
              if (length(bpaths) > 0) {
                rv$behavioral <- map(bpaths, read_behavioral) |> list_rbind()
              } else {
                stop("Upload behavioral CSV(s) first — needed for clean Condition labels.")
              }
            }

            trial_info <- rv$behavioral |>
              left_join(
                distinct(
                  gaze_trial, participant, trial,
                  stim, background, object
                ),
                by = c("participant", "trial")
              ) |>
              # Behavioral Condition is the source of truth for emo / location;
              # the stim-filename parse misfires on the "airport_a_n_r.jpeg"
              # form ("n" / "r" instead of "neg" / "right").
              tidyr::separate_wider_delim(
                Condition,
                delim = "-",
                names = c("emo", "location"),
                cols_remove = FALSE
              )

            fixations <- fixations |>
              left_join(trial_info, by = c("participant", "trial"))

            fix_labeled <- label_aoi(fixations)

            fix_summary <- fix_labeled |>
              filter(AOI != "Outside") |>
              group_by(
                participant, trial, AOI,
                background, object, emo, location
              ) |>
              summarise(
                mean_fix_duration = mean(duration, na.rm = TRUE),
                n_fixations = n(),
                total_dwell_time = sum(duration, na.rm = TRUE),
                .groups = "drop"
              )

            trial_cells <- fix_labeled |>
              distinct(participant, trial, emo, location)

            per_trial_aoi <- fix_labeled |>
              filter(AOI != "Outside") |>
              group_by(participant, trial, emo, location, AOI) |>
              summarise(
                n_fixations = n(),
                total_dwell_time = sum(duration, na.rm = TRUE),
                mean_fix_duration = mean(duration, na.rm = TRUE),
                .groups = "drop"
              )

            per_trial_full <- trial_cells |>
              tidyr::crossing(AOI = c("Left", "Right")) |>
              left_join(per_trial_aoi,
                by = c("participant", "trial", "emo", "location", "AOI")
              ) |>
              mutate(
                n_fixations      = coalesce(n_fixations, 0L),
                total_dwell_time = coalesce(total_dwell_time, 0),
                on_object        = AOI == str_to_title(location)
              )

            emoloc <- per_trial_full |>
              group_by(participant, emo, location, AOI, on_object) |>
              summarise(
                n_trials = n(),
                mean_n_fixations = mean(n_fixations, na.rm = TRUE),
                mean_fix_duration = mean(mean_fix_duration, na.rm = TRUE),
                mean_total_dwell_time = mean(total_dwell_time, na.rm = TRUE),
                .groups = "drop"
              )

            rv$fixations <- fix_labeled
            rv$fix_summary <- fix_summary
            rv$emoloc <- emoloc
            rv$status <- sprintf(
              "Fixations: %d events across %d trials. AOI summaries built.",
              nrow(fix_labeled), n_distinct(paste(
                fix_labeled$participant,
                fix_labeled$trial
              ))
            )
          },
          error = function(e) {
            rv$status <- paste("Fixation pipeline failed:", conditionMessage(e))
          }
        )
      })
    }

    observeEvent(input$run_behavioral, run_behavioral())
    observeEvent(input$run_validation, run_validation())
    observeEvent(input$run_fixations, run_fixations())
    observeEvent(input$run_all, {
      run_behavioral()
      run_validation()
      run_fixations()
    })

    render_dt <- function(tbl) {
      datatable(tbl,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    }
    output$tbl_behavioral <- renderDT(req(rv$behavioral) |> render_dt())
    output$tbl_validation <- renderDT(req(rv$validation) |> render_dt())
    output$tbl_msg_events <- renderDT(req(rv$msg_events) |> render_dt())
    output$tbl_trial_missing   <- renderDT(req(rv$trial_missing)   |> render_dt())
    output$tbl_subject_missing <- renderDT(req(rv$subject_missing) |> render_dt())
    output$tbl_fixations <- renderDT(req(rv$fixations) |> render_dt())
    output$tbl_fix_summary <- renderDT(req(rv$fix_summary) |> render_dt())
    output$tbl_emoloc <- renderDT(req(rv$emoloc) |> render_dt())

    output$plot_summary <- renderPlot({
      req(rv$emoloc)
      df <- rv$emoloc |>
        mutate(
          emo       = dplyr::recode(emo, "neg" = "negative", "neu" = "neutral"),
          Condition = paste(emo, location, sep = "-"),
          on_object = factor(on_object,
                             levels = c(TRUE, FALSE),
                             labels = c("on object", "off object"))
        )
      barplot_summary(df, input$plot_y, input$plot_x,
                      input$plot_fill, input$plot_facet)
    })

    viz_images <- reactive({
      paths <- stage_uploaded(input$stim_images)
      setNames(paths, basename(paths))
    })

    observe({
      req(rv$fixations)
      updateSelectInput(session, "viz_participant",
        choices = sort(unique(rv$fixations$participant))
      )
    })

    observe({
      req(rv$fixations, viz_images())
      mode <- input$viz_mode %||% "single"
      imgs <- names(viz_images())
      if (length(imgs) == 0) {
        updateSelectInput(session, "viz_pick", choices = character(0))
        return()
      }
      if (mode %in% c("all", "heatmap")) {
        stims <- rv$fixations |>
          filter(stim %in% imgs) |>
          distinct(stim) |>
          arrange(stim) |>
          pull(stim)
        updateSelectInput(session, "viz_pick", choices = stims)
      } else {
        req(input$viz_participant)
        trials <- rv$fixations |>
          filter(participant == input$viz_participant, stim %in% imgs) |>
          distinct(trial, stim) |>
          arrange(trial)
        if (nrow(trials) == 0) {
          updateSelectInput(session, "viz_pick", choices = character(0))
          return()
        }
        choices <- setNames(
          as.character(trials$trial),
          paste0("Trial ", trials$trial, " — ", trials$stim)
        )
        updateSelectInput(session, "viz_pick", choices = choices)
      }
    })

    output$viz_plot <- renderPlot({
      req(rv$fixations, input$viz_pick)
      imgs <- viz_images()
      validate(need(
        length(imgs) > 0,
        "Upload at least one stim image to view fixations."
      ))
      mode <- input$viz_mode %||% "single"

      if (mode %in% c("all", "heatmap")) {
        stim_name <- input$viz_pick
        validate(need(
          nzchar(stim_name) && stim_name %in% names(imgs),
          "Pick a stim whose image you've uploaded."
        ))
        fix_df <- rv$fixations |> filter(stim == stim_name)
      } else {
        req(input$viz_participant)
        trial_pick <- suppressWarnings(as.integer(input$viz_pick))
        validate(need(!is.na(trial_pick), "Pick a trial."))
        fix_df <- rv$fixations |>
          filter(participant == input$viz_participant, trial == trial_pick)
        validate(need(nrow(fix_df) > 0, "No fixations for this trial."))
        stim_name <- fix_df$stim[1]
        validate(need(
          stim_name %in% names(imgs),
          paste0("No uploaded image matches '", stim_name, "'.")
        ))
      }

      validate(need(nrow(fix_df) > 0, "No fixations for this selection."))
      img_path <- unname(imgs[stim_name])
      validate(need(
        length(img_path) == 1 && !is.na(img_path) && file.exists(img_path),
        "Image file not found on disk."
      ))
      img_array <- read_stim_image(img_path)

      if (mode == "heatmap") {
        validate(need(
          nrow(fix_df) >= 5,
          "Need at least 5 fixations to estimate a heatmap."
        ))
        heatmap_plot(fix_df, img_array, stim_name,
          title = "Fixation heatmap (all participants)"
        )
      } else if (mode == "all") {
        aoi_plot(fix_df, img_array, stim_name,
          title = "Fixations across participants",
          color_by = "participant"
        )
      } else {
        aoi_plot(fix_df, img_array, stim_name,
          title = paste0(
            input$viz_participant,
            " — trial ", input$viz_pick
          ),
          color_by = "AOI"
        )
      }
    })

    make_dl <- function(tbl_react, fname) {
      downloadHandler(
        filename = function() fname,
        content  = function(file) write_csv(tbl_react(), file)
      )
    }
    output$dl_behavioral <- make_dl(reactive(req(rv$behavioral)), "encoding_behavioral.csv")
    output$dl_validation <- make_dl(reactive(req(rv$validation)), "encoding_validation.csv")
    output$dl_msg_events <- make_dl(reactive(req(rv$msg_events)), "encoding_msg_events.csv")
    output$dl_trial_missing   <- make_dl(reactive(req(rv$trial_missing)),   "encoding_missing_per_trial.csv")
    output$dl_subject_missing <- make_dl(reactive(req(rv$subject_missing)), "encoding_missing_per_subject.csv")
    output$dl_fixations <- make_dl(reactive(req(rv$fixations)), "encoding_fixations.csv")
    output$dl_fix_summary <- make_dl(reactive(req(rv$fix_summary)), "encoding_fix_aoi_summary.csv")
    output$dl_emoloc <- make_dl(reactive(req(rv$emoloc)), "encoding_emo_location_aoi.csv")

    # Exposed for combinedServer — the Combined tab consumes these.
    list(
      fixations  = reactive(rv$fixations),
      behavioral = reactive(rv$behavioral)
    )
  })
}

recognitionServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(
      behavioral = NULL, acc = NULL, acc_cond = NULL,
      validation = NULL, msg_events = NULL,
      fixations = NULL,
      trial_missing = NULL, subject_missing = NULL,
      status = "Upload files, then click a Run button."
    )
    output$status <- renderText(rv$status)

    run_behavioral <- function() {
      paths <- stage_uploaded(input$behavioral)
      req(length(paths) > 0)
      rv$status <- sprintf("Loading %d recognition file(s)…", length(paths))
      tryCatch(
        {
          rv$behavioral <- map(paths, read_recognition_behavioral) |> list_rbind()
          rv$acc <- recognition_accuracy(rv$behavioral)
          rv$acc_cond <- recognition_accuracy(
            rv$behavioral,
            groupvars = c("participant", "Condition")
          )
          rv$status <- sprintf(
            "Background recognition: %d trials, %d participants.",
            nrow(rv$behavioral),
            n_distinct(rv$behavioral$participant)
          )
        },
        error = function(e) {
          rv$status <- paste("Behavioral load failed:", conditionMessage(e))
        }
      )
    }

    run_validation <- function() {
      mpaths <- stage_uploaded(input$msg)
      req(length(mpaths) > 0)
      tryCatch(
        {
          msg_all <- map(mpaths, read_msg) |> list_rbind()
          rv$validation <- parse_validation(msg_all)
          rv$msg_events <- extract_msg_events(msg_all)
          rv$status <- sprintf(
            "Validation: %d blocks; %d trial events.",
            nrow(rv$validation), nrow(rv$msg_events)
          )
        },
        error = function(e) {
          rv$status <- paste("Validation parse failed:", conditionMessage(e))
        }
      )
    }

    run_fixations <- function() {
      gpaths <- stage_uploaded(input$gaze)
      mpaths <- stage_uploaded(input$msg)
      req(length(gpaths) > 0, length(mpaths) > 0)
      withProgress(message = "Detecting fixations (I-VT)…", value = 0, {
        tryCatch(
          {
            incProgress(0.1, detail = "Loading gaze samples")
            gaze <- map(gpaths, read_gaze) |> list_rbind()
            msg <- map(mpaths, read_msg) |> list_rbind()
            msg_events <- extract_msg_events(msg)
            rv$msg_events <- msg_events
            rv$validation <- parse_validation(msg)

            incProgress(0.2, detail = "Assigning samples to trials")
            trial_stim <- build_trial_stim_recognition(msg_events)
            gaze_trial <- assign_gaze_to_trials(gaze, msg_events, trial_stim,
              decompose_stim = FALSE, trial_dur_ms = 5000
            )

            rv$trial_missing   <- trial_missing(gaze_trial)
            rv$subject_missing <- subject_missing(rv$trial_missing)

            incProgress(0.2, detail = "Downsampling to 120 Hz + preprocessing")
            gaze_pp <- preprocess_120hz(gaze_trial)

            incProgress(0.3, detail = "Running I-VT per trial")
            ivt_results <- run_ivt_per_trial(gaze_pp)
            fixations <- ivt_fixations(ivt_results)

            incProgress(0.15, detail = "Merging with behavioral + labeling AOIs")
            if (!is.null(rv$behavioral)) {
              trial_info <- rv$behavioral |>
                left_join(distinct(gaze_trial, participant, trial, stim),
                  by = c("participant", "trial")
                )
              fixations <- fixations |>
                left_join(trial_info, by = c("participant", "trial"))
            }

            fix_labeled <- label_aoi(fixations)

            rv$fixations <- fix_labeled
            rv$status <- sprintf(
              "Fixations: %d events across %d trials. Summary tables follow the scope toggle.",
              nrow(fix_labeled), n_distinct(paste(
                fix_labeled$participant,
                fix_labeled$trial
              ))
            )
          },
          error = function(e) {
            rv$status <- paste("Fixation pipeline failed:", conditionMessage(e))
          }
        )
      })
    }

    observeEvent(input$run_behavioral, run_behavioral())
    observeEvent(input$run_validation, run_validation())
    observeEvent(input$run_fixations, run_fixations())
    observeEvent(input$run_all, {
      run_behavioral()
      run_validation()
      run_fixations()
    })

    # Scope-aware summary reactives. `input$scope` is the radio in
    # phase_outputs_recognition: "old_correct" = old + correctly recognized,
    # "old_all" = all old scenes regardless of accuracy, "all" = every
    # fixation (old + new). NA status/accuracy (e.g. fixations with no merged
    # behavioral) are kept in the two old-scoped views. Outside-AOI fixations
    # always dropped — the summary is about AOI dwell.
    fix_summary_react <- reactive({
      fl <- rv$fixations
      req(fl)
      scope <- input$scope %||% "old_correct"
      if (scope == "old_correct") {
        fl <- fl |>
          filter(stimulus_status == "old" | is.na(stimulus_status)) |>
          filter(is.na(accuracy) | accuracy == 1)
      } else if (scope == "old_all") {
        fl <- fl |>
          filter(stimulus_status == "old" | is.na(stimulus_status))
      }
      fl |>
        filter(AOI != "Outside") |>
        group_by(participant, trial, Background, Condition, AOI) |>
        summarise(
          n_fixations       = n(),
          mean_fix_duration = mean(duration, na.rm = TRUE),
          total_dwell_time  = sum(duration,  na.rm = TRUE),
          .groups           = "drop"
        )
    })

    fix_by_cond_react <- reactive({
      fix_summary_react() |>
        group_by(Condition, AOI) |>
        summarise(
          n_trials              = n(),
          mean_n_fixations      = mean(n_fixations,       na.rm = TRUE),
          mean_fix_duration     = mean(mean_fix_duration, na.rm = TRUE),
          mean_total_dwell_time = mean(total_dwell_time,  na.rm = TRUE),
          .groups               = "drop"
        )
    })

    render_dt <- function(tbl) {
      datatable(tbl,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    }
    output$tbl_behavioral <- renderDT(req(rv$behavioral) |> render_dt())
    output$tbl_acc <- renderDT(req(rv$acc) |> render_dt())
    output$tbl_acc_cond <- renderDT(req(rv$acc_cond) |> render_dt())
    output$tbl_validation <- renderDT(req(rv$validation) |> render_dt())
    output$tbl_msg_events <- renderDT(req(rv$msg_events) |> render_dt())
    output$tbl_trial_missing   <- renderDT(req(rv$trial_missing)   |> render_dt())
    output$tbl_subject_missing <- renderDT(req(rv$subject_missing) |> render_dt())
    output$tbl_fixations <- renderDT(req(rv$fixations) |> render_dt())
    output$tbl_fix_summary <- renderDT(fix_summary_react()  |> render_dt())
    output$tbl_fix_by_cond <- renderDT(fix_by_cond_react()  |> render_dt())

    # Recognition summary plots: roll the per-trial fix_summary up to one
    # row per (participant, Condition, AOI), then derive emo / location /
    # on_object from Condition so the same X / fill / facet choices work
    # as on the encoding tab.
    output$plot_summary <- renderPlot({
      df <- fix_summary_react() |>
        group_by(participant, Condition, AOI) |>
        summarise(
          mean_total_dwell_time = mean(total_dwell_time,  na.rm = TRUE),
          mean_n_fixations      = mean(n_fixations,       na.rm = TRUE),
          mean_fix_duration     = mean(mean_fix_duration, na.rm = TRUE),
          n_trials              = dplyr::n(),
          .groups = "drop"
        ) |>
        mutate(
          emo       = dplyr::recode(
            stringr::str_extract(Condition, "^[^-]+"),
            "neg" = "negative", "neu" = "neutral"
          ),
          location  = stringr::str_extract(Condition, "(?<=-).+$"),
          on_object = factor(AOI == stringr::str_to_title(location),
                             levels = c(TRUE, FALSE),
                             labels = c("on object", "off object")),
          Condition = paste(emo, location, sep = "-")
        )
      barplot_summary(df, input$plot_y, input$plot_x,
                      input$plot_fill, input$plot_facet)
    })

    viz_images <- reactive({
      paths <- stage_uploaded(input$stim_images)
      setNames(paths, basename(paths))
    })

    observe({
      req(rv$fixations)
      updateSelectInput(session, "viz_participant",
        choices = sort(unique(rv$fixations$participant))
      )
    })

    # In recognition, the same background is shown to participants who saw
    # it encoded under different conditions (counterbalanced across lists),
    # so collapsing across participants by Background alone would mix
    # conditions. The all/heatmap dropdown is keyed on (stim × Condition)
    # and the plot filters on both. Dropdown choices come from the
    # fixations table directly — they are not gated on whether the user
    # has uploaded matching images yet, so the available stim × condition
    # combinations are visible up front; image availability is validated
    # at plot time.
    pick_sep <- " || "
    observe({
      req(rv$fixations)
      mode <- input$viz_mode %||% "single"
      if (mode %in% c("all", "heatmap")) {
        combos <- rv$fixations |>
          filter(!is.na(Condition)) |>
          distinct(stim, Condition) |>
          arrange(stim, Condition)
        if (nrow(combos) == 0) {
          updateSelectInput(session, "viz_pick", choices = character(0))
          return()
        }
        choices <- setNames(
          paste(combos$stim, combos$Condition, sep = pick_sep),
          paste0(combos$stim, " — ", combos$Condition)
        )
        updateSelectInput(session, "viz_pick", choices = choices)
      } else {
        req(input$viz_participant)
        trials <- rv$fixations |>
          filter(participant == input$viz_participant) |>
          distinct(trial, stim, Condition) |>
          arrange(trial)
        if (nrow(trials) == 0) {
          updateSelectInput(session, "viz_pick", choices = character(0))
          return()
        }
        choices <- setNames(
          as.character(trials$trial),
          paste0("Trial ", trials$trial, " — ", trials$stim,
                 " — ", trials$Condition)
        )
        updateSelectInput(session, "viz_pick", choices = choices)
      }
    })

    output$viz_plot <- renderPlot({
      req(rv$fixations, input$viz_pick)
      imgs <- viz_images()
      validate(need(
        length(imgs) > 0,
        "Upload at least one stim image to view fixations."
      ))
      mode <- input$viz_mode %||% "single"
      cond <- NA_character_

      if (mode %in% c("all", "heatmap")) {
        parts <- strsplit(input$viz_pick, pick_sep, fixed = TRUE)[[1]]
        validate(need(length(parts) == 2,
          "Pick a stim × condition combination."))
        stim_name <- parts[1]; cond <- parts[2]
        validate(need(
          nzchar(stim_name) && stim_name %in% names(imgs),
          "Pick a stim whose image you've uploaded."
        ))
        fix_df <- rv$fixations |>
          filter(stim == stim_name, Condition == cond)
      } else {
        req(input$viz_participant)
        trial_pick <- suppressWarnings(as.integer(input$viz_pick))
        validate(need(!is.na(trial_pick), "Pick a trial."))
        fix_df <- rv$fixations |>
          filter(participant == input$viz_participant, trial == trial_pick)
        validate(need(nrow(fix_df) > 0, "No fixations for this trial."))
        stim_name <- fix_df$stim[1]
        validate(need(
          stim_name %in% names(imgs),
          paste0("No uploaded image matches '", stim_name, "'.")
        ))
      }

      validate(need(nrow(fix_df) > 0, "No fixations for this selection."))
      img_path <- unname(imgs[stim_name])
      validate(need(
        length(img_path) == 1 && !is.na(img_path) && file.exists(img_path),
        "Image file not found on disk."
      ))
      img_array <- read_stim_image(img_path)

      if (mode == "heatmap") {
        validate(need(
          nrow(fix_df) >= 5,
          "Need at least 5 fixations to estimate a heatmap."
        ))
        heatmap_plot(fix_df, img_array, stim_name,
          title = paste0("Fixation heatmap — ", cond,
                         " (all participants)")
        )
      } else if (mode == "all") {
        aoi_plot(fix_df, img_array, stim_name,
          title = paste0("Fixations across participants — ", cond),
          color_by = "participant"
        )
      } else {
        aoi_plot(fix_df, img_array, stim_name,
          title = paste0(
            input$viz_participant,
            " — trial ", input$viz_pick
          ),
          color_by = "AOI"
        )
      }
    })

    make_dl <- function(tbl_react, fname) {
      downloadHandler(
        filename = function() fname,
        content  = function(file) write_csv(tbl_react(), file)
      )
    }
    output$dl_behavioral <- make_dl(reactive(req(rv$behavioral)), "recognition_behavioral.csv")
    output$dl_acc <- make_dl(reactive(req(rv$acc)), "recognition_accuracy.csv")
    output$dl_acc_cond <- make_dl(reactive(req(rv$acc_cond)), "recognition_accuracy_by_condition.csv")
    output$dl_validation <- make_dl(reactive(req(rv$validation)), "recognition_validation.csv")
    output$dl_msg_events <- make_dl(reactive(req(rv$msg_events)), "recognition_msg_events.csv")
    output$dl_trial_missing   <- make_dl(reactive(req(rv$trial_missing)),   "recognition_missing_per_trial.csv")
    output$dl_subject_missing <- make_dl(reactive(req(rv$subject_missing)), "recognition_missing_per_subject.csv")
    output$dl_fixations <- make_dl(reactive(req(rv$fixations)), "recognition_fixations.csv")
    output$dl_fix_summary <- make_dl(fix_summary_react,  "recognition_fix_summary.csv")
    output$dl_fix_by_cond <- make_dl(fix_by_cond_react,  "recognition_fix_by_condition.csv")

    # Exposed for combinedServer — the Combined tab consumes these.
    list(
      fixations  = reactive(rv$fixations),
      behavioral = reactive(rv$behavioral)
    )
  })
}

combined_outputs_ui <- function(ns_id) {
  ns <- NS(ns_id)
  card(
    card_header("Combined encoding + recognition"),
    tags$p(
      "Restricted to Backgrounds that appear in ",
      tags$b("both"), " phases per participant. Background-recognition scope ",
      "is controlled by the radio below."
    ),
    tags$p(
      "Run the encoding ", tags$b("Detect fixations"), " step and the ",
      "recognition ", tags$b("Detect fixations"), " step first. Tables ",
      "below populate automatically."
    ),
    radioButtons(ns("rec_scope"), "Background-recognition scope",
      choices = c(
        "Old + correct (hits only)" = "old_correct",
        "All trials"                = "all"
      ),
      selected = "old_correct", inline = TRUE
    ),
    navset_card_tab(
      nav_panel(
        "Paired fixations (long)",
        dl_csv(ns("dl_long")),
        DTOutput(ns("tbl_long"))
      ),
      nav_panel(
        "Per (participant × Background × AOI)",
        dl_csv(ns("dl_per_bg")),
        DTOutput(ns("tbl_per_bg"))
      ),
      nav_panel(
        "Per (participant × Condition × AOI)",
        dl_csv(ns("dl_per_cond")),
        DTOutput(ns("tbl_per_cond"))
      )
    )
  )
}

combinedServer <- function(id, enc_state, rec_state) {
  moduleServer(id, function(input, output, session) {
    fixations_long <- reactive({
      enc <- enc_state$fixations()
      rec <- rec_state$fixations()
      validate(
        need(!is.null(enc), "Run encoding fixation detection first."),
        need(!is.null(rec), "Run recognition fixation detection first.")
      )
      build_fixations_long(enc, rec,
        recognition_scope = input$rec_scope %||% "old_correct"
      )
    })

    per_bg <- reactive(summarise_per_background(fixations_long()))
    per_cd <- reactive(summarise_per_condition(fixations_long()))

    render_dt <- function(tbl) {
      datatable(tbl,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    }
    output$tbl_long    <- renderDT(req(fixations_long()) |> render_dt())
    output$tbl_per_bg  <- renderDT(req(per_bg())         |> render_dt())
    output$tbl_per_cond <- renderDT(req(per_cd())        |> render_dt())

    make_dl <- function(tbl_react, fname) {
      downloadHandler(
        filename = function() fname,
        content  = function(file) write_csv(tbl_react(), file)
      )
    }
    output$dl_long    <- make_dl(fixations_long, "combined_fixations_long.csv")
    output$dl_per_bg  <- make_dl(per_bg,         "combined_per_background.csv")
    output$dl_per_cond <- make_dl(per_cd,        "combined_per_condition_aoi.csv")
  })
}

# ---- Object memory (object old/new recognition) ----------------------------
# Behavioral-only tab: pulls the object-recognition block out of the same
# recognition CSV and computes signal-detection memory (accuracy, hit/FA,
# d', criterion) overall and by emotion.
object_ui <- function(ns_id) {
  ns <- NS(ns_id)
  card(
    card_header("Object memory uploads"),
    layout_columns(
      col_widths = c(8, 4),
      fileInput(ns("behavioral"),
        "Recognition behavioral CSV(s)",
        multiple = TRUE, accept = ".csv"
      ),
      div(
        class = "d-flex align-items-end h-100",
        actionButton(ns("run"), "Run object memory",
          class = "btn-primary btn-sm"
        )
      )
    ),
    verbatimTextOutput(ns("status"))
  )
}

object_outputs_ui <- function(ns_id) {
  ns <- NS(ns_id)
  tagList(
    card(
      card_header("Object recognition trial scope"),
      radioButtons(ns("scope"), NULL,
        choices = c(
          "Old + correct"             = "old_correct",
          "Old + correct + incorrect" = "old_all",
          "All trials"                = "all"
        ),
        selected = "all", inline = TRUE
      ),
      tags$small(tags$em(
        "Applies to the ", tags$b("Behavioral"), " (per-trial) table below. ",
        "The accuracy / d′ tables always use all trials (old + new), since d′ ",
        "needs both studied items and foils."
      ))
    ),
    card(
      card_header("Object recognition memory"),
      tags$p(
        "Object old/new recognition, extracted from the ", tags$b("object block"),
        " of the same recognition behavioral file (separate from the background ",
        "task). Old objects were seen at encoding; ", tags$b("foils"),
        " are new. Per participant: accuracy, hit / false-alarm rates, ",
        tags$b("d′"), " (memory sensitivity) and criterion ", tags$b("c"),
        ", overall, by Condition, and split by emotion (neg / neu)."
      ),
      navset_card_tab(
        nav_panel(
          "Behavioral",
          dl_csv(ns("dl_trials")),
          DTOutput(ns("tbl_trials"))
        ),
        nav_panel(
          "Accuracy (HR / FAR / d′ / c)",
          dl_csv(ns("dl_acc")),
          DTOutput(ns("tbl_acc"))
        ),
        nav_panel(
          "Accuracy by Condition",
          dl_csv(ns("dl_acc_cond")),
          DTOutput(ns("tbl_acc_cond"))
        ),
        nav_panel(
          "Accuracy × emotion",
          dl_csv(ns("dl_acc_emo")),
          DTOutput(ns("tbl_acc_emo"))
        ),
        nav_panel(
          "Summary plot",
          plotOutput(ns("plot"), height = "460px")
        )
      )
    )
  )
}

objectServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(
      trials = NULL, acc = NULL, acc_cond = NULL, acc_emo = NULL,
      status = "Upload recognition behavioral CSV(s), then click Run object memory."
    )
    output$status <- renderText(rv$status)

    observeEvent(input$run, {
      paths <- stage_uploaded(input$behavioral)
      req(length(paths) > 0)
      rv$status <- sprintf("Reading %d file(s)…", length(paths))
      tryCatch(
        {
          trials <- map(paths, read_object_recognition) |> list_rbind()
          rv$trials   <- trials
          rv$acc      <- recognition_accuracy(trials)
          rv$acc_cond <- recognition_accuracy(trials,
                                             groupvars = c("participant", "Condition"))
          rv$acc_emo  <- recognition_accuracy(trials,
                                             groupvars = c("participant", "emo"))
          rv$status <- sprintf(
            "Object memory: %d trials from %d participant(s). Mean d′ = %.2f.",
            nrow(trials), n_distinct(trials$participant),
            mean(rv$acc$d_prime, na.rm = TRUE)
          )
        },
        error = function(e) {
          rv$status <- paste("Object memory failed:", conditionMessage(e))
        }
      )
    })

    # Per-trial table honors the scope radio. Accuracy / d′ tables always use
    # all trials (d′ needs old + new), mirroring how Background Recognition's
    # accuracy is independent of its fixation-scope radio.
    trials_scoped <- reactive({
      t <- req(rv$trials)
      switch(input$scope %||% "all",
        old_correct = dplyr::filter(t, stimulus_status == "old", accuracy == 1),
        old_all     = dplyr::filter(t, stimulus_status == "old"),
        t
      )
    })

    render_dt <- function(tbl) {
      datatable(tbl, options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE)
    }
    output$tbl_trials   <- renderDT(trials_scoped()    |> render_dt())
    output$tbl_acc      <- renderDT(req(rv$acc)        |> render_dt())
    output$tbl_acc_cond <- renderDT(req(rv$acc_cond)   |> render_dt())
    output$tbl_acc_emo  <- renderDT(req(rv$acc_emo)    |> render_dt())
    output$plot         <- renderPlot(object_plot(req(rv$acc_emo)))

    make_dl <- function(tbl_react, fname) {
      downloadHandler(
        filename = function() fname,
        content  = function(file) write_csv(tbl_react(), file)
      )
    }
    output$dl_trials   <- make_dl(trials_scoped,                "object_recognition_trials.csv")
    output$dl_acc      <- make_dl(reactive(req(rv$acc)),        "object_recognition_accuracy.csv")
    output$dl_acc_cond <- make_dl(reactive(req(rv$acc_cond)),   "object_recognition_accuracy_by_condition.csv")
    output$dl_acc_emo  <- make_dl(reactive(req(rv$acc_emo)),    "object_recognition_accuracy_by_emotion.csv")

    # Exposed for recogComboServer — the Combined recognition tab consumes this.
    list(trials = reactive(rv$trials))
  })
}

# ---- Combined recognition (background × object memory) ---------------------
# Behavioral-only tab: pairs each studied item's background-recognition and
# object-recognition outcomes, linked through the encoding scene↔object
# pairing, so you can see whether the scene and its object were each
# recognized. No eye-tracking.
recog_combo_ui <- function(ns_id) {
  ns <- NS(ns_id)
  card(
    card_header("Combined recognition — background × object memory"),
    tags$p(
      "Joins ", tags$b("background recognition"), " and ",
      tags$b("object recognition"), " at the item level for studied ",
      tags$b("(old)"), " items. The background block only records the scene ",
      "and the object block only the object, so the two are linked through ",
      "the ", tags$b("encoding"), " scene↔object pairing — letting you see ",
      "whether each studied scene and its object were later recognized. ",
      "Behavioral only — no eye-tracking. Foils are excluded (the two blocks' ",
      "foils are distinct items with no pairing)."
    ),
    tags$p(
      "Run ", tags$b("Encoding → Run behavioral"), ", ",
      tags$b("Background Recognition → Run behavioral"), ", and ",
      tags$b("Object Memory → Run object memory"),
      " first; the tables below populate automatically."
    ),
    layout_columns(
      col_widths = c(6, 6),
      radioButtons(ns("bg_scope"), "Background scope",
        choices = c(
          "Old + correct"             = "old_correct",
          "Old + correct + incorrect" = "all"
        ),
        selected = "all", inline = TRUE
      ),
      radioButtons(ns("obj_scope"), "Object scope",
        choices = c(
          "Old + correct"             = "old_correct",
          "Old + correct + incorrect" = "all"
        ),
        selected = "all", inline = TRUE
      )
    ),
    tags$small(tags$em(
      "Each scope filters the paired studied items below. ",
      tags$b("Old + correct"), " keeps only items that block recognized ",
      "correctly (a hit); ", tags$b("Old + correct + incorrect"),
      " keeps every studied item regardless of accuracy."
    )),
    navset_card_tab(
      nav_panel(
        "Per item (background × object)",
        dl_csv(ns("dl_items")),
        DTOutput(ns("tbl_items"))
      ),
      nav_panel(
        "Joint memory summary",
        dl_csv(ns("dl_summary")),
        DTOutput(ns("tbl_summary"))
      )
    )
  )
}

recogComboServer <- function(id, enc_state, rec_state, obj_state) {
  moduleServer(id, function(input, output, session) {
    combined_full <- reactive({
      enc <- enc_state$behavioral()
      bg  <- rec_state$behavioral()
      obj <- obj_state$trials()
      validate(
        need(!is.null(enc),
          "Run Encoding → Run behavioral first (needed for the scene↔object pairing)."),
        need(!is.null(bg),
          "Run Background Recognition → Run behavioral first."),
        need(!is.null(obj),
          "Run Object Memory → Run object memory first.")
      )
      combine_recognition(enc, bg, obj)
    })

    # Apply the per-block scope radios: "old_correct" keeps that block's hits
    # only (accuracy == 1); "all" keeps every studied item (correct +
    # incorrect). The join is already old-only, so "all" = all paired items.
    combined <- reactive({
      d <- combined_full()
      if ((input$bg_scope  %||% "all") == "old_correct") d <- dplyr::filter(d, bg_accuracy == 1)
      if ((input$obj_scope %||% "all") == "old_correct") d <- dplyr::filter(d, obj_accuracy == 1)
      d
    })
    summary_tbl <- reactive(recognition_joint_summary(combined()))

    render_dt <- function(tbl) {
      datatable(tbl,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    }
    output$tbl_items   <- renderDT(req(combined())    |> render_dt())
    output$tbl_summary <- renderDT(req(summary_tbl()) |> render_dt())

    make_dl <- function(tbl_react, fname) {
      downloadHandler(
        filename = function() fname,
        content  = function(file) write_csv(tbl_react(), file)
      )
    }
    output$dl_items   <- make_dl(combined,    "recognition_combined_items.csv")
    output$dl_summary <- make_dl(summary_tbl, "recognition_combined_summary.csv")
  })
}

ui <- page_navbar(
  title = "DS-Mem-Emo",
  # Boston College colors: maroon (#8C2232) navbar + primary controls, gold
  # (#C2A14D) accents. Content stays on a near-white background so tables and
  # plots remain readable; bslib auto-picks light navbar text on the maroon bg.
  theme = bs_theme(
    version     = 5,
    bg          = "#FFFFFF",
    fg          = "#23282B",
    primary     = "#8C2232",
    secondary   = "#C2A14D",
    "navbar-bg" = "#8C2232"
  ),
  # Each tab is a tall stack of cards (uploads, plots, viewers, tables), so
  # let the page scroll normally. The default (fillable = TRUE) makes the
  # active panel a fixed-height fill container, which compresses the cards
  # until form controls — e.g. the file uploads — overflow on top of the
  # run buttons. This was visible both locally and on Posit Connect Cloud.
  fillable = FALSE,
  nav_panel(
    "Encoding",
    phase_ui("enc", "Encoding"),
    phase_outputs_encoding("enc")
  ),
  nav_panel(
    "Background Recognition",
    phase_ui("rec", "Background Recognition"),
    phase_outputs_recognition("rec")
  ),
  nav_panel(
    "Object Memory",
    object_ui("obj"),
    object_outputs_ui("obj")
  ),
  nav_panel(
    "Recognition (combined)",
    recog_combo_ui("recog_combo")
  ),
  nav_panel(
    "Combined (ET data)",
    combined_outputs_ui("combined")
  ),
  nav_panel(
    "About",
    card(
      card_header("How to use"),
      tags$ol(
        tags$li("Pick the Encoding or Background Recognition tab."),
        tags$li("Upload the behavioral CSV(s) (PsychoPy output)."),
        tags$li(
          "Upload the matching ", tags$code("*_gaze.csv"),
          " and ", tags$code("*_msg.csv"),
          " from Tobii (one pair per participant)."
        ),
        tags$li(
          "Click ", tags$b("Run behavioral"), " for fast tables, ",
          tags$b("Parse validation"), " for calibration QC, ",
          tags$b("Detect fixations"), " for the full I-VT pipeline, or ",
          tags$b("Run everything"), " to chain them."
        ),
        tags$li(
          "For ", tags$b("Object Memory"), ", upload the recognition ",
          "behavioral CSV(s) — d′ / accuracy for the object old/new block ",
          "are computed from the same file (no eye-tracking needed)."
        ),
        tags$li(
          "The ", tags$b("Recognition (combined)"), " tab pairs each studied ",
          "item's background- and object-recognition outcomes (behavioral ",
          "only), linked via the encoding scene↔object pairing — run ",
          tags$b("Encoding"), ", ", tags$b("Background Recognition"), ", and ",
          tags$b("Object Memory"), " behavioral first and it fills in automatically."
        )
      ),
      tags$p(
        "File naming: participant ID is parsed from the filename as ",
        tags$code("^DS\\d+[_-]\\d+"),
        " (e.g. ", tags$code("DS24_2056"), "). Gaze and msg uploads are matched on participant ID."
      ),
      tags$p(
        "Pipeline assumes a 1920×1080 screen with a 700×550 picture box (x: 610–1310, y: 265–815) split into Left/Right AOIs at x = 960. I-VT params match Tobii Pro Lab defaults (30°/s, 60 ms min fixation). Update ",
        tags$code("R/pipeline.R"), " if your geometry differs."
      ),
      tags$p(
        "Requires: ", tags$code("kollaR"),
        " for fixation detection (",
        tags$code("install.packages('kollaR')"), ")."
      )
    ),
    card(
      card_header("Data dictionary"),
      includeMarkdown("DATA_DICTIONARY.md")
    )
  )
)

server <- function(input, output, session) {
  enc_state <- encodingServer("enc")
  rec_state <- recognitionServer("rec")
  obj_state <- objectServer("obj")
  combinedServer("combined", enc_state, rec_state)
  recogComboServer("recog_combo", enc_state, rec_state, obj_state)
}

shinyApp(ui, server)
