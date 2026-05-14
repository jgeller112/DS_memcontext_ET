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
    theme_minimal()
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
    theme_minimal()
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
        card_header("Box plots — encoding summary (one dot per participant)"),
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
        plotOutput(ns("viz_plot"), height = "640px")
      )
    )
  )
}

phase_outputs_recognition <- function(ns_id) {
  ns <- NS(ns_id)
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
      "Picture-duration QC",
      dl_csv(ns("dl_duration")),
      DTOutput(ns("tbl_duration"))
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
        plotOutput(ns("viz_plot"), height = "640px")
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
              decompose_stim = TRUE
            )

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
    output$tbl_fixations <- renderDT(req(rv$fixations) |> render_dt())
    output$tbl_fix_summary <- renderDT(req(rv$fix_summary) |> render_dt())
    output$tbl_emoloc <- renderDT(req(rv$emoloc) |> render_dt())

    output$plot_summary <- renderPlot({
      req(rv$emoloc)
      y_var <- input$plot_y
      x_var <- input$plot_x
      fill_var <- input$plot_fill
      facet_var <- input$plot_facet

      y_label <- c(
        mean_total_dwell_time = "Mean total dwell time (ms)",
        mean_n_fixations      = "Mean number of fixations",
        mean_fix_duration     = "Mean fixation duration (ms)",
        n_trials              = "Number of trials"
      )[y_var]

      okabe_ito <- c(
        "#E69F00", "#56B4E9", "#009E73", "#F0E442",
        "#0072B2", "#D55E00", "#CC79A7", "#000000"
      )

      df <- rv$emoloc |>
        mutate(
          emo = dplyr::recode(emo, "neg" = "negative", "neu" = "neutral"),
          Condition = paste(emo, location, sep = "-"),
          on_object = factor(on_object,
            levels = c(TRUE, FALSE),
            labels = c("on object", "off object")
          )
        ) |>
        mutate(across(any_of(c(
          "AOI", "location", "emo",
          "Condition", "on_object"
        )), as.factor))

      use_fill <- fill_var != "_none"
      mapping <- if (use_fill) {
        aes(
          x = .data[[x_var]], y = .data[[y_var]],
          fill = .data[[fill_var]]
        )
      } else {
        aes(x = .data[[x_var]], y = .data[[y_var]])
      }

      p <- ggplot(df, mapping) +
        geom_boxplot(
          outlier.shape = NA,
          alpha = 0.7,
          color = "black",
          linewidth = 0.6,
          position = position_dodge(width = 0.75)
        ) +
        geom_point(
          shape = 21,
          color = "black",
          size = 2.4,
          stroke = 0.5,
          alpha = 0.85,
          position = position_jitterdodge(
            jitter.width = 0.15,
            dodge.width  = 0.75
          ),
          show.legend = FALSE
        ) +
        theme_minimal(base_size = 14) +
        theme(
          axis.title = element_text(face = "bold", size = 14),
          axis.text  = element_text(face = "bold", size = 14)
        ) +
        labs(
          y = y_label, x = x_var,
          fill = if (use_fill) fill_var else NULL
        )

      if (use_fill) {
        p <- p + scale_fill_manual(values = okabe_ito)
      }
      if (facet_var != "_none") {
        p <- p + facet_wrap(vars(.data[[facet_var]]), ncol = 1)
      }
      p
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
    output$dl_fixations <- make_dl(reactive(req(rv$fixations)), "encoding_fixations.csv")
    output$dl_fix_summary <- make_dl(reactive(req(rv$fix_summary)), "encoding_fix_aoi_summary.csv")
    output$dl_emoloc <- make_dl(reactive(req(rv$emoloc)), "encoding_emo_location_aoi.csv")
  })
}

recognitionServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(
      behavioral = NULL, acc = NULL, acc_cond = NULL,
      duration = NULL, validation = NULL, msg_events = NULL,
      fixations = NULL, fix_summary = NULL, fix_by_cond = NULL,
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
          rv$duration <- map(paths, function(p) {
            read_csv(p, show_col_types = FALSE) |>
              filter(!is.na(back.started)) |>
              slice_head(n = 90) |>
              transmute(
                participant   = extract_pid(p),
                trial         = row_number(),
                back_started  = suppressWarnings(as.numeric(back.started)),
                back_stopped  = suppressWarnings(as.numeric(back.stopped)),
                back_duration = back_stopped - back_started,
                corrupt       = is.na(back_started) | is.na(back_stopped)
              )
          }) |>
            list_rbind() |>
            group_by(participant) |>
            summarise(
              n          = n(),
              n_corrupt  = sum(corrupt),
              min_dur    = min(back_duration, na.rm = TRUE),
              median_dur = median(back_duration, na.rm = TRUE),
              max_dur    = max(back_duration, na.rm = TRUE),
              mean_dur   = mean(back_duration, na.rm = TRUE),
              n_off_5s   = sum(abs(back_duration - 5) > 0.05, na.rm = TRUE),
              .groups    = "drop"
            )
          rv$status <- sprintf(
            "Recognition: %d trials, %d participants.",
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
              decompose_stim = FALSE
            )

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

            fix_summary <- fix_labeled |>
              filter(stimulus_status == "old" | is.na(stimulus_status)) |>
              filter(is.na(accuracy) | accuracy == 1) |>
              filter(AOI != "Outside") |>
              group_by(participant, trial, Background, Condition, AOI) |>
              summarise(
                n_fixations = n(),
                mean_fix_duration = mean(duration, na.rm = TRUE),
                total_dwell_time = sum(duration, na.rm = TRUE),
                .groups = "drop"
              )

            fix_by_cond <- fix_summary |>
              group_by(Condition, AOI) |>
              summarise(
                n_trials = n(),
                mean_n_fixations = mean(n_fixations, na.rm = TRUE),
                mean_fix_duration = mean(mean_fix_duration, na.rm = TRUE),
                mean_total_dwell_time = mean(total_dwell_time, na.rm = TRUE),
                .groups = "drop"
              )

            rv$fixations <- fix_labeled
            rv$fix_summary <- fix_summary
            rv$fix_by_cond <- fix_by_cond
            rv$status <- sprintf(
              "Fixations: %d events across %d trials. Old + correct summary built.",
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
    output$tbl_acc <- renderDT(req(rv$acc) |> render_dt())
    output$tbl_acc_cond <- renderDT(req(rv$acc_cond) |> render_dt())
    output$tbl_duration <- renderDT(req(rv$duration) |> render_dt())
    output$tbl_validation <- renderDT(req(rv$validation) |> render_dt())
    output$tbl_msg_events <- renderDT(req(rv$msg_events) |> render_dt())
    output$tbl_fixations <- renderDT(req(rv$fixations) |> render_dt())
    output$tbl_fix_summary <- renderDT(req(rv$fix_summary) |> render_dt())
    output$tbl_fix_by_cond <- renderDT(req(rv$fix_by_cond) |> render_dt())

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
    output$dl_behavioral <- make_dl(reactive(req(rv$behavioral)), "recognition_behavioral.csv")
    output$dl_acc <- make_dl(reactive(req(rv$acc)), "recognition_accuracy.csv")
    output$dl_acc_cond <- make_dl(reactive(req(rv$acc_cond)), "recognition_accuracy_by_condition.csv")
    output$dl_duration <- make_dl(reactive(req(rv$duration)), "recognition_duration_summary.csv")
    output$dl_validation <- make_dl(reactive(req(rv$validation)), "recognition_validation.csv")
    output$dl_msg_events <- make_dl(reactive(req(rv$msg_events)), "recognition_msg_events.csv")
    output$dl_fixations <- make_dl(reactive(req(rv$fixations)), "recognition_fixations.csv")
    output$dl_fix_summary <- make_dl(reactive(req(rv$fix_summary)), "recognition_fix_summary.csv")
    output$dl_fix_by_cond <- make_dl(reactive(req(rv$fix_by_cond)), "recognition_fix_by_condition.csv")
  })
}

ui <- page_navbar(
  title = "DS mem-context ET — behavioral + ET summaries",
  theme = bs_theme(version = 5),
  nav_panel(
    "Encoding",
    phase_ui("enc", "Encoding"),
    phase_outputs_encoding("enc")
  ),
  nav_panel(
    "Recognition",
    phase_ui("rec", "Recognition"),
    phase_outputs_recognition("rec")
  ),
  nav_panel(
    "About",
    card(
      card_header("How to use"),
      tags$ol(
        tags$li("Pick the Encoding or Recognition tab."),
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
        " for fixation detection. Install with ",
        tags$code("install.packages('kollaR')"), "."
      )
    ),
    card(
      card_header("Data dictionary"),
      includeMarkdown("DATA_DICTIONARY.md")
    )
  )
)

server <- function(input, output, session) {
  encodingServer("enc")
  recognitionServer("rec")
}

shinyApp(ui, server)
