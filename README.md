# DS_memcontext_ET

End-to-end pipeline for the DS memory-context eye-tracking study. Two groups
(TD = typically developing, DS = Down syndrome), two phases per participant
(**encoding** + **recognition**), with PsychoPy behavioral CSVs and Tobii
gaze + msg CSVs.

The repo has two faces:

| | what it is |
|---|---|
| **`encoding_behavioral.qmd`** | Quarto pipeline — full reproducible workflow from raw files → behavioral tables → I-VT fixations → AOI summaries → eyesim gaze reinstatement |
| **`app/`** | Shiny app wrapping the behavioral + ET summaries with browser uploads, AOI fixation viewer, heatmaps, and box plots |
| **`app/DATA_DICTIONARY.md`** | Column-level docs for every CSV the pipeline writes |

`Data/` is intentionally not tracked — participant files stay local.

## Setup

```r
install.packages(c(
  "shiny", "bslib", "DT", "ggplot2", "ggrain", "markdown",
  "tidyverse", "here", "kollaR", "jpeg", "png"
))
# eyesim is GitHub-only:
remotes::install_github("bbuchsbaum/eyesim")
```

## Run the app locally

```r
shiny::runApp("app")
```

The app expects three file types per phase (Encoding tab / Recognition tab):
- behavioral CSVs (PsychoPy output)
- `*_gaze.csv` (Tobii gaze samples)
- `*_msg.csv` (Tobii trial-event messages)

Participant ID is parsed from the filename as `^DS\d+[_-]\d+`.

## Expected folder layout (local)

```
Data/
  TD/
    Behavioral/{encoding,recognition}/*.csv
    Eye_tracking/{encoding,recognition}/raw_gaze_msg/*_{gaze,msg}.csv
  DS/   (mirror)
```

Screen / picture geometry assumed by the pipeline (override in `app/R/pipeline.R`
if your setup differs):

- Screen: 1920 × 1080 px
- Picture box: 700 × 550 px centered → x: 610–1310, y: 265–815
- Left / Right AOIs split at x = 960

I-VT parameters match Tobii Pro Lab defaults (30 °/s, 60 ms min fixation,
75 ms merge gap, 0.5° merge angle, `one_degree = 40 px/deg`).

## Deploy to shinyapps.io

```r
# one-time, with credentials from https://www.shinyapps.io/admin/#/tokens
rsconnect::setAccountInfo(
  name   = "<your-shinyapps-name>",
  token  = "<token>",
  secret = "<secret>"
)
rsconnect::deployApp("app", appName = "ds-memcontext-et")
```

Note: `kollaR` is in CRAN, so it installs cleanly on shinyapps.io.
`eyesim` (used by the qmd only, not the app) is GitHub-only and would need
`remotes::install_github("bbuchsbaum/eyesim")` baked into the deploy
environment if you extend the app to include reinstatement.
