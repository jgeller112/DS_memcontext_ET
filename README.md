# DS_memcontext_ET

End-to-end pipeline for the DS memory-context eye-tracking study. Two groups
(TD = typically developing, DS = Down syndrome), two phases per participant
(**encoding** + **recognition**), with PsychoPy behavioral CSVs and Tobii
gaze + msg CSVs.

**Live app — [DS-Mem-Emo](https://connect.posit.cloud/jgeller112/content/019e2753-9be4-74bf-40d4-d9c5af097402)** (hosted on Posit Connect Cloud)

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
  "tidyverse", "here", "kollaR", "jpeg", "png", "osfr"
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

