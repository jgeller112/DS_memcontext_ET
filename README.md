# DS_memcontext_ET

End-to-end pipeline for the DS memory-context eye-tracking study. Two groups
(TD = typically developing, DS = Down syndrome), two phases per participant
(**encoding** + **recognition**), with PsychoPy behavioral CSVs and Tobii
gaze + msg CSVs.

**Live app — [DS-Mem-Emo](https://connect.posit.cloud/jgeller112/content/019e2753-9be4-74bf-40d4-d9c5af097402)** (hosted on Posit Connect Cloud)

The repo has two faces:

| | what it is |
|---|---|
| **`encoding_behavioral.qmd`** | Quarto pipeline — full reproducible workflow from raw files → behavioral tables → I-VT fixations → AOI summaries → gaze reinstatement (eyesim density similarity **and** Left/Right AUC) → object-recognition memory. Can pull the raw data straight from OSF via `osfr`. |
| **`app/`** | Shiny app wrapping the behavioral + ET summaries: browser uploads, AOI fixation viewer, heatmaps, box plots, encoding↔recognition fixation summaries, object-memory d′, and combined background×object recognition |
| **`app/DATA_DICTIONARY.md`** | Column-level docs for every CSV the pipeline writes |

`Data/` is intentionally not tracked — participant files stay local. The qmd's
"Get the data (OSF)" chunk downloads them into `Data/` from the (public) OSF
component [`64a2j`](https://osf.io/64a2j/) when they're absent.

## Setup

Install the packages directly:

```r
install.packages(c(
  "shiny", "bslib", "DT", "ggplot2", "ggrain", "markdown",
  "tidyverse", "here", "kollaR", "jpeg", "png", "osfr"
))
# eyesim is GitHub-only (used by the qmd's gaze-reinstatement section):
remotes::install_github("bbuchsbaum/eyesim")
```

…or restore the pinned versions the app was built with (the `app/` project is
`renv`-managed, lockfile committed):

```r
setwd("app"); renv::restore()
```

## Run the app locally

```r
shiny::runApp("app")
```

Tabs:

- **Encoding** / **Background Recognition** — upload the behavioral CSV(s),
  the `*_gaze.csv`, and `*_msg.csv` for that phase, then run behavioral
  tables, validation QC, or the full I-VT fixation pipeline.
- **Object Memory** — upload the recognition behavioral CSV(s); computes
  object old/new memory (accuracy, hit/FA, d′, criterion) overall and by
  emotion. Behavioral only — object recognition had no eye-tracking.
- **Recognition (combined)** — pairs each studied item's background- and
  object-recognition outcomes (behavioral only), linked through the encoding
  scene↔object pairing, with a per-participant joint-memory summary.
- **Combined** — encoding↔recognition fixation summaries (paired fixations,
  per-Background and per-Condition AOI dwell).

Each phase tab expects three file types:
- behavioral CSVs (PsychoPy output)
- `*_gaze.csv` (Tobii gaze samples)
- `*_msg.csv` (Tobii trial-event messages)

Participant ID is parsed from the filename as `^DS\d+[_-]\d+` (so it must match
across phases — e.g. encoding and recognition files for the same person need
the same `DS…` prefix).

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

