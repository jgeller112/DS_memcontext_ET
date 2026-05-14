# Data dictionary — DS_memcontext_ET app outputs

Every CSV the Shiny app writes is described below. The qmd pipeline produces the same shapes (with the group prefix `TD_` / `DS_`).

---

## Conventions used across all files

| concept | value |
|---|---|
| screen | 1920 × 1080 px; y grows downward in screen coords |
| picture box | 700 × 550 px centered → x: 610–1310, y: 265–815 |
| AOI split | Left = x 610–960, Right = x 960–1310 (`Outside` = anywhere else) |
| participant ID | parsed from filename as `^DS\d+[_-]\d+` (e.g. `DS21-2120`, `DS24_2056`) |
| trial | 1-indexed row order within a participant's behavioral file (after non-task rows are dropped) |
| time | Tobii `system_time_stamp` is microseconds; all derived `*_ms` / `*_dwell` / `duration` fields are milliseconds |
| I-VT params | Tobii Pro Lab defaults — 30 °/s velocity threshold, 60 ms min fixation, 75 ms merge gap, 0.5 ° merge angle, `one_degree = 40 px/deg` |

---

# Encoding outputs

## `encoding_behavioral.csv`
One row per encoding trial, stacked across every uploaded behavioral CSV.

| column | type | meaning |
|---|---|---|
| `participant` | chr | participant ID parsed from filename |
| `trial` | int | 1-indexed trial number within participant |
| `Background` | chr | background-scene filename (e.g. `airyroom.jpeg`) |
| `Object` | chr | object overlaid on the scene (e.g. `seaweed`) |
| `Condition` | chr | trial condition tag — `<emo>-<location>`, e.g. `neg-right`, `neu-left` |
| `List` | chr | counterbalance list label (e.g. `CB1`, `CB2`) |
| `mouse_clicked_name` | chr | PsychoPy `mouse.clicked_name` — the button label clicked on this trial (e.g. `['thumbs_up']`) |
| `phase` | chr | always `"encoding"` |

## `encoding_validation.csv`
One row per calibration-validation block (sessions with recalibrations produce multiple rows per participant).

| column | type | meaning |
|---|---|---|
| `participant` | chr | participant ID |
| `system_time_stamp` | num | Tobii timestamp at the validation event (µs) |
| `Dev_L`, `Dev_R` | num | mean deviation, left/right eye (degrees of visual angle) |
| `RMS_L`, `RMS_R` | num | RMS sample-to-sample noise, left/right eye (degrees) |
| `LOSS_L`, `LOSS_R` | num | proportion of lost samples, 0–1 |
| `SD_L`, `SD_R` | num | spatial SD of validation samples, left/right eye (degrees) |

## `encoding_msg_events.csv`
Trial-onset and offset markers from the Tobii msg file.

| column | type | meaning |
|---|---|---|
| `participant` | chr | participant ID |
| `system_time_stamp` | num | Tobii timestamp (µs) |
| `msg` | chr | raw message (e.g. `onset_airyroom.seaweed.neu.right.jpeg`) |
| `event` | chr | `"onset"` or `"offset"` |
| `stim` | chr | the stim filename the message refers to (msg with the `onset_`/`offset_` prefix stripped) |

## `encoding_fixations.csv`
Every I-VT fixation event, AOI-labeled and joined with behavioral fields. Output of `kollaR::algorithm_ivt()` after `kollaR::preprocess_gaze()`, restricted to within-trial samples.

| column | type | meaning |
|---|---|---|
| `participant` | chr | participant ID |
| `trial` | int | trial number |
| `onset` | num | fixation onset (ms, relative to trial onset) |
| `offset` | num | fixation offset (ms) |
| `duration` | num | fixation duration (ms) |
| `x`, `y` | num | fixation centroid (px, screen coords) |
| `stim` | chr | full stim filename for this trial |
| `background`, `object`, `emo`, `location` | chr | components decomposed from `stim` — `<background>.<object>.<emo>.<location>.jpeg` |
| `Background`, `Object`, `Condition`, `List`, `mouse_clicked_name`, `phase` | mixed | behavioral fields joined from `encoding_behavioral.csv` |
| `x_pic`, `y_pic` | num | fixation coords relative to picture top-left (`x - 610`, `y - 265`) |
| `AOI` | chr | `"Left"`, `"Right"`, or `"Outside"` based on which half of the picture box the fixation falls in |

(Additional kollaR columns — sample counts, mean velocity, etc. — may pass through depending on package version.)

## `encoding_fix_aoi_summary.csv`
Per `(participant, trial, AOI)` rollup of the encoding fixations. `Outside` fixations dropped.

| column | type | meaning |
|---|---|---|
| `participant`, `trial`, `AOI` | mixed | grouping keys |
| `background`, `object`, `emo`, `location` | chr | trial stim attributes |
| `mean_fix_duration` | num | mean fixation duration in this AOI on this trial (ms) |
| `n_fixations` | int | number of fixations in this AOI on this trial |
| `total_dwell_time` | num | sum of fixation durations in this AOI on this trial (ms) |

## `encoding_emo_location_aoi.csv`
Per `(participant, emo, location, AOI)` summary, averaged across trials. Every trial is crossed with both `AOI = Left` and `AOI = Right` before grouping, so AOIs with zero fixations on a given trial count as zeros (rather than dropping the trial).

| column | type | meaning |
|---|---|---|
| `participant` | chr | participant ID |
| `emo` | chr | trial emotion tag (e.g. `neg`, `neu`) parsed from the stim filename |
| `location` | chr | cued object side (`left` / `right`), parsed from the stim filename |
| `AOI` | chr | which AOI this row summarises fixations *into* — `Left` or `Right` |
| `on_object` | lgl | `TRUE` when `AOI` matches `location` (i.e. fixation landed on the object side) |
| `n_trials` | int | number of trials in this `(participant, emo, location)` cell — identical for the Left and Right AOI rows within the same cell |
| `mean_n_fixations` | num | mean (across trials) number of fixations landing in this AOI |
| `mean_fix_duration` | num | mean (across trials) of each trial's mean fixation duration in this AOI (ms; zero-fix trials excluded since duration is undefined there) |
| `mean_total_dwell_time` | num | mean (across trials) total dwell time in this AOI (ms; zero-fix trials counted as 0) |

> **How to read a pair of rows:** within `(emo=neg, location=left)`, compare the `on_object=TRUE` row (AOI=Left) to `on_object=FALSE` (AOI=Right) — the difference is the object-side dwell preference.

---

# Recognition outputs

## `recognition_behavioral.csv`
One row per back-task trial (capped at 90), stacked across uploaded recognition CSVs.

| column | type | meaning |
|---|---|---|
| `participant` | chr | participant ID |
| `trial` | int | trial number within participant |
| `Background` | chr | background filename probed on this trial |
| `Composite` | chr | composite stim filename (full encoded stim if shown) |
| `Object` | chr | object name (when applicable) |
| `Condition` | chr | encoding condition tag for this Background (e.g. `neg-left`) |
| `stimulus_status` | chr | `"old"` (Background was studied at encoding) or `"new"` (lure) |
| `List` | chr | counterbalance list label |
| `response` | chr | PsychoPy response code (e.g. `"old"` / `"new"`) |
| `accuracy` | int | 1 if `response` matches `stimulus_status`, 0 otherwise |
| `phase` | chr | always `"recognition"` |

## `recognition_accuracy.csv`
One row per participant. Log-linear-corrected signal-detection metrics (Hautus 1995): 0.5 added to hits/FAs, 1 added to old/new totals.

| column | type | meaning |
|---|---|---|
| `participant` | chr | participant ID |
| `n_old`, `n_new` | int | trial counts for old / new items |
| `n_hit`, `n_cr` | int | correct old responses (hits) and correct new responses (correct rejections) |
| `n_fa`, `n_miss` | int | derived: `n_new - n_cr` and `n_old - n_hit` |
| `n_total`, `n_correct` | int | total scored trials and total correct |
| `accuracy` | num | `n_correct / n_total` (0–1) |
| `hit_rate` | num | `(n_hit + 0.5) / (n_old + 1)` |
| `fa_rate` | num | `(n_fa + 0.5) / (n_new + 1)` |
| `d_prime` | num | `qnorm(hit_rate) - qnorm(fa_rate)` |
| `c_bias` | num | `-0.5 * (qnorm(hit_rate) + qnorm(fa_rate))` — response criterion |

## `recognition_accuracy_by_condition.csv`
Same metrics as above, grouped by `(participant, Condition)`. Columns identical plus `Condition`.

## `recognition_duration_summary.csv`
QC on back-task picture-display duration. Each back-task picture should be on screen ~5 s.

| column | type | meaning |
|---|---|---|
| `participant` | chr | participant ID |
| `n` | int | number of back-task trials checked |
| `n_corrupt` | int | trials where `back.started` / `back.stopped` couldn't be parsed as numeric (often PsychoPy array literals) |
| `min_dur`, `median_dur`, `max_dur`, `mean_dur` | num | distribution of `back.stopped - back.started` (seconds) |
| `n_off_5s` | int | number of trials whose duration deviates from 5 s by more than 50 ms |

## `recognition_validation.csv`
Same columns as `encoding_validation.csv` — calibration QC for the recognition session.

## `recognition_msg_events.csv`
Same columns as `encoding_msg_events.csv`. Recognition stim names are kept whole (not decomposed into background/object/emo/location).

## `recognition_fixations.csv`
Recognition I-VT fixations, AOI-labeled, joined with `recognition_behavioral`.

| column | type | meaning |
|---|---|---|
| `participant`, `trial`, `onset`, `offset`, `duration`, `x`, `y` | mixed | as in encoding fixations |
| `stim` | chr | recognition stim filename (not decomposed) |
| `Background`, `Composite`, `Object`, `Condition`, `stimulus_status`, `List`, `response`, `accuracy`, `phase` | mixed | merged from `recognition_behavioral.csv` |
| `x_pic`, `y_pic` | num | picture-relative coords |
| `AOI` | chr | `Left` / `Right` / `Outside` |

## `recognition_fix_summary.csv`
Per `(participant, trial, Background, Condition, AOI)` rollup, **restricted to correctly recognized old items** (`stimulus_status == "old"` AND `accuracy == 1`). `Outside` fixations dropped. Matches the eyesim reinstatement scope.

| column | type | meaning |
|---|---|---|
| `participant`, `trial`, `Background`, `Condition`, `AOI` | mixed | grouping keys |
| `n_fixations` | int | number of fixations in this AOI on this trial |
| `mean_fix_duration` | num | mean fixation duration in this AOI on this trial (ms) |
| `total_dwell_time` | num | sum of fixation durations in this AOI on this trial (ms) |

## `recognition_fix_by_condition.csv`
Group-level rollup of `recognition_fix_summary.csv` — one row per `(Condition, AOI)`.

| column | type | meaning |
|---|---|---|
| `Condition`, `AOI` | chr | grouping keys |
| `n_trials` | int | number of (participant × trial) rows feeding into this cell |
| `mean_n_fixations` | num | mean fixations per trial in this AOI under this Condition |
| `mean_fix_duration` | num | mean of trial-level mean fixation durations (ms) |
| `mean_total_dwell_time` | num | mean of trial-level total dwell times (ms) |

---

## Notes

- **`Outside` fixations** are dropped from all AOI summary tables (`*_aoi_summary`, `*_fix_summary`, `emo_location_aoi`). They are kept in the raw `*_fixations.csv` so you can re-filter as needed.
- **kollaR fixation columns** — the raw kollaR fixation table may include additional columns (sample counts, mean velocity, etc.) depending on package version; the columns documented above are the load-bearing ones.
- **Stim filename schema** (encoding only): `<background>.<object>.<emo>.<location>.jpeg`. Some stims use `_` as the delimiter — the pipeline normalizes those before splitting.
