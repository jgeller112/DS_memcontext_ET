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
| trial window | every trial is truncated to **exactly 5 s** (5000 ms) of gaze data after onset — any samples beyond that are dropped before preprocessing, I-VT, and missing-data QC |

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

## `encoding_missing_per_trial.csv`
One row per `(participant, trial)`. Missing-sample counts within the 5-s trial window. A sample is "missing" when the binocular-average `avg_x_px` or `avg_y_px` is NA (i.e. both eyes lost or one eye dropped and the other never recovered).

| column | type | meaning |
|---|---|---|
| `participant`, `trial` | mixed | grouping keys |
| `n_samples` | int | number of gaze samples in this trial (after truncation to ≤5000 ms) |
| `n_missing` | int | samples with NA in `avg_x_px` or `avg_y_px` |
| `prop_missing` | num | `n_missing / n_samples` (0–1) |
| `duration_ms` | num | observed duration of this trial — `max(time_ms) - min(time_ms)` |
| `trial_dur_ms` | num | configured truncation length (5000 by default) |

## `encoding_missing_per_subject.csv`
Per-participant rollup of `encoding_missing_per_trial.csv`.

| column | type | meaning |
|---|---|---|
| `participant` | chr | participant ID |
| `n_trials` | int | trials contributing to this row |
| `total_samples`, `total_missing` | int | sums across trials |
| `mean_prop_missing`, `median_prop_missing`, `max_prop_missing` | num | distribution of `prop_missing` across trials |
| `n_trials_over_thresh` | int | trials with `prop_missing > 0.50` (the default flag threshold) |
| `bad_trial_threshold` | num | threshold used for `n_trials_over_thresh` (0.50) |

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
Grouped by `(participant, Condition)` and restricted to the **old** conditions (`neg-left`, `neg-right`, `neu-left`, `neu-right`) — foil rows are dropped, since on their own they carry no hit rate. The app surfaces `participant`, `Condition`, `n_total`, `n_correct`, `accuracy`, `n_hit`, `hit_rate_raw`, `foil_fa_rate`, and `corrected_accuracy`.

| column | type | meaning |
|---|---|---|
| `n_hit` | int | hits in this Condition (old items answered "old") |
| `hit_rate_raw` | num | `n_hit / n_old` — raw hit rate |
| `foil_fa_rate` | num | raw false-alarm rate of the **emotion-matched** foils, `n_fa(foil_e) / n_new(foil_e)` |
| `corrected_accuracy` | num | `hit_rate_raw − foil_fa_rate` |

`corrected_accuracy` is the hit rate minus the **emotion-matched** foil false-alarm rate, using raw (uncorrected) proportions per participant. Let `e ∈ {neg, neu}` be the emotion of an old condition and let the matching foil be `foil_<e>`:

```
hit_rate_raw(cond) = n_hit(cond) / n_old(cond)
foil_fa_rate(e)    = n_fa(foil_e) / n_new(foil_e)      # emotion-matched foils
corrected_accuracy(cond) = hit_rate_raw(cond) − foil_fa_rate(e)   # e.g. neg-left uses foil_neg
```

So `neg-left` and `neg-right` both subtract the `foil_neg` false-alarm rate, while `neu-left` and `neu-right` subtract the `foil_neu` rate. The foils supply the false-alarm rate only and are not themselves listed. (These raw rates differ from the log-linear `hit_rate` / `fa_rate` of the per-participant table, which add the `(x+0.5)/(n+1)` correction for d′.)

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
Same columns as `encoding_msg_events.csv`. Recognition `stim` names are kept whole (the on-screen filename, typically the bare Background) — there is no encoding-style decomposition because the recognition `Composite` column is not reliably populated. Within a participant, each Background appears on exactly one recognition trial, so `(participant, Background)` is sufficient to key a trial and `Condition` rides along from the behavioral join.

## `recognition_missing_per_trial.csv`
Same definition + columns as `encoding_missing_per_trial.csv`, applied to recognition gaze (5-s truncated).

## `recognition_missing_per_subject.csv`
Same definition + columns as `encoding_missing_per_subject.csv`, applied to recognition gaze.

## `recognition_fixations.csv`
Recognition I-VT fixations, AOI-labeled, joined with `recognition_behavioral`.

| column | type | meaning |
|---|---|---|
| `participant`, `trial`, `onset`, `offset`, `duration`, `x`, `y` | mixed | as in encoding fixations |
| `stim` | chr | on-screen recognition stim filename (typically the bare Background — what the participant actually saw) |
| `Background`, `Composite`, `Object`, `Condition`, `stimulus_status`, `List`, `response`, `accuracy`, `phase` | mixed | merged from `recognition_behavioral.csv` |
| `x_pic`, `y_pic` | num | picture-relative coords |
| `AOI` | chr | `Left` / `Right` / `Outside` |

## `recognition_fix_summary.csv`
Per `(participant, trial, Background, Condition, AOI)` rollup, **restricted to correctly recognized old items** (`stimulus_status == "old"` AND `accuracy == 1`). `Outside` fixations dropped.

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

# Combined (encoding + recognition) outputs

All combined tables are restricted to Backgrounds that appear in **both** phases per participant; recognition fixations are pre-filtered to old + correct (`stimulus_status == "old"` AND `accuracy == 1`).

## `combined_fixations_long.csv`
Tall stack of encoding + recognition fixations, paired on `(participant, Background)`.

| column | type | meaning |
|---|---|---|
| `participant` | chr | participant ID |
| `Background` | chr | scene filename present in both phases |
| `Condition` | chr | encoding condition (e.g. `neg-left`) — rides along from the behavioral join |
| `List` | chr | counterbalance list label |
| `AOI` | chr | `Left` / `Right` / `Outside` |
| `x`, `y` | num | fixation centroid (px, screen coords) |
| `duration` | num | fixation duration (ms) |
| `onset` | num | fixation onset relative to trial start (ms) |
| `phase` | chr | `"encoding"` or `"recognition"` |

## `combined_per_background.csv`
One row per `(participant, Background, Condition, AOI)`. `Outside` fixations dropped before summarising. Missing phase × AOI cells are filled with `n_fix = 0`, `total_dwell = 0`, `mean_dur = NA`.

| column | type | meaning |
|---|---|---|
| `participant`, `Background`, `Condition`, `AOI` | mixed | grouping keys |
| `encoding_n_fix`, `recognition_n_fix` | int | fixation counts in this AOI in each phase |
| `encoding_mean_dur`, `recognition_mean_dur` | num | mean fixation duration (ms) in each phase |
| `encoding_total_dwell`, `recognition_total_dwell` | num | total dwell time (ms) in each phase |

## `combined_per_condition_aoi.csv`
One row per `(participant, Condition, AOI)` — `combined_per_background` averaged across Backgrounds.

| column | type | meaning |
|---|---|---|
| `participant`, `Condition`, `AOI` | mixed | grouping keys |
| `encoding_n_backgrounds`, `recognition_n_backgrounds` | int | Backgrounds contributing to this cell in each phase |
| `encoding_mean_n_fix`, `recognition_mean_n_fix` | num | mean of per-Background fixation counts |
| `encoding_mean_fix_duration`, `recognition_mean_fix_duration` | num | mean (across Backgrounds) of within-Background mean fixation duration (ms) |
| `encoding_mean_total_dwell_time`, `recognition_mean_total_dwell_time` | num | mean (across Backgrounds) of total dwell time in this AOI (ms) |

## `object_recognition_trials.csv`
Per-trial object old/new recognition, from the **Object Memory** tab. Pulled from the object block of the recognition CSV (rows where the object routine ran), separate from the background-recognition rows. Old objects were seen at encoding; new objects are foils. The tab's scope radio filters this exported table — *old + correct* (hits only), *old + correct + incorrect* (all studied items), or *all trials* (old + foils, the default); the accuracy / d′ tables below always use all trials.

| column | type | meaning |
|---|---|---|
| `participant`, `trial` | mixed | grouping keys |
| `Object` | chr | object image filename |
| `Condition` | chr | `<emo>-<location>` for old objects, `foil_<emo>` for foils |
| `emo` | chr | `neg` / `neu`, derived from `Condition` |
| `stimulus_status` | chr | `old` (seen at encoding) / `new` (foil) |
| `response` | chr | participant's old/new judgment |
| `accuracy` | int | 1 = correct, 0 = incorrect |
| `List`, `phase` | chr | counterbalance list; `phase = "object_recognition"` |

## `object_recognition_accuracy.csv`
Per-participant signal-detection object memory (same columns as `recognition_accuracy.csv`): `n_old`, `n_new`, `n_hit`, `n_cr`, `n_fa`, `n_miss`, `accuracy`, `hit_rate`, `fa_rate`, `d_prime`, `c_bias`. Hit/FA rates use the (x+0.5)/(n+1) log-linear correction.

## `object_recognition_accuracy_by_condition.csv`
One row per `(participant, Condition)`, restricted to the **old** object conditions (`<emo>-<location>`) — foil rows (`foil_<emo>`) are dropped, since on their own they carry no hit rate. Mirrors `recognition_accuracy_by_condition.csv`: the app surfaces `participant`, `Condition`, `n_total`, `n_correct`, `accuracy`, `n_hit`, `hit_rate_raw`, `foil_fa_rate`, and `corrected_accuracy`.

| column | type | meaning |
|---|---|---|
| `n_hit` | int | hits in this Condition |
| `hit_rate_raw` | num | `n_hit / n_old` — raw hit rate |
| `foil_fa_rate` | num | raw false-alarm rate of the **emotion-matched** foils, `n_fa(foil_e) / n_new(foil_e)` |
| `corrected_accuracy` | num | `hit_rate_raw − foil_fa_rate` |

`corrected_accuracy` is the hit rate minus the **emotion-matched** foil false-alarm rate, using raw (uncorrected) proportions per participant. For an old condition with emotion `e ∈ {neg, neu}` and matching foil `foil_<e>`:

```
hit_rate_raw(cond) = n_hit(cond) / n_old(cond)
foil_fa_rate(e)    = n_fa(foil_e) / n_new(foil_e)      # emotion-matched foils
corrected_accuracy(cond) = hit_rate_raw(cond) − foil_fa_rate(e)   # e.g. neg-left uses foil_neg
```

`neg-left` / `neg-right` subtract the `foil_neg` false-alarm rate; `neu-left` / `neu-right` subtract the `foil_neu` rate. The foils supply the false-alarm rate only and are not themselves listed.

## `object_recognition_accuracy_by_emotion.csv`
Same columns as above, one row per `(participant, emo)` — object memory split by emotion (negative vs. neutral). For each emotion, hits come from old objects of that emotion and false alarms from foils of that emotion. (Unlike the by-Condition table, each emotion contains both old and foil items, so `d_prime` is well defined here.)

---

# Combined recognition (background × object memory)

Behavioral-only join from the **Recognition (combined)** tab. Pairs each studied (old) item's background-recognition and object-recognition outcomes. The background block records only the scene and the object block only the object, so the two are linked through the **encoding** scene↔object pairing (`Object` is 1:1 with `Background` at encoding). Join keys are matched case-insensitively (PsychoPy sometimes varies a filename's capitalization across routines), so all 60 studied items pair. Foils are excluded (the two blocks' foils are distinct items with no cross-pairing). No eye-tracking involved. Two scope radios filter the exported rows: a **Background scope** and an **Object scope**, each either *old + correct* (that block's hits only) or *old + correct + incorrect* (all studied items, the default).

## `recognition_combined_items.csv`
One row per studied item (`participant × Background × Object`), with both memory outcomes side by side.

| column | type | meaning |
|---|---|---|
| `participant` | chr | participant ID |
| `Background` | chr | studied scene (join key to background recognition) |
| `Object` | chr | object encoded with that scene (join key to object recognition) |
| `Condition` | chr | encoding condition (e.g. `neg-left`) |
| `bg_response`, `bg_accuracy` | mixed | background-recognition response and accuracy (1 = correct) |
| `emo` | chr | `neg` / `neu` from the object block |
| `obj_response`, `obj_accuracy` | mixed | object-recognition response and accuracy (1 = correct) |
| `joint_outcome` | chr | `both recognized` / `background only` / `object only` / `neither` |

## `recognition_combined_summary.csv`
Per-participant rollup of `recognition_combined_items.csv`.

| column | type | meaning |
|---|---|---|
| `participant` | chr | grouping key |
| `n_items` | int | studied items contributing (paired in both blocks) |
| `bg_accuracy`, `obj_accuracy` | num | mean background / object recognition accuracy |
| `p_both`, `p_background_only`, `p_object_only`, `p_neither` | num | proportion of items in each `joint_outcome` bucket |

---

## Notes

- **`Outside` fixations** are dropped from all AOI summary tables (`*_aoi_summary`, `*_fix_summary`, `emo_location_aoi`). They are kept in the raw `*_fixations.csv` so you can re-filter as needed.
- **kollaR fixation columns** — the raw kollaR fixation table may include additional columns (sample counts, mean velocity, etc.) depending on package version; the columns documented above are the load-bearing ones.
- **Stim filename schema** (encoding only): `<background>.<object>.<emo>.<location>.jpeg`. Some stims use `_` as the delimiter — the pipeline normalizes those before splitting.
