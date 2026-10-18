# methylclock 2.0.0

(In Bioconductor devel as the 1.99.x series, following the convention for a
major rewrite; it becomes 2.0.0 at the next release.)

A ground-up rewrite. The package now computes **44 epigenetic clocks** behind
a declarative registry, runs out-of-core on HDF5 through BigDataStatMeth, and
ships a toolbox to compare, interpret and quality-control the results.

## Architecture

* **Declarative clock registry** (`clock_register()`, `clock_list()`,
  `clock_info()`, and the shipped `clock_catalog` table): one entry per
  clock — target, engine, what it estimates, native platforms, training
  tissue, licence and citation. Adding a clock is adding a row, not
  editing the engine.
* **Manifest-backed resource resolver** (`mcd_resource()`,
  `mcd_backends()`): coefficients are resolved through a configurable
  backend chain — a local mirror, ExperimentHub, or a direct Zenodo
  download — and normalized to one canonical shape on entry
  (`normalize_coef()`).
* **Three predictor engines** in this release: linear (including models
  with squared-beta terms), counter (mitotic clocks, including the
  parametric epiTOC2), and neural networks (AltumAge with weights from
  HDF5; a compiled Bayesian network).
* A unified `methylclock()` entry point; `DNAmAge()` and `DNAmGA()` remain
  as compatible wrappers.

## Clocks

* 44 clocks: chronological (including the cross-platform Garma 2024 model,
  the only one here native to EPICv2), biological (PhenoAge), pace of
  ageing (DunedinPACE), telomere length, gestational (cord blood and
  placenta), neonatal (the four NEOage clocks), mitotic counters, causal
  clocks, and nine trait/exposure EpiScores (opt-in via
  `target = "trait"`).
* Every clock declares the **tissue it was trained on**, the quantity it
  estimates with its units (the four NEOage clocks report post-menstrual
  and postnatal age in weeks; Levine, phenotypic age), and its native
  arrays. The arrays are where a clock was built and validated, not where
  it can be computed — CpG coverage decides that. Restrictively licensed
  clocks are implemented and clearly marked.

## Scale

* Input can be an in-memory matrix or an **HDF5-backed matrix**; whole-array
  clocks (BLUP, EN) stream from disk without materializing the array, and
  the heavy algebra runs through **BigDataStatMeth**. `mc_to_hdf5()`
  converts a matrix or a text file once, to work on disk from then on.
* Results follow the same policy: HDF5 as the durable store with an
  in-memory cache, and `persist()` to keep a session result.

## Missing data

* Configurable imputation: `impute = "mean"`, `"reference"`, `"none"`, or
  **`"knn"`** — a C++ k-nearest-neighbours imputation that runs out-of-core
  on HDF5, exposed also as `imputeKNN()`; plus a per-sample coverage floor
  (`min.perc.sample`) so heavily imputed samples are not passed off as
  measured ones.

## Interpretation toolbox

* Cell composition and acceleration: `cellCounts()`, `ageAcceleration()`
  (ageAcc, residual, and the cell-adjusted residualCells), plus the
  canonical named measures: `IEAA()` (Horvath residual adjusted for the
  seven immune covariates of Chen et al. 2016, the three rare subsets
  computed from published CpG estimators) and `EEAA()` (the fixed-weight
  Hannum blend of Chen et al.
  2016, with the parameters published in European patent EP 3 494 210 B1).
  `ageAccelerationChen()` computes a related Chen-style blend whose
  Klemera-Doubal weights are re-estimated from the data at hand; its
  output column is `chenAcc`, to keep the EEAA name for the canonical
  measure.
* Plots: predicted-vs-age (per group), clock agreement, distributions,
  densities by group (overlay, mirror and ridge layouts), acceleration by
  group, forest/dumbbell group differences, per-sample discordance, sample
  PCA, Bland–Altman — one consistent theme, colour-blind-safe palettes.
* **Reference ranges** (`plotReferenceRange()`): percentile bands of a
  *named* reference cohort with the user's samples on top; nine shipped
  reference cohorts spanning four continents, each described by what it is.
* **Longitudinal tools**: `clockTrajectories()`, `trajectoryRates()` (each
  subject's pace, with its uncertainty caveats) and `plotTrajectories()`,
  plus a real repeated-measures example dataset.
* **Quality control**: `clockAccuracy()`, `sampleQC()` (outlier score and
  clock discordance) and `qcReport()` — an HTML report where every section
  carries a written reading of its own numbers.

## Data

* Only estimates and phenotypes ship with the package; coefficient tables
  live in `methylclockData`/Zenodo, and validation used frozen public
  cohorts. Bundled datasets: `clock_catalog`, `methylclock_demo`,
  `methylclock_validation`, `methylclock_references`,
  `methylclock_longitudinal`, `methylclock_smoking`, `methylclock_cells`,
  `methylclock_betas`.

## Compatibility notes

* Estimates were verified against the previous implementations; the one
  deliberate change: `Wu` uses `adult.age = 48`, the value of the clock's
  publication.
* EPICv2 arrays: the pre-2023 clocks do not apply to EPICv2 probe names and
  say so rather than guessing; the Garma model runs on EPICv2 natively.
  A proper probe mapping for the older clocks is planned.
