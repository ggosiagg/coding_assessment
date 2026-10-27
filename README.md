# coding_assessment

An R-based clinical programming assessment covering the three core areas of a
statistical programming workflow in the pharmaceutical industry: **SDTM**,
**ADaM**, and **TLG** (Tables, Listings, Graphs). The project is built with the
[pharmaverse](https://pharmaverse.org/) ecosystem of packages
(`sdtm.oak`, `admiral`, `pharmaverseraw`, `pharmaversesdtm`, `pharmaverseadam`)
and runs against example clinical trial data shipped with those packages.

## Repository structure

```
coding_assessment/
├── inst/data/
│   └── sdtm_ct.csv                      # SDTM controlled terminology (CT) specification
├── question_1_sdtm/                     # Question 1: SDTM DS domain
│   ├── 01_create_ds_domain.R            #   Builds the SDTM Disposition (DS) domain
│   ├── 01_run_ds_domain_with_log.R      #   Runner that executes the script and captures a log
│   ├── 01_create_ds_domain.log          #   Execution log
│   └── ds.RDS                           #   Output: final DS domain dataset
├── question_2_adam/                     # Question 2: ADaM ADSL
│   ├── create_adsl.R                    #   Builds the Subject-Level Analysis Dataset (ADSL)
│   ├── run_create_adsl_with_log.R       #   Runner that executes the script and captures a log
│   ├── create_adsl.log                  #   Execution log
│   └── adsl.RDS                         #   Output: final ADSL dataset
├── question_3_tlg/                      # Question 3: Tables, Listings, Graphs
│   ├── 01_create_ae_summary_table.R     #   Hierarchical AE summary table (gtsummary/gt)
│   ├── 01_run_ae_summary_table_with_log.R
│   ├── 01_create_ae_summary_table.log
│   ├── 02_create_visualizations.R       #   AE severity bar chart + Top 10 AE chart
│   ├── 02_run_create_visualisations_with_log.R
│   ├── 02_create_visualizations.log
│   ├── ae_summary_table.html            #   Output: rendered AE summary table
│   ├── ae_severity_bar_chart.png        #   Output: AE severity by treatment arm
│   └── top10_ae_chart.png               #   Output: Top 10 AEs with 95% CIs
└── project.Rproj                        # RStudio project file
```

## Question 1 — SDTM (`question_1_sdtm/`)

Creates the SDTM **Disposition (DS) domain** from raw disposition data
(`pharmaverseraw::ds_raw`) using `sdtm.oak` mapping utilities:

- Maps `DSTERM`, `DSDECOD` (via controlled terminology codelist C66727) and
  derives `DSCAT` from business rules (randomized subjects → "PROTOCOL
  MILESTONE", non-randomized → "DISPOSITION EVENT", others → "OTHER EVENT").
- Derives `DSSTDTC` / `DSDTC` from raw date/time components, handling unknown
  components ("UN", "UNK") via `assign_datetime()`.
- Maps `VISIT` / `VISITNUM` from the raw `INSTANCE` variable through CT.
- Finalizes the domain with `STUDYID`, `USUBJID`, `DSSEQ` and `DSSTDY`
  (relative study day vs. `DM.RFSTDTC`).

**Notable finding:** controlled terminology mismatches (case mismatch for
"Ambul ECG Removal" and missing "Unscheduled X.X" entries) were patched
in-memory for the exercise; in production these would be raised as
standards/data-management governance issues.

## Question 2 — ADaM (`question_2_adam/`)

Creates the ADaM **Subject-Level Analysis Dataset (ADSL)** from SDTM domains
(DM, EX, VS, AE, DS) using `admiral` derivation utilities:

- Derives age groupings `AGEGR9` / `AGEGR9N` via category lookup tables.
- Derives treatment start/end datetimes `TRTSDTM` / `TRTEDTM` (with time
  imputation flags) from first/last valid EX dose — a valid dose is
  `EXDOSE > 0`, or `EXDOSE == 0` when `EXTRT` contains "PLACEBO".
- Derives `ITTFL` (intent-to-treat flag) from `ARM` assignment.
- Derives `LSTALVDT` (last known alive date) as the maximum complete date
  across four sources: valid vital signs assessments, AE onset dates,
  disposition dates, and the date part of `TRTEDTM`.

**Notable finding:** per the current spec, subjects with `ARM = "Screen
Failure"` still receive `ITTFL = "Y"` — flagged as a likely spec gap for
follow-up with standards/stats owners.

## Question 3 — TLG (`question_3_tlg/`)

Produces AE reporting outputs from ADaM datasets (`pharmaverseadam::adsl`,
`pharmaverseadam::adae`):

1. **AE summary table** (`ae_summary_table.html`) — a hierarchical summary of
   treatment-emergent adverse events (TEAEs) by System Organ Class (`AESOC`)
   and Preferred Term (`AETERM`), split by planned treatment (`TRT01A`), built
   with `gtsummary::tbl_hierarchical()` and rendered to HTML via `gt`. Unique
   subjects are counted per SOC/PT level, with percentages against the full
   ADSL safety population denominator.
2. **Visualizations**:
   - `ae_severity_bar_chart.png` — AE severity distribution by treatment arm
     (`ggplot2`).
   - `top10_ae_chart.png` — top 10 most frequent AEs (one record per subject
     per term) with exact 95% Clopper-Pearson confidence intervals.

AE analyses are restricted to the safety population (`SAFFL == "Y"`); TEAEs
follow `TRTEMFL == "Y"`.

## Reproducibility

- Each main script has a companion `*_with_log.R` runner that re-executes the
  program while capturing console output to a `.log` file for audit purposes.
- The `02_create_visualizations.R` script also records `sessionInfo()` in its
  log.
- Intermediate/final datasets are saved as `.RDS` files in their respective
  question folders.

## Required R packages

`sdtm.oak`, `pharmaverseraw`, `pharmaversesdtm`, `pharmaverseadam`, `admiral`,
`dplyr`, `lubridate`, `stringr`, `gtsummary`, `gt`, `ggplot2`

## Getting started

1. Open `project.Rproj` in RStudio.
2. Install the packages listed above.
3. Run any of the `*_with_log.R` runner scripts (or the main scripts directly)
   to regenerate the datasets, table, and plots.
