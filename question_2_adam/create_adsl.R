# ==============================================================================
# Author      : ggosiagg
# Created on  : 2026-10-27
# File        : question_2_adam/create_adsl.R.R
# Purpose     : Build ADaM ADSL (Subject-Level Analysis Dataset) from SDTM
#               domains using admiral derivation utilities.
#
# High-level steps implemented in this script:
#   1) Load required packages and SDTM source domains (DM, EX, VS, AE, DS).
#   2) Convert blank values to NA for robust derivations.
#   3) Initialize ADSL from DM (drop DOMAIN).
#   4) Derive age grouping variables (AGEGR9, AGEGR9N) via lookup tables.
#   5) Derive treatment start datetime (TRTSDTM, TRTSTMF) from first valid EX dose.
#   6) Derive ITTFL (intent-to-treat flag).
#   7) Derive treatment end datetime (TRTEDTM, TRTETMF) from last valid EX dose.
#   8) Derive LSTALVDT (last known alive date) from extreme events across
#      VS, AE, DS and ADSL.TRTEDTM.
#   9) Save final ADSL dataset as an RDS file.
#
# Key findings to report:
#   - Valid dose definition: EXDOSE > 0, or EXDOSE == 0 with EXTRT containing
#     "PLACEBO" (placebo zero-dose records still count as treatment).
#   - EX datetimes imputed from hour onward only (highest_imputation = "h");
#     start times imputed to 00:00:00, end times to 23:59:59 per convention.
#   - ITTFL is derived as specified (ARM non-missing -> "Y"); note: subjects
#     with ARM = "Screen Failure" will still get ITTFL = "Y" — this looks like
#     a spec gap and should be raised with standards/stats owners.
#   - LSTALVDT considers only complete dates (nchar(dtc) >= 9, i.e. no partial
#     dates) from VS/AE/DS, plus datepart of TRTEDTM; maximum date wins.
# ==============================================================================

library(admiral)
library(dplyr)
library(pharmaversesdtm)
library(lubridate)
library(stringr)

# ------------------------------------------------------------------------------
# 1) Read SDTM source data
# ------------------------------------------------------------------------------
# Source SDTM domains from pharmaversesdtm package
dm <- pharmaversesdtm::dm
ex <- pharmaversesdtm::ex
vs <- pharmaversesdtm::vs
ae <- pharmaversesdtm::ae
ds <- pharmaversesdtm::ds

# ------------------------------------------------------------------------------
# 2) Pre-process: convert blanks to NA
# ------------------------------------------------------------------------------
# Ensures missing-value logic in derivations is robust ("" vs NA)
dm <- convert_blanks_to_na(dm)
ex <- convert_blanks_to_na(ex)
vs <- convert_blanks_to_na(vs)
ae <- convert_blanks_to_na(ae)
ds <- convert_blanks_to_na(ds)

# ------------------------------------------------------------------------------
# 3) Initialize ADSL from DM
# ------------------------------------------------------------------------------
# Start by assigning DM to ADSL, drop DOMAIN column
adsl <- dm %>%
  select(-DOMAIN)

# ------------------------------------------------------------------------------
# 4) Derive AGEGR9, AGEGR9N (age groupings)
# ------------------------------------------------------------------------------
# Lookup: categorize AGE into AGEGR9 bands
agegr9_lookup <- exprs(
  ~condition,           ~AGEGR9,
  AGE < 18,               "<18",
  between(AGE, 18, 50), "18-50",
  AGE > 50,               ">50"
)

# Lookup: map AGEGR9 to its numeric counterpart AGEGR9N
agegr9n_lookup <- exprs(
  ~condition,           ~AGEGR9N,
  AGEGR9 == "<18",            1,
  AGEGR9 == "18-50",          2,
  AGEGR9 == ">50",            3
)

# Apply both categorization lookups sequentially (AGEGR9N depends on AGEGR9)
adsl <- adsl %>%
  derive_vars_cat(
    definition = agegr9_lookup
  ) %>%
  derive_vars_cat(
    definition = agegr9n_lookup
  )

# ------------------------------------------------------------------------------
# 5) Derive TRTSDTM, TRTSTMF (first treatment datetime)
# ------------------------------------------------------------------------------
# Pre-process EX: build EXSTDTM from EXSTDTC.
# Impute only from hour onward (date must be complete); missing time -> 00:00:00.
# ignore_seconds_flag = FALSE: imputed seconds do not set an imputation flag.
ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    highest_imputation = "h",
    time_imputation = "00:00:00",
    ignore_seconds_flag = FALSE,
    new_vars_prefix = "EXST"
  )

# Merge first valid dose: EXDOSE > 0, or EXDOSE == 0 with EXTRT containing
# "PLACEBO" (placebo zero-dose counts as treatment start)
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  )

# ------------------------------------------------------------------------------
# 6) Derive ITTFL (intent-to-treat flag)
# ------------------------------------------------------------------------------
# Rule: subject assigned to an ARM -> "Y", otherwise "N".
# Note: ARM = "Screen Failure" subjects still get "Y" per current spec;
# flagged as a likely spec gap for follow-up with standards/stats owners.
adsl <- adsl %>%
  mutate(
    ITTFL = if_else(is.na(ARM), "N", "Y")
  )

# ------------------------------------------------------------------------------
# 7) Derive TRTEDTM, TRTETMF (last treatment datetime)
# ------------------------------------------------------------------------------
# Needed as one of the LSTALVDT source events (spec point (4)).
# Same valid-dose logic as TRTSDTM but mode = "last"; end times -> 23:59:59.
ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    highest_imputation = "h",
    time_imputation = "23:59:59",
    ignore_seconds_flag = FALSE,
    new_vars_prefix = "EXEN"
  )

# Merge last valid dose (EXDOSE > 0, or EXDOSE == 0 with EXTRT containing "PLACEBO")
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))),
    new_vars = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  )

# ------------------------------------------------------------------------------
# 8) Derive LSTALVDT (last known alive date)
# ------------------------------------------------------------------------------
# Extreme-event derivation across four sources; only complete dates
# (nchar(dtc) >= 9) qualify, maximum date wins:
#   (1) VS: last vital assessment with valid result (VSSTRESN & VSSTRESC present)
#   (2) AE: last complete AE onset date (AESTDTC)
#   (3) DS: last complete disposition date (DSSTDTC)
#   (4) ADSL: datepart of TRTEDTM (last valid treatment administration)
adsl <- adsl %>%
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    events = list(
      # (1) last complete date of vital assessment with a valid test result
      event(
        dataset_name = "vs",
        order = exprs(VSDTC, VSSEQ),
        condition = !is.na(VSSTRESN) & !is.na(VSSTRESC) & !is.na(VSDTC) & nchar(VSDTC) >= 9,
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(VSDTC, highest_imputation = "n"),
          seq = VSSEQ
        )
      ),
      # (2) last complete onset date of AEs
      event(
        dataset_name = "ae",
        order = exprs(AESTDTC, AESEQ),
        condition = !is.na(AESTDTC) & nchar(AESTDTC) >= 9,
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(AESTDTC, highest_imputation = "n"),
          seq = AESEQ
        ),
      ),
      # (3) last complete disposition date
      event(
        dataset_name = "ds",
        order = exprs(DSSTDTC, DSSEQ),
        condition = !is.na(DSSTDTC) & nchar(DSSTDTC) >= 9,
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(DSSTDTC, highest_imputation = "n"),
          seq = DSSEQ
        ),
      ),
      # (4) last date of treatment administration with a valid dose
      event(
        dataset_name = "adsl",
        condition = !is.na(TRTEDTM),
        set_values_to = exprs(
          LSTALVDT = as.Date(TRTEDTM)
        ),
      )
    ),
    # Take values from all 4 datasets
    source_datasets = list(vs = vs, ae = ae, ds = ds, adsl = adsl),
    tmp_event_nr_var = event_nr,
    # Order by date (then seq, then event) and take the maximum
    order = exprs(LSTALVDT, seq, event_nr),
    mode = "last",
    new_vars = exprs(LSTALVDT)
  )

# ------------------------------------------------------------------------------
# 9) Save output
# ------------------------------------------------------------------------------
saveRDS(adsl,"question_2_adam/adsl.RDS")

