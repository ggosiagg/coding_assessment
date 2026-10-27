# ==============================================================================
# Author      : ggosiagg
# Created on  : 2026-10-27
# File        : question_1_sdtm/01_create_ds_domain.R
# Purpose     : Build SDTM DS (Disposition) domain from raw disposition data
#               using pharmaverse/sdtm.oak mapping utilities.
#
# High-level steps implemented in this script:
#   1) Load required packages and raw source data.
#   2) Create oak identifiers for row-level traceability.
#   3) Load and (temporarily) augment controlled terminology (CT).
#   4) Map DS topic/decision/category variables (DSTERM, DSDECOD, DSCAT).
#   5) Derive SDTM date/datetime variables (DSSTDTC, DSDTC).
#   6) Map VISIT and VISITNUM from INSTANCE using CT.
#   7) Finalize SDTM-required variables and derivations (DSSEQ, DSSTDY).
#   8) Save final DS dataset as an RDS file.
#
# key findings to report:
#   - Controlled terminology mismatches were found and handled locally:
#       a) Case mismatch: "Ambul Ecg Removal" (raw) vs "Ambul ECG Removal" (CT).
#       b) Missing CT entries for several unscheduled visits present in raw data.
#   - For exercise completion, CT was patched in-script; in production this should
#     trigger governance follow-up with standards/DM owners (not hardcoded fixes).
#   - Business logic for DSCAT depends on randomization status and OTHERSP missingness.
#   - Date handling supports unknown components ("UN", "UNK") via assign_datetime().
# ==============================================================================

library(sdtm.oak)
library(pharmaverseraw)
library(pharmaversesdtm)
library(dplyr)

# ------------------------------------------------------------------------------
# 1) Read raw data
# ------------------------------------------------------------------------------
# Source disposition raw data from pharmaverse package
ds_raw <- pharmaverseraw::ds_raw

# ------------------------------------------------------------------------------
# 2) Generate internal oak identifiers for robust joins/traceability
# ------------------------------------------------------------------------------
# oak_id variables are used by mapping helpers to align transformed records
ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

# ------------------------------------------------------------------------------
# 3) Load controlled terminology specification
# ------------------------------------------------------------------------------
study_ct <- read.csv("inst/data/sdtm_ct.csv")

# ------------------------------------------------------------------------------
# 4) Map core DS topic variable (DSTERM)
# ------------------------------------------------------------------------------
# Rule:
#   - If OTHERSP is populated, use OTHERSP as DSTERM
#   - Otherwise use IT.DSTERM
#
# Note: assign_no_ct is used because these are direct assignments (no CT lookup)
ds <-
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "OTHERSP",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  ) |>
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "IT.DSTERM",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  )

# ------------------------------------------------------------------------------
# 5) Map disposition decoding variable (DSDECOD)
# ------------------------------------------------------------------------------
# Rule:
#   - If OTHERSP is populated, map directly from OTHERSP
#   - Otherwise map IT.DSDECOD through CT codelist C66727
ds <- ds %>%
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "OTHERSP",
    tgt_var = "DSDECOD",
    id_vars = oak_id_vars()
  ) |>
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "IT.DSDECOD",
    tgt_var = "DSDECOD",
    ct_spec = study_ct,
    ct_clst = "C66727",
    id_vars = oak_id_vars()
  )

# ------------------------------------------------------------------------------
# 6) Derive DSCAT using business rules
# ------------------------------------------------------------------------------
# Business logic implemented:
#   - Randomized subjects -> "PROTOCOL MILESTONE"
#   - Non-randomized and OTHERSP missing -> "DISPOSITION EVENT"
#   - OTHERSP populated -> "OTHER EVENT"
ds <- ds %>%
  hardcode_no_ct(
    raw_dat = condition_add(ds_raw, IT.DSDECOD == "Randomized"),
    raw_var = "IT.DSDECOD",
    tgt_var = "DSCAT",
    tgt_val = "PROTOCOL MILESTONE",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    raw_dat = condition_add(ds_raw, IT.DSDECOD != "Randomized" & is.na(OTHERSP)),
    raw_var = "IT.DSDECOD",
    tgt_var = "DSCAT",
    tgt_val = "DISPOSITION EVENT",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    raw_dat = condition_add(ds_raw, !is.na(OTHERSP)),
    raw_var = "OTHERSP",
    tgt_var = "DSCAT",
    tgt_val = "OTHER EVENT",
    id_vars = oak_id_vars()
  ) %>%
  # Derive DSSTDTC from IT.DSSTDAT (date-level)
  assign_datetime(
    tgt_var = "DSSTDTC",
    raw_dat = ds_raw,
    raw_var = "IT.DSSTDAT",
    raw_fmt = "m-d-y",
    raw_unk = c("UN", "UNK")
  ) %>%
  # Derive DSDTC from date+time components (DSDTCOL + DSTMCOL)
  assign_datetime(
    tgt_var = "DSDTC",
    raw_dat = ds_raw,
    raw_var = c("DSDTCOL", "DSTMCOL"),
    raw_fmt = c("m-d-y", "H:M"),
    raw_unk = c("UN", "UNK")
  )

# ------------------------------------------------------------------------------
# 7) Controlled terminology remediation for VISIT/VISITNUM mapping
# ------------------------------------------------------------------------------
# Findings:
#   - "Ambul Ecg Removal" appears in raw but only "Ambul ECG Removal" exists in CT.
#   - Multiple "Unscheduled X.X" values appear in raw but are missing in CT.
#
# Exercise approach:
#   - Patch CT in-memory so mapping can proceed.
# Production approach:
#   - Raise terminology/data management issue and update governed CT artifacts.
#

# 7a) Add case-variant collected_value row for Ambul(atory) ECG visit
ecg_rows <- study_ct[study_ct$collected_value == "Ambul ECG Removal", ]
ecg_rows$collected_value <- "Ambul Ecg Removal"
study_ct <- rbind(study_ct, ecg_rows)

# 7b) Add missing unscheduled visit values based on existing "Unscheduled 3.1" template
unsch_row <- study_ct[study_ct$collected_value == "Unscheduled 3.1", ]

# Missing unscheduled visit numeric suffixes identified in raw data
vals <- c(6.1, 1.1, 5.1, 4.1, 8.2, 13.1)

sapply(vals, function(x) {
  # Update VISITNUM term
  unsch_row[unsch_row$term_code == "VISITNUM", ]$term_value <- paste0(x)
  # Update VISIT label
  unsch_row[unsch_row$term_code == "VISIT", ]$term_value <- paste0("UNSCHEDULED ", x)
  # Update raw collected value key used for mapping
  unsch_row$collected_value <- paste0("Unscheduled ", x)
  # Append to study CT
  study_ct <<- rbind(study_ct, unsch_row)
})

# ------------------------------------------------------------------------------
# 8) Map VISIT and VISITNUM from INSTANCE
# ------------------------------------------------------------------------------
ds <- ds %>%
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISIT",
    ct_spec = study_ct,
    ct_clst = "VISIT",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISITNUM",
    ct_spec = study_ct,
    ct_clst = "VISITNUM",
    id_vars = oak_id_vars()
  )

# ------------------------------------------------------------------------------
# 9) Finalize DS domain structure and required derivations
# ------------------------------------------------------------------------------
# Target variable order:
# STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT, VISITNUM,
# VISIT, DSDTC, DSSTDTC, DSSTDY
ds <- ds %>%
  dplyr::mutate(
    STUDYID = ds_raw$STUDY,
    DOMAIN = "DS",
    USUBJID = paste0("01-", ds_raw$PATNUM),
    # Standard SDTM convention: uppercase controlled text fields
    DSTERM = toupper(DSTERM),
    DSDECOD = toupper(DSDECOD),
    DSCAT = toupper(DSCAT),
    VISITNUM = VISITNUM,
    VISIT = VISIT,
    DSDTC = DSDTC,
    DSSTDTC = DSSTDTC
  ) %>%
  # Sequence within subject by chronological/event-defining keys
  derive_seq(
    tgt_var = "DSSEQ",
    rec_vars = c("USUBJID", "DSSTDTC", "DSDECOD")
  ) %>%
  # Relative study day using DM.RFSTDTC as reference start date
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "DSSTDTC",
    refdt = "RFSTDTC",
    study_day_var = "DSSTDY"
  ) %>%
  select(
    "STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM", "DSDECOD",
    "DSCAT", "VISITNUM", "VISIT", "DSDTC", "DSSTDTC", "DSSTDY"
  )

# ------------------------------------------------------------------------------
# 10) Save output
# ------------------------------------------------------------------------------
saveRDS(ds, "question_1_sdtm/ds.RDS")

