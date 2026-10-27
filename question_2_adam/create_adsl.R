library(admiral)
library(dplyr, warn.conflicts = FALSE)
library(pharmaversesdtm)
library(lubridate)
library(stringr)

# Get SDTM domains from pharmaversedstm
dm <- pharmaversesdtm::dm
ex <- pharmaversesdtm::ex
vs <- pharmaversesdtm::vs
ae <- pharmaversesdtm::ae
ds <- pharmaversesdtm::ds

# Convert blank values to NAs
dm <- convert_blanks_to_na(dm)
ex <- convert_blanks_to_na(ex)
vs <- convert_blanks_to_na(vs)
ae <- convert_blanks_to_na(ae)
ds <- convert_blanks_to_na(ds)

# Start by assigning DM to ADSL, drop DOMAIN column
adsl <- dm %>%
  select(-DOMAIN)

# AGEGR9, AGEGR9N
# Create lookup tables to derive AGEGR9, AGEGR9N

# Assign values for AGEGR9 basing on AGE
agegr9_lookup <- exprs(
  ~condition,           ~AGEGR9,
  AGE < 18,               "<18",
  between(AGE, 18, 50), "18-50",
  AGE > 50,               ">50"
)

# Assign values for AGEGR9N basing on AGEGR9
agegr9n_lookup <- exprs(
  ~condition,           ~AGEGR9N,
  AGEGR9 == "<18",            1,
  AGEGR9 == "18-50",          2,
  AGEGR9 == ">50",            3
)

# Derive AGEGR9, AGEGR9N
adsl <- adsl %>%
  derive_vars_cat(
    definition = agegr9_lookup
  ) %>%
  derive_vars_cat(
    definition = agegr9n_lookup
  )

# TRTSDTM,TRTSTMF
# Check for the Start Date in EX Domain
ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    # Impute only starting from hours, date should not be imputed
    highest_imputation = "h",
    # Make sure to impute as per specification 00:00:00, this time value will be not printed 
    # in the data.frame
    time_imputation = "00:00:00",
    # This ensures that even if the seconds are imputed there is no imputation Flag
    ignore_seconds_flag = FALSE,
    new_vars_prefix = "EXST"
  )

# Derive TRTSDTM, TRTSTMF for valid doses
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    # Valid dose is if EXDOSE > 0
    filter_add = (EXDOSE > 0 |
                    # or EXDOSE == 0 and EXTRT contains "PLACEBO"
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  )

# ITTFL
adsl <- adsl %>%
  mutate(
    ITTFL = if_else(is.na(ARM), "N", "Y")
  )

# I derived ITTFL as was specified in the specs
# however, I think we should not assign ITTFL for 
# patients with ARM = "Screen Failure", in my opoinion
# specs should be updated

# LSTAVLDT
# Because of point (4) of the specification for LSTAVLDT
# (4) last date of treatment administration where patient received a valid dose 
# (datepart of Datetime of Last Exposure to Treatment
# [ADSL.TRTEDTM]). First, I need to derive TRTEDTM, I will do it in the same way 
# as I was asked to derive TRTSDTM, taking the last date instead of the first

# Check for the Start Date in EX Domain
ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    # Impute only starting from hours, date should not be imputed
    highest_imputation = "h",
    # Make sure to impute as per specification 23:59:59, this time value will be not printed 
    # in the data.frame
    time_imputation = "23:59:59",
    # This ensures that even if the seconds are imputed there is no imputation Flag
    ignore_seconds_flag = FALSE,
    new_vars_prefix = "EXEN"
  )

# Derive TRTEDTM, TRTETMF for valid doses
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    # Valid dose is if EXDOSE > 0
    filter_add = (EXDOSE > 0 |
                    # or EXDOSE == 0 and EXTRT contains "PLACEBO"
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))),
    new_vars = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  )

# Derive LSTAVLDT
adsl <- adsl %>%
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    events = list(
      # (1) last complete date of vital assessment with a valid test result
      # ([VS.VSSTRESN] and [VS.VSSTRESC] not both missing) and datepart of [VS.VSDTC] not missing
      event(
        dataset_name = "vs",
        order = exprs(VSDTC, VSSEQ),
        condition = !is.na(VSSTRESN) & !is.na(VSSTRESC) & !is.na(VSDTC) & nchar(VSDTC) >= 9,
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(VSDTC, highest_imputation = "n"),
          seq = VSSEQ
        )
      ),
      # (2) last complete onset date of AEs (datepart of Start Date/Time of Adverse Event [AE.AESTDTC]).
      event(
        dataset_name = "ae",
        order = exprs(AESTDTC, AESEQ),
        condition = !is.na(AESTDTC) & nchar(AESTDTC) >= 9,
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(AESTDTC, highest_imputation = "n"),
          seq = AESEQ
        ),
      ),
      # (3) last complete disposition date (datepart of Start Date/Time of Disposition Event [DS.DSSTDTC]).
      event(
        dataset_name = "ds",
        order = exprs(DSSTDTC, DSSEQ),
        condition = !is.na(DSSTDTC) & nchar(DSSTDTC) >= 9,
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(DSSTDTC, highest_imputation = "n"),
          seq = DSSEQ
        ),
      ),
      # (4) last date of treatment administration where patient received a valid dose (datepart of Datetime of Last Exposure to Treatment
      # [ADSL.TRTEDTM]).
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
    # Order
    order = exprs(LSTALVDT, seq, event_nr),
    # Take the last meaning - maximun date
    mode = "last",
    new_vars = exprs(LSTALVDT)
  )

# Save ADSL to an RDS file
saveRDS(adsl,"question_2_adam/adsl.RDS")