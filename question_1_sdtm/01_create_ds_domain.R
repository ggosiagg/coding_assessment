library(sdtm.oak)
library(pharmaverseraw)
library(pharmaversesdtm)
library(dplyr)

# Read in raw data from pharamverse
ds_raw <- pharmaverseraw::ds_raw

# Assign oak_id
ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

# Read-in controlled terminology file
study_ct <- read.csv("inst/data/sdtm_ct.csv")

# Mapping the topic variable DSTERM
ds <-
  # If OTHERSP is not null then assign OTHERSP to DSTERM
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "OTHERSP",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  ) |>
  # Otherwise assign IT.DSTERM TO DSTERM
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "IT.DSTERM",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  )

# Other variables
ds <- ds %>%
  # If OTHERSP is not null then assign OTHERSP to DSDECOD
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "OTHERSP",
    tgt_var = "DSDECOD",
    id_vars = oak_id_vars()
  ) |>
  # Otherwise assign IT.DESDECOD to DSDECOD
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "IT.DSDECOD",
    tgt_var = "DSDECOD",
    ct_spec = study_ct,
    ct_clst = "C66727",
    id_vars = oak_id_vars()
  ) 

# Some of the values are not in the controlled terminology list
# However they come straight from the eCRF

ds <- ds %>%
  # For patients with IT.DSDECOD randomized set DSCAT to PROTOCOL MILESTONE
  hardcode_no_ct(
    raw_dat = condition_add(ds_raw, IT.DSDECOD == "Randomized"),
    raw_var = "IT.DSDECOD",
    tgt_var = "DSCAT",
    tgt_val = "PROTOCOL MILESTONE",
    id_vars = oak_id_vars()
  ) %>%
  # For other patients with missing OTHERSP set DSCAT to DISPOSTION EVENT
  hardcode_no_ct(
    raw_dat = condition_add(ds_raw, IT.DSDECOD != "Randomized" & is.na(OTHERSP)),
    raw_var = "IT.DSDECOD",
    tgt_var = "DSCAT",
    tgt_val = "DISPOSITION EVENT",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    # For patients with non-missing OTHERSP set DSCAT to OTHER EVENT
    raw_dat = condition_add(ds_raw, !is.na(OTHERSP)),
    raw_var = "OTHERSP",
    tgt_var = "DSCAT",
    tgt_val = "OTHER EVENT",
    id_vars = oak_id_vars()
  ) %>%
  # Set start date
  assign_datetime(
    tgt_var = "DSSTDTC",
    raw_dat = ds_raw,
    raw_var = "IT.DSSTDAT",
    raw_fmt = "m-d-y",
    raw_unk = c("UN", "UNK")
  ) %>%
  # Set start datetime
  assign_datetime(
    tgt_var = "DSDTC",
    raw_dat = ds_raw,
    raw_var = c("DSDTCOL","DSTMCOL"),
    raw_fmt = c("m-d-y","H:M"),
    raw_unk = c("UN", "UNK")
  ) 

# Noticed by the user: For VISIT and VISITNUM findings vs. study_ct
# Controlled terminology needs to be updated!
# 'Ambul Ecg Removal' is found in the raw data, but 'Ambul ECG Removal' is in CT
# Also Unscheduled VISITS that are in the ds_raw are not in the study_ct
# For the purpose of this exercise I will update the controlled terminology data.frame
# But in real setup we would need to circle back to the owner of the terminology for necessary
# updates or go back to data management team to look into it

# Adding the Ambul Ecg Removal with different casing
ecg_rows <- study_ct[study_ct$collected_value=="Ambul ECG Removal",]
ecg_rows$collected_value <- "Ambul Ecg Removal"
study_ct <- rbind(study_ct,ecg_rows)

# Adding the rows for other unscheduled visits
unsch_row <- study_ct[study_ct$collected_value=="Unscheduled 3.1",]
# Specify missing values for unscheduled visitis
vals <- c(6.1, 1.1, 5.1, 4.1, 8.2, 13.1)

sapply(vals, function(x){
  # Add proper values for unscheduled visits
  unsch_row[unsch_row$term_code=="VISITNUM",]$term_value <- paste0(x)
  unsch_row[unsch_row$term_code=="VISIT",]$term_value <- paste0("UNSCHEDULED ", x)
  unsch_row$collected_value <- paste0("Unscheduled ", x)
  # Add the new rows and overwrite already existing study_ct dataframe
  study_ct <<- rbind(study_ct, unsch_row)
})

ds <- ds %>%
  # Map VISIT from INSTANCE using assign_ct
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISIT",
    ct_spec = study_ct,
    ct_clst = "VISIT",
    id_vars = oak_id_vars()
  ) %>%
  # Map VISITNUM from INSTANCE using assign_ct
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISITNUM",
    ct_spec = study_ct,
    ct_clst = "VISITNUM",
    id_vars = oak_id_vars()
  )

# Finalize DS creation

#STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT, VISITNUM, VISIT, DSDTC, DSSTDTC, DSSTDY

ds <- ds %>%
  dplyr::mutate(
    STUDYID = ds_raw$STUDY,
    DOMAIN = "DS",
    USUBJID = paste0("01-", ds_raw$PATNUM),
    DSTERM = toupper(DSTERM),
    DSDECOD = toupper(DSDECOD),
    DSCAT = toupper(DSCAT),
    VISITNUM = VISITNUM,
    VISIT = VISIT,
    DSDTC = DSDTC,
    DSSTDTC = DSSTDTC
  ) %>%
  derive_seq(
    tgt_var = "DSSEQ",
    rec_vars = c("USUBJID", "DSSTDTC", "DSDECOD")
  ) %>%
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "DSSTDTC",
    refdt = "RFSTDTC",
    study_day_var = "DSSTDY"
  ) %>%
  select("STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM", "DSDECOD", 
         "DSCAT", "VISITNUM", "VISIT", "DSDTC", "DSSTDTC", "DSSTDY")

# Saving DS domain to sdtm folder
saveRDS(ds,"question_1_sdtm/ds.RDS")