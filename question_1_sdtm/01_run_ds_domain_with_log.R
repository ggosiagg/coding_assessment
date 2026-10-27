# Runner: executes 01_create_ds_domain.R and writes a pharmaverse logrx log file
# Run from the repo root: Rscript question_1_sdtm/01_run_ds_domain_with_log.R

library(logrx)

axecute(
  file     = "question_1_sdtm/01_create_ds_domain.R",
  log_name = "question_1_sdtm/01_create_ds_domain.log"
)
