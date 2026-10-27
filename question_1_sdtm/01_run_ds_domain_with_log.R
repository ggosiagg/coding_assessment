# question_1_sdtm/01_run_ds_domain_with_log.R
# Runner: executes 01_create_ds_domain.R and captures all console output to a log

log_file <- "question_1_sdtm/01_create_ds_domain.log"
log_con  <- file(log_file, open = "wt")

sink(log_con, split = TRUE)    # output to console AND log
sink(log_con, type = "message") # capture messages/warnings too

cat("=== Log: 01_create_ds_domain.R ===\n")
cat("Start:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

status <- tryCatch(
  {
    source("question_1_sdtm/01_create_ds_domain.R", echo = TRUE)
    cat("\n--- Program completed successfully ---\n")
    0L
  },
  error = function(e) {
    cat("\n--- ERROR:", conditionMessage(e), "---\n")
    1L
  }
)

cat("\nEnd:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
print(sessionInfo())

sink(type = "message")
sink()
close(log_con)

quit(status = status)
