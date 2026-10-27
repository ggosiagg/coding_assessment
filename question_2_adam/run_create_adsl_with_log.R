# ==============================================================================
# Author      : ggosiagg
# Created on  : 2026-10-27
# File        : question_2_adam/run_create_adsl_with_log.R
# Purpose     : Runner that executes create_adsl.R and captures all console
#               output, messages, warnings and errors into a log file using
#               base R (sink), for traceability.
# Usage       : Rscript question_2_adam/run_create_adsl_with_log.R
# ==============================================================================

# ------------------------------------------------------------------------------
# 1) Open log connection and redirect output (console + log simultaneously)
# ------------------------------------------------------------------------------
log_file <- "question_2_adam/create_adsl.log"
log_con  <- file(log_file, open = "wt")

sink(log_con, split = TRUE)       # standard output: console AND log
sink(log_con, type = "message")   # also capture messages/warnings/errors

cat("=== Log: create_adsl.R ===\n")
cat("Start:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ------------------------------------------------------------------------------
# 2) Execute the ADSL program, echoing all code into the log
# ------------------------------------------------------------------------------
status <- tryCatch(
  {
    source("question_2_adam/create_adsl.R", echo = TRUE)
    cat("\n--- Program completed successfully ---\n")
    0L
  },
  error = function(e) {
    cat("\n--- ERROR:", conditionMessage(e), "---\n")
    1L
  }
)

# ------------------------------------------------------------------------------
# 3) Footer: timestamp + session info, then close log and exit with status
# ------------------------------------------------------------------------------
cat("\nEnd:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
print(sessionInfo())

sink(type = "message")
sink()
close(log_con)

quit(status = status)