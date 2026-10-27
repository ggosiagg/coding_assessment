# ==============================================================================
# Author      : ggosiagg
# Created on  : 2026-10-27
# File        : question_3_tlg/01_create_ae_summary_table.R
# Purpose     : Build a TLG AE summary table: hierarchical counts of
#               treatment-emergent adverse events (TEAEs) by System Organ Class
#               (AESOC) and Preferred Term (AETERM), split by planned treatment
#               (TRT01A), using gtsummary and ADaM input data (ADSL, ADAE).
#
# High-level steps implemented in this script:
#   1) Load required packages and ADaM source datasets (ADSL, ADAE).
#   2) Pre-process ADAE: restrict to the safety population (SAFFL == "Y") and
#      treatment-emergent adverse events (TRTEMFL == "Y").
#   3) Generate a hierarchical summary table (AESOC / AETERM) by TRT01A,
#      counting unique subjects (id = USUBJID) with ADSL as the denominator.
#   4) Export the rendered table to HTML via gt.
#
# Key findings to report:
#   - Population flags: specs do not explicitly state the analysis population;
#     safety population (SAFFL == "Y") was assumed, as is standard for AE
#     summaries. TEAE restriction follows TRTEMFL == "Y".
#   - tbl_hierarchical() counts unique subjects per SOC/PT level using
#     id = USUBJID, so subjects with multiple events within a SOC/PT are
#     counted once at that level.
#   - denominator = adsl ensures percentages are computed against the full
#     safety population per treatment arm, not just subjects with AEs.
#   - An overall row ("Treatment Emergent AEs") is included via
#     overall_row = TRUE and relabelled with the
#     "..ard_hierarchical_overall.." label argument.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1) Load libraries & data
# ------------------------------------------------------------------------------
library(dplyr)
library(gtsummary)
library(pharmaverseadam)
library(gt)

# Source ADaM datasets from pharmaverseadam package
adsl <- pharmaverseadam::adsl
adae <- pharmaverseadam::adae

# ------------------------------------------------------------------------------
# 2) Pre-process ADAE: safety population + treatment-emergent AEs
# ------------------------------------------------------------------------------
adae <- adae |>
  filter(
    # safety population (it is not explicitly pointed in specs, but AE is usually
    # performed on Safety population)
    SAFFL == "Y",
    # treatment-emergent adverse events
    TRTEMFL == "Y"
  )

# ------------------------------------------------------------------------------
# 3) Generate the hierarchical AE summary table
# ------------------------------------------------------------------------------
tbl <- adae |>
  tbl_hierarchical(
    # Select the System Orgen Class, and Term
    variables = c(AESOC, AETERM),
    by = TRT01A,
    id = USUBJID,
    denominator = adsl,
    overall_row = TRUE,
    label = "..ard_hierarchical_overall.." ~ "Treatment Emergent AEs"
  )

# ------------------------------------------------------------------------------
# 4) Save output
# ------------------------------------------------------------------------------
# Save the generated table to HTML
tbl |>
  as_gt() |>
  gt::gtsave(filename = "question_3_tlg/ae_summary_table.html")
