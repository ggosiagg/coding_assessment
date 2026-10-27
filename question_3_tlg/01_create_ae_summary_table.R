# Load libraries & data -------------------------------------
library(dplyr)
library(gtsummary)
library(pharmaverseadam)
library(gt)

adsl <- pharmaverseadam::adsl
adae <- pharmaverseadam::adae

# Pre-processing --------------------------------------------
adae <- adae |>
  filter(
    # safety population (it is not explicitly pointed in specs, but AE is usually 
    # performed on Safety population)
    SAFFL == "Y",
    # treatment-emergent adverse events
    TRTEMFL == "Y"
  )

# Generate the table
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

# Save the generated table to HTML 
tbl |> 
  as_gt() |>
  gt::gtsave(filename = "question_3_tlg/ae_summary_table.html")