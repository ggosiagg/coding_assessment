# Load libraries & data -------------------------------------
library(dplyr)
library(gtsummary)
library(pharmaverseadam)
library(gt)
library(ggplot2)

adsl <- pharmaverseadam::adsl
adae <- pharmaverseadam::adae

# Pre-processing --------------------------------------------
adae <- adae |>
  filter(
    # safety population (it is not explicitly pointed in specs, but AE is usually 
    # performed on Safety population)
    SAFFL == "Y"
  )

# Plot 1
# Create a bar plot
ae_severity <- ggplot(adae, aes(x = TRT01A, fill = AESEV)) +
  geom_bar() +
  labs(
    y = "Count of AEs",
    x = "Treatment Arm",
    title = "AE severity distribution by treatment"
  )

# Save it to png file, used scale = 2 to properly render the plot 
# labels of treatment arm were overlapping with scale = 1
ggsave("question_3_tlg/ae_severity_bar_chart.png", plot = ae_severity, scale = 2)

# Plot 2
# Calculate most often AETERM, calculate an AETERM only ONCE per patient 
ae_terms <- adae %>%
  distinct(USUBJID, AETERM) %>%
  count(AETERM, sort = TRUE)

# Select top 10
top10_ae_terms <- ae_terms[1:10,]

# Add total number of patients to the df for traceability
top10_ae_terms$N <- n_distinct(adae$USUBJID)

# For each row calculate the 95% CI
vals <- lapply(seq_len(nrow(top10_ae_terms)), function(i) {
  x <- top10_ae_terms$n[i]
  N <- top10_ae_terms$N[i]
  
  # Calculate confidence intervals
  bin_test <- binom.test(x, N)
  
  # Multiple estimates by 100 to get ready to plot values
  data.frame(
    est = as.numeric(bin_test$estimate)*100,
    lower = as.numeric(bin_test$conf.int[1])*100,
    upper = as.numeric(bin_test$conf.int[2])*100
  )
})

# Merge all the lists into one data.frame
vals_df <- do.call(rbind, vals)

# Add the calculate CIs to the main top 10 data.frame
top10_ae_terms <- cbind(top10_ae_terms, vals_df)

# Convert the AETERM to factor to keep proper ordering
top10_ae_terms$AETERM <- factor(
  top10_ae_terms$AETERM,
  levels = rev(top10_ae_terms$AETERM)
)

top10_ae <- ggplot(top10_ae_terms, aes(x = est, y = AETERM)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.2) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0("n = ", n_distinct(adae$USUBJID), " subjects; 95% Clopper-Pearson CIs"),
    x = "Percentage of Patients (%)",
    y = NULL
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10)
  )

# Save it to png file, using scale = 2 to properly
# render the plot in png file
ggsave("question_3_tlg/top10_ae_chart.png", plot = top10_ae, scale=2)