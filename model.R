################################
# 1. LOAD LIBRARIES & FILE PATHS
################################
library(Synth)
library(readr)
library(dplyr)
library(tidyr)
library(SCtools)
library(ggplot2)
library(tidyverse)
library(reshape2)

# Define file paths for each dataset
file_paths <- list(
  fertility_rate   = "data/Fertility rate, total (births per woman).csv",
  gdp_growth       = "data/GDP per capita growth (annual %).csv",
  urban_pop_growth = "data/Urban population growth (annual %).csv",
  unemployment     = "data/Unemployment, total (% of total labor force) (national estimate).csv",
  female_labor     = "data/Labor force, female (% of total labor force).csv",
  age_dependency   = "data/Age dependency ratio (% of working-age population).csv",
  female_male_ratio= "data/Ratio of female to male labor force participation rate (%) (national estimate).csv",
  net_migration    = "data/Net migration.csv"
)

##################
# 2. LOAD DATASETS
##################
# Read each CSV file
fertility_rate   <- read_csv(file_paths$fertility_rate)
gdp_growth       <- read_csv(file_paths$gdp_growth)
urban_pop_growth <- read_csv(file_paths$urban_pop_growth)
unemployment     <- read_csv(file_paths$unemployment)
female_labor     <- read_csv(file_paths$female_labor)
age_dependency   <- read_csv(file_paths$age_dependency)
female_male_ratio<- read_csv(file_paths$female_male_ratio)
net_migration    <- read_csv(file_paths$net_migration)

# DATA PREP FOR PAPER
fertility_long <- fertility_rate %>%
  pivot_longer(
    cols = -c(`Country Name`, `Country Code`, `Indicator Name`, `Indicator Code`),
    names_to = "Year",
    values_to = "TFR"
  ) %>%
  mutate(Year = as.numeric(Year))

# Potential Donor Pool Countries
countries <- c("Poland", "Armenia", "Austria", "Azerbaijan", "Belarus", "Bulgaria",
               "Czechia", "Estonia", "France", "Germany", "Georgia", "Greece",
               "Ireland", "Iceland", "Italy", "Kazakhstan", "Latvia",
               "Lithuania", "Moldova", "Romania", "Russian Federation", "Spain",
               "Sweden", "Turkiye", "Ukraine", "Uzbekistan", "Hungary",
               "Slovakia", "Albania", "Slovenia", "Bosnia and Herzegovina",
               "Croatia", "North Macedonia", "Montenegro", "Serbia", "Slovak Republic",
               "United Kingdom", "Netherlands", "Belgium", "Portugal", "Switzerland", 
               "Denmark", "Finland", "Norway", "Cyprus", "Malta", "Luxembourg", "Kosovo",
               "Kyrgyz Republic", "Tajikistan", "Turkmenistan"
              )

# Filter the dataset for the post-intervention period
fertility_filtered <- fertility_long %>%
  filter(`Country Name` %in% countries, Year >= 2007, Year <= 2022)

# Summarize to compute the lowest and highest TFR for each country and add row numbers
fertility_summary <- fertility_filtered %>%
  group_by(`Country Name`) %>%
  summarize(
    min_TFR = min(TFR, na.rm = TRUE),
    max_TFR = max(TFR, na.rm = TRUE)
  ) %>%
  mutate(TFR_range = paste0("TFR ~", round(min_TFR, 2), " - ", round(max_TFR, 2))) %>%
  ungroup() %>%
  mutate(Row = row_number()) %>%                # Add row numbers here
  select(Row, `Country Name`, TFR_range)         # Reorder to have row numbers as the leftmost column

# Print the results
print(fertility_summary)

# Save the summarized results to a CSV file
write_csv(fertility_summary, "outputs/fertility_summary.csv")


# RELIGION

# Define the file path for the religion breakdown CSV file
religion_file <- "data/religous-composition.csv"

# Read the CSV file
religion_data <- read_csv(religion_file)

# Define the list of countries to include
countries <- c("Georgia", "Poland", "Albania", "Armenia", "Austria", "Azerbaijan",
               "Belarus", "Bosnia-Herzegovina", "Bulgaria", "Croatia", "Czech Republic",
               "Estonia", "France", "Germany", "Greece", "Hungary", "Iceland",
               "Ireland", "Italy", "Kazakhstan", "Latvia", "Lithuania", "Moldova",
               "Montenegro", "Republic of Macedonia", "Romania", "Russia", "Serbia",
               "Slovakia", "Slovenia", "Spain", "Sweden", "Turkey", "Ukraine",
               "Uzbekistan", "United Kingdom", "Netherlands", "Belgium", "Portugal", "Switzerland", 
               "Denmark", "Finland", "Norway", "Cyprus", "Malta", "Luxembourg", "Kosovo",
               "Kyrgyzstan", "Tajikistan", "Turkmenistan"
               )

# Filter the data for the year 2010 and the specified countries
religion_filtered <- religion_data %>%
  filter(Year == 2010, Country %in% countries)

# Create a formatted breakdown string for each country.
religion_formatted <- religion_filtered %>%
  mutate(
    Breakdown = paste0(
      Christians, "% Christians", "\n",
      Muslims, "% Muslims", "\n",
      Unaffiliated, "% Unaffiliated", "\n",
      Hindus, "% Hindus", "\n",
      Buddhists, "% Buddhists", "\n",
      `Folk Religions`, "% Folk Religions", "\n",
      `Other Religions`, "% Other Religions", "\n",
      Jews, "% Jews"
    )
  )

religion_final <- religion_formatted %>%
  mutate(Row = row_number()) %>%
  select(Row, Country, Breakdown)

# View the output in the console
print(religion_final)

# Save as CSV
write_csv(religion_final, "outputs/religious_composition_2010.csv")

#########################################
# 3. DEFINE DONOR POOL & HELPER FUNCTIONS
#########################################
donor_countries <- c(
                    "Albania",
                    "Croatia",
                    "Cyprus",
                    # "Kosovo", Not enough data
                    "Malta",
                    "Moldova",
                    "North Macedonia",
                    "Slovak Republic",
                    "Slovenia",
                    "Turkiye",
                    "Azerbaijan",
                    "Belgium",
                    "Bulgaria",
                    "Denmark",
                    "France",
                    "Italy",
                    "Latvia",
                    "Netherlands",
                    "Portugal",
                    "Serbia",
                    "Switzerland",
                    "Ukraine"
                    )

# Function: Filter & clean each dataset for countries of interest
clean_and_filter_data <- function(data, countries) {
  data %>%
    select(`Country Name`, starts_with("19"), starts_with("20")) %>%
    filter(`Country Name` %in% c("Georgia", countries))
}

# Function: Reshape data to long format and clean NAs in the outcome variable
prepare_scm_data <- function(df, outcome_var) {
  df %>%
    pivot_longer(cols = starts_with("19") | starts_with("20"),
                 names_to = "Year", values_to = outcome_var) %>%
    mutate(Year = as.numeric(Year)) %>%
    filter(!is.na(.data[[outcome_var]]))
}

###############################
# 4. CLEAN & RESHAPE DATASETS
###############################
# Clean each dataset using the helper function
fertility_rate_filtered   <- clean_and_filter_data(fertility_rate, donor_countries)
gdp_growth_filtered       <- clean_and_filter_data(gdp_growth, donor_countries)
urban_pop_growth_filtered <- clean_and_filter_data(urban_pop_growth, donor_countries)
unemployment_filtered     <- clean_and_filter_data(unemployment, donor_countries)
female_labor_filtered     <- clean_and_filter_data(female_labor, donor_countries)
age_dependency_filtered   <- clean_and_filter_data(age_dependency, donor_countries)
female_male_ratio_filtered<- clean_and_filter_data(female_male_ratio, donor_countries)
net_migration_filtered    <- clean_and_filter_data(net_migration, donor_countries)

# Reshape each dataset into long format
fertility_rate_long    <- prepare_scm_data(fertility_rate_filtered, "Fertility_Rate")
gdp_growth_long        <- prepare_scm_data(gdp_growth_filtered, "GDP_Per_Capita_Growth")
urban_pop_growth_long  <- prepare_scm_data(urban_pop_growth_filtered, "Urban_Population_Growth")
unemployment_long      <- prepare_scm_data(unemployment_filtered, "Unemployment_Rate")
female_labor_long      <- prepare_scm_data(female_labor_filtered, "Female_Labor_Force")
age_dependency_long    <- prepare_scm_data(age_dependency_filtered, "Age_Dependency_Ratio")
female_male_ratio_long <- prepare_scm_data(female_male_ratio_filtered, "Female_Male_Labor_Ratio")
net_migration_long     <- prepare_scm_data(net_migration_filtered, "Net_Migration")

########################################
# 5. MERGE DATASETS & ADD COUNTRY CODES
########################################
# Merge all covariates by Country Name and Year
merged_data <- fertility_rate_long %>%
  left_join(gdp_growth_long,       by = c("Country Name", "Year")) %>%
  left_join(urban_pop_growth_long, by = c("Country Name", "Year")) %>%
  left_join(unemployment_long,     by = c("Country Name", "Year")) %>%
  left_join(female_labor_long,     by = c("Country Name", "Year")) %>%
  left_join(age_dependency_long,   by = c("Country Name", "Year")) %>%
  left_join(female_male_ratio_long,by = c("Country Name", "Year")) %>%
  left_join(net_migration_long,      by = c("Country Name", "Year"))

# Create a numeric identifier for countries (Georgia = 1)
country_codes <- data.frame(
  `Country Name` = c("Georgia", donor_countries),
  Country_Code   = 1:(length(donor_countries) + 1)
)

# Rename 'Country.Name' to 'Country Name' in the country_codes dataframe
country_codes <- country_codes %>%
  rename(`Country Name` = `Country.Name`)

# Merge country codes into the data
merged_data <- merged_data %>%
  left_join(country_codes, by = "Country Name") %>%
  mutate(Country_Code = as.numeric(Country_Code)) %>%
  as.data.frame()  # Ensure data is a data.frame

###############################
# 6. SYNTHETIC CONTROL PREPARATION
###############################
# Set parameters for the synthetic control method
treatment_id <- 1      # Georgia
control_ids  <- 2:(length(donor_countries) + 1)
pre_period   <- 1960:2007
full_period  <- 1960:2022

# Run dataprep and synth functions that trigger warnings
dataprep.out <- dataprep(
  foo = merged_data,
  predictors         = c("GDP_Per_Capita_Growth", 
                         #"Urban_Population_Growth",
                         "Unemployment_Rate", 
                         "Female_Labor_Force",
                         "Age_Dependency_Ratio"
                         #"Net_Migration"
                         ),
  predictors.op      = "mean",
  dependent          = "Fertility_Rate",
  unit.variable      = "Country_Code",
  time.variable      = "Year",
  treatment.identifier = treatment_id,
  controls.identifier  = control_ids,
  time.predictors.prior = pre_period,
  time.optimize.ssr    = pre_period,
  unit.names.variable  = "Country Name",
  time.plot            = full_period
)

synth.out <- synth(dataprep.out, optimxmethod = "All", quadopt="ipop")

########################################
# 7. PLOT TREATED VS. SYNTHETIC GEORGIA
########################################
path.plot(dataprep.res = dataprep.out, synth.res = synth.out,
          Ylab = "Fertility Rate (Births per Woman)",
          Ylim = c(0, 5))
abline(v = 2007, col = "black", lty = 2, lwd = 2)

###############################
# 8. PLACEBO STUDIES: IN-SPACE
###############################
# Define variables for placebo in-space
dependent_var <- "Fertility_Rate"
predictors    <- c("GDP_Per_Capita_Growth", "Urban_Population_Growth",
                   "Unemployment_Rate", "Female_Labor_Force",
                   "Age_Dependency_Ratio", "Net_Migration")
treatment_time <- 2007

# Run multiple synthetic control analysis for the treated unit and controls
res <- multiple.synth(
  foo                  = merged_data,
  predictors           = predictors,
  predictors.op        = "mean",
  dependent            = dependent_var,
  unit.variable        = "Country_Code",
  time.variable        = "Year",
  special.predictors   = NULL,
  treated.units        = treatment_id,
  control.units        = control_ids,
  time.predictors.prior= pre_period,
  time.optimize.ssr    = pre_period,
  unit.names.variable  = "Country Name",
  time.plot            = full_period,
  treatment.time       = treatment_time,
  gen.placebos         = FALSE,
  strategy             = "sequential",
  Sigf.ipop            = 5
)

# Generate and plot placebos using the multicore strategy
placebo <- generate.placebos(dataprep.out, synth.out, Sigf.ipop = 2, strategy = 'multicore')

p0  <- plot_placebos(placebo, discard.extreme = FALSE, xlab = 'Year') +
  ggtitle("Placebo Results (No Limit)")
p1 <- plot_placebos(placebo, discard.extreme = TRUE, mspe.limit = 30, xlab = 'Year') +
  ggtitle("Placebo Results (MSPE Limit = 30x)")

p0  + scale_color_manual(name = "", values = c("black", "grey"),
                              labels = c("Georgia", "Control units"))
p1 + scale_color_manual(name = "", values = c("black", "grey"),
                              labels = c("Georgia", "Control units"))

# Compute and print MSPE ratio and p-value
ratio   <- mspe.test(placebo)
p_value <- ratio$p.val
print(paste("Placebo in-space p-value:", p_value))
print(ratio$test)

##################################
# 9. PRE-TREATMENT MSPE COMPARISON
##################################

# Extract Georgia's observed and synthetic values for the pre-treatment period
observed_georgia <- dataprep.out$Y1plot[which(dataprep.out$tag$time.plot %in% pre_period)]
synthetic_georgia <- (dataprep.out$Y0plot %*% synth.out$solution.w)[which(dataprep.out$tag$time.plot %in% pre_period)]

# Compute pre-treatment MSPE for Georgia
mspe_georgia <- mean((observed_georgia - synthetic_georgia)^2, na.rm = TRUE)

# Extract donor MSPEs
donor_mspe <- unlist(placebo$mspe.placs)

# Extract donor country names correctly
donor_units <- dataprep.out$tag$controls.identifier  # Unit numbers of donor countries
donor_country_names <- dataprep.out$names.and.numbers$unit.names[
  match(donor_units, dataprep.out$names.and.numbers$unit.numbers)
]

# Compute how much larger each donor unit's MSPE is relative to Georgia's
mspe_comparison <- data.frame(
  Country = donor_country_names,
  Pre_Treatment_MSPE = donor_mspe,
  Relative_to_Georgia = donor_mspe / mspe_georgia  # Compute ratio
)

# Print results
cat("Pre-Treatment MSPE for Georgia:", mspe_georgia, "\n\n")
cat("Comparison of donor MSPEs relative to Georgia:\n")
print(mspe_comparison)

# Plot the comparison
ggplot(mspe_comparison, aes(x = reorder(Country, Relative_to_Georgia), y = Relative_to_Georgia)) +
  geom_bar(stat = "identity", fill = "grey50") +
  coord_flip() +
  labs(title = "Pre-Treatment MSPE Comparison: Donor Units vs. Georgia",
       x = "Country",
       y = "MSPE Ratio (Donor / Georgia)") +
  scale_y_continuous(breaks = seq(0, max(mspe_comparison$Relative_to_Georgia), by = 25)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Print countries and count of countries above and below an MSPE limit
# Define threshold value
mspe_limit <- 30

# Filter countries based on the threshold
above_limit <- mspe_comparison %>% filter(Relative_to_Georgia > mspe_limit)
below_limit <- mspe_comparison %>% filter(Relative_to_Georgia <= mspe_limit)

# Count the number of countries in each category
count_above <- nrow(above_limit)
count_below <- nrow(below_limit)

# Print results
cat("\nCountries with Relative_to_Georgia ABOVE", mspe_limit, "(", count_above, "countries ):\n")
print(above_limit)

cat("\nCountries with Relative_to_Georgia BELOW or EQUAL to", mspe_limit, "(", count_below, "countries ):\n")
print(below_limit)

############################################
# 10. MSPE RATIOS (SCATTER PLOT + HISTOGRAM)
############################################
# Scatter Plot
mspe_plot(placebo)

# Extract and convert MSPE ratios to numeric
mspe_values <- as.numeric(as.character(unlist(ratio$test)))
mspe_data   <- data.frame(MSPE_Ratio = mspe_values)

# Store Georgia's MSPE ratio
georgia_mspe <- ratio$test$MSPE.ratios[22]

# Create the histogram and conditionally fill the bin that contains georgia_mspe in red
ggplot(mspe_data, aes(x = MSPE_Ratio)) +
  geom_histogram(aes(fill = ifelse(..xmin.. <= georgia_mspe & ..xmax.. > georgia_mspe, "red", "white")),
                 bins = 30, color = "black", alpha = 0.7) +
  scale_fill_identity() +
  labs(title = "Histogram of MSPE Ratios",
       x = "MSPE Ratio", y = "Frequency") +
  theme_minimal()

###############################
# 11. PLACEBO STUDIES: IN-TIME
###############################
# Define placebo treatment years (pre-treatment years)
placebo_years        <- 2000:2006
actual_treatment_year<- 2007

# List to store placebo results by year
placebo_results <- list()

for (py in placebo_years) {
  # Update dataprep using data up to one year before the placebo year
  dp_placebo <- dataprep(
    foo = merged_data,
    predictors         = c("GDP_Per_Capita_Growth", "Urban_Population_Growth",
                           "Unemployment_Rate", "Female_Labor_Force",
                           "Age_Dependency_Ratio", "Net_Migration"),
    predictors.op      = "mean",
    dependent          = "Fertility_Rate",
    unit.variable      = "Country_Code",
    time.variable      = "Year",
    treatment.identifier = treatment_id,
    controls.identifier  = control_ids,
    time.predictors.prior = 1960:(py - 1),
    time.optimize.ssr    = 1960:(py - 1),
    unit.names.variable  = "Country Name",
    time.plot            = full_period
  )
  
  # Run synthetic control for this placebo year
  synth_placebo <- synth(dp_placebo)
  placebo_results[[as.character(py)]] <- list(dataprep = dp_placebo, synth = synth_placebo)
  
  # Plot the placebo results
  path.plot(dataprep.res = dp_placebo, synth.res = synth_placebo,
            Ylab = "Fertility Rate (Placebo)", Xlab = "Year",
            Legend = c("Treated", "Synthetic"), Ylim = c(0, 5))
  abline(v = py, col = "blue", lty = 2, lwd = 2)
}

#############################################
# 12. LEAVE-ONE-OUT CROSS VALIDATION (LOOCV)
#############################################
loocv_results <- list()

for (country_code in control_ids) {
  # Exclude one donor country from the control pool
  loocv_controls <- setdiff(control_ids, country_code)
  
  dp_loocv <- dataprep(
    foo = merged_data,
    predictors         = c("GDP_Per_Capita_Growth", "Urban_Population_Growth",
                           "Unemployment_Rate", "Female_Labor_Force",
                           "Age_Dependency_Ratio", "Net_Migration"),
    predictors.op      = "mean",
    dependent          = "Fertility_Rate",
    unit.variable      = "Country_Code",
    time.variable      = "Year",
    treatment.identifier = treatment_id,
    controls.identifier  = loocv_controls,
    time.predictors.prior = pre_period,
    time.optimize.ssr    = pre_period,
    unit.names.variable  = "Country Name",
    time.plot            = full_period
  )
  
  synth_loocv <- synth(dp_loocv)
  # Get the name of the excluded country
  excluded_name <- merged_data$`Country Name`[merged_data$Country_Code == country_code][1]
  loocv_results[[excluded_name]] <- list(dataprep = dp_loocv, synth = synth_loocv)
  
#   # Plot LOOCV result
#   path.plot(dataprep.res = dp_loocv, synth.res = synth_loocv,
#             Ylab = "Fertility Rate", Xlab = "Year",
#             Legend = c("Treated", "Synthetic"),
#             Main = paste("LOOCV: Excluding", excluded_name),
#             Ylim = c(0, 5))
#   abline(v = 2007, col = "red", lty = 2, lwd = 2)
  }

########################
# 13. NA VALUES ANALYSIS
########################
# Reshape data for NA count per country and variable
na_by_country_variable <- merged_data %>%
  pivot_longer(cols = -`Country Name`, names_to = "Variable", values_to = "Value") %>%
  group_by(`Country Name`, Variable) %>%
  summarise(
    NA_Count     = sum(is.na(Value)),
    Total_Entries= n(),
    NA_Proportion= (NA_Count / Total_Entries) * 100,
    .groups      = "drop"
  ) %>%
  arrange(desc(NA_Proportion))

# Display and save the NA breakdown
print("Missing Values Breakdown by Country and Variable:")
print(na_by_country_variable)
write.csv(na_by_country_variable, "outputs/na_by_country_variable.csv", row.names = FALSE)

#######################
# 14. WEIGHTS & BALANCE
#######################

synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out)
print(synth.tables)

###################################
# 15. TABLE OF POST-TREATMENT GAPS
##################################

# Define the treatment year
treatment_year <- 2007

# Extract the full time series of years from dataprep
all_years <- dataprep.out$tag$time.plot

# Identify indices for post-treatment years (i.e., years > treatment_year)
post_treatment_indices <- which(all_years > treatment_year)

# Extract observed TFR values for post-treatment period
observed_post <- dataprep.out$Y1plot[post_treatment_indices]

# Calculate synthetic TFR values for post-treatment period
synthetic_post <- as.numeric(dataprep.out$Y0plot %*% synth.out$solution.w)[post_treatment_indices]

# Compute the vertical gap between the observed and synthetic series for each year
gap <- observed_post - synthetic_post

# Create a data frame with Year, Observed, Synthetic, and Gap values
post_treatment_gap_table <- data.frame(
  Year = all_years[post_treatment_indices],
  Observed_Fertility = observed_post,
  Synthetic_Fertility = synthetic_post,
  Gap = gap
)

# Display the table in the console
print(post_treatment_gap_table)

# Save the table as a CSV file for further review
write.csv(post_treatment_gap_table, "outputs/post_treatment_gap_table.csv", row.names = FALSE)

#################################
# 16. ESTIMATE EXTRA PEOPLE ALIVE
#################################

# Load & reshape total population data (ages 15-64)
pop15_64 <- read_csv("data/Population ages 15-64, total.csv")

pop15_64_long <- pop15_64 %>%
  select(`Country Name`, starts_with("196"), starts_with("197"), starts_with("198"),
         starts_with("199"), starts_with("200"), starts_with("201"), starts_with("202")) %>%
  pivot_longer(
    cols = -`Country Name`,
    names_to = "Year",
    values_to = "Population"
  ) %>%
  mutate(Year = as.numeric(Year))

# Filter for Georgia and the post-treatment period (2008-2022)
pop_georgia <- pop15_64_long %>%
  filter(`Country Name` == "Georgia", Year >= 2008, Year <= 2022)

# Load & reshape female percentage data
female_pct <- read_csv("data/Population, female (% of total population).csv")

female_pct_long <- female_pct %>%
  select(`Country Name`, starts_with("196"), starts_with("197"), starts_with("198"),
         starts_with("199"), starts_with("200"), starts_with("201"), starts_with("202")) %>%
  pivot_longer(
    cols = -`Country Name`,
    names_to = "Year",
    values_to = "Female_Pct"
  ) %>%
  mutate(Year = as.numeric(Year))

# Filter for Georgia and the post-treatment period (2008-2022)
female_pct_georgia <- female_pct_long %>%
  filter(`Country Name` == "Georgia", Year >= 2008, Year <= 2022)

# Merge to compute estimated female population in Georgia
# Here we first compute the total female population from the available data,
# then estimate the number in the reproductive age range.
pop_female <- pop_georgia %>%
  left_join(female_pct_georgia, by = c("Country Name", "Year")) %>%
  mutate(Female_Population = Population * (Female_Pct / 100),
         # Assume about 65% of women aged 15-64 are in the 15-49 age range
         Repro_Female_Pop = Female_Population * 0.65)

# Estimate extra births using the TFR gap
#
# Since TFR represents births per woman over her entire reproductive lifespan,
# we convert the gap to an annual extra birth rate by dividing by an assumed
# reproductive span (e.g., 30 years).
repro_years <- 30  # average reproductive period in years

extra_births <- post_treatment_gap_table %>%
  filter(Year >= 2008, Year <= 2022) %>%
  left_join(pop_female %>% select(Year, Repro_Female_Pop), by = "Year") %>%
  mutate(Extra_Births = (Gap / repro_years) * Repro_Female_Pop)

#  Print the results
print("Extra births (extra people alive) by year:")
print(extra_births %>% select(Year, Extra_Births))

total_extra_births <- sum(extra_births$Extra_Births, na.rm = TRUE)
print(paste("Total extra births (extra people alive) from 2008 to 2022:", total_extra_births))

# Represent total extra births as a percentage of the cumulative reproductive-age female population

# Compute the total reproductive-age female population over the post-treatment period
total_repro_female_pop <- sum(pop_female$Repro_Female_Pop, na.rm = TRUE)

# Compute the percentage: (total extra births / total cumulative reproductive population) * 100
percentage_extra_births <- (total_extra_births / total_repro_female_pop) * 100

# Annual average comparison
avg_repro_female_pop <- mean(pop_female$Repro_Female_Pop, na.rm = TRUE)
annual_percentage_extra_births <- (extra_births$Extra_Births / avg_repro_female_pop) * 100

print(paste("Total extra births as percentage of cumulative reproductive-age female population (2008-2022):", 
            round(percentage_extra_births, 2), "%"))

print("Extra births (as percentage of average reproductive-age female population) by year:")
print(data.frame(Year = extra_births$Year, 
                 Percentage = round(annual_percentage_extra_births, 2)))

#####################################################
# 17. CALCULATE COHEN'S D FOR POST-TREATMENT TFR GAP
#####################################################

# Extract the full time series of years from dataprep
years <- dataprep.out$tag$time.plot

# Define the treatment year (2007) and identify post-treatment years (2008-2022)
treatment_year <- 2007
post_treatment_indices <- which(years > treatment_year)

# Extract observed and synthetic TFR values for the post-treatment period
observed_post <- dataprep.out$Y1plot[post_treatment_indices]
synthetic_post <- as.numeric(dataprep.out$Y0plot %*% synth.out$solution.w)[post_treatment_indices]

# Calculate the gap (difference between observed and synthetic TFR)
tfr_gap <- observed_post - synthetic_post

# Compute Cohen's d for the paired differences:
# Cohen's d = (mean difference) / (standard deviation of differences)
mean_gap <- mean(tfr_gap, na.rm = TRUE)
sd_gap   <- sd(tfr_gap, na.rm = TRUE)
cohens_d <- mean_gap / sd_gap

print(mean_gap)
print(sd_gap)

# Print the result
print(paste("Cohen's d for post-treatment TFR gap:", round(cohens_d, 3)))

##########################
# 18. PLOT LOOCV ESTIMATES
##########################

# Extract the common time vector and the treated (observed) outcome.
years <- dataprep.out$tag$time.plot
observed <- dataprep.out$Y1plot

# Compute the baseline synthetic control trajectory.
synthetic_baseline <- as.numeric(dataprep.out$Y0plot %*% synth.out$solution.w)

# Create a data frame to store leave‐one‐out synthetic control trajectories.
# Each element in loocv_results contains its own dataprep and synth objects.
loo_df <- data.frame(Year = years)

for (name in names(loocv_results)) {
  dp_loocv <- loocv_results[[name]]$dataprep
  synth_loocv <- loocv_results[[name]]$synth
  # Calculate the synthetic trajectory for the leave-one-out iteration:
  loo_trajectory <- as.numeric(dp_loocv$Y0plot %*% synth_loocv$solution.w)
  loo_df[[name]] <- loo_trajectory
}

# Melt the leave-one-out data frame to long format for plotting.
loo_long <- melt(loo_df, id.vars = "Year", variable.name = "LOO", value.name = "Synthetic")

# Create a data frame for the treated and baseline synthetic control trajectories.
df_baseline <- data.frame(
  Year = years,
  Observed = observed,
  Synthetic = synthetic_baseline
)

p <- ggplot() +
  # Plot treated (observed) outcomes as a solid black line.
  geom_line(data = df_baseline, aes(x = Year, y = X1),
            color = "black", size = 1) +
  # Plot baseline synthetic control as a dashed black line.
  geom_line(data = df_baseline, aes(x = Year, y = Synthetic),
            color = "black", size = 1, linetype = "dashed") +
  # Overlay all leave-one-out synthetic control trajectories as gray lines.
  geom_line(data = loo_long, aes(x = Year, y = Synthetic, group = LOO),
            color = "grey", size = 0.8, alpha = 0.7) +
  # Add a vertical line to mark the treatment year.
  geom_vline(xintercept = treatment_year, linetype = "dashed", color = "red", linewidth = 0.7) +
  labs(title = "Leave-One-Out Synthetic Control Estimates",
       x = "Year",
       y = "Fertility Rate (Births per Woman)") +
  scale_y_continuous(limits = c(0, 5)) +
  theme_minimal()

print(p)
