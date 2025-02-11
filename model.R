###############################
# 1. LOAD LIBRARIES & FILE PATHS
###############################
library(Synth)
library(readr)
library(dplyr)
library(tidyr)
library(SCtools)
library(ggplot2)

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

###############################
# 2. LOAD DATASETS
###############################
# Read each CSV file
fertility_rate   <- read_csv(file_paths$fertility_rate)
gdp_growth       <- read_csv(file_paths$gdp_growth)
urban_pop_growth <- read_csv(file_paths$urban_pop_growth)
unemployment     <- read_csv(file_paths$unemployment)
female_labor     <- read_csv(file_paths$female_labor)
age_dependency   <- read_csv(file_paths$age_dependency)
female_male_ratio<- read_csv(file_paths$female_male_ratio)
net_migration    <- read_csv(file_paths$net_migration)

###############################
# 3. DEFINE DONOR POOL & HELPER FUNCTIONS
###############################
# Define donor countries
donor_countries <- c("Ireland", 
                     "Iceland", 
                     "France", 
                     "Sweden", 
                     "Armenia", 
                     "Azerbaijan", 
                     "Ukraine", 
                     "Russian Federation", 
                     "Moldova", 
                     "Estonia", 
                     "Latvia", 
                     "Lithuania", 
                     "Kazakhstan", 
                     "Uzbekistan")

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

###############################
# 5. MERGE DATASETS & ADD COUNTRY CODES
###############################
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

# Run dataprep to create the data structure for synth()
dataprep.out <- dataprep(
  foo = merged_data,
  predictors         = c("GDP_Per_Capita_Growth", 
                         "Urban_Population_Growth",
                         "Unemployment_Rate", 
                         "Female_Labor_Force",
                         "Age_Dependency_Ratio", 
                         "Net_Migration"),
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

# Run the synthetic control analysis
synth.out <- synth(dataprep.out)

###############################
# 7. PLOT TREATED VS. SYNTHETIC GEORGIA
###############################
path.plot(dataprep.res = dataprep.out, synth.res = synth.out,
          Ylab = "Fertility Rate (Births per Woman)",
          Ylim = c(1, 3.5))
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

p  <- plot_placebos(placebo, discard.extreme = FALSE, xlab = 'Year') +
  ggtitle("Placebo Results (No Limit)")
p1 <- plot_placebos(placebo, discard.extreme = TRUE, mspe.limit = 100, xlab = 'Year') +
  ggtitle("Placebo Results (MSPE Limit = 100x)")
p2 <- plot_placebos(placebo, discard.extreme = TRUE, mspe.limit = 10, xlab = 'Year') +
  ggtitle("Placebo Results (MSPE Limit = 10x)")

p  + scale_color_manual(name = "", values = c("black", "grey"),
                              labels = c("Georgia", "Control units"))
p1 + scale_color_manual(name = "", values = c("black", "grey"),
                              labels = c("Georgia", "Control units"))
p2 + scale_color_manual(name = "", values = c("black", "grey"),
                              labels = c("Georgia", "Control units"))

# Compute and print MSPE ratio and p-value
ratio   <- mspe.test(placebo)
p_value <- ratio$p.val
print(paste("Placebo in-space p-value:", p_value))
print(ratio$test)

###############################
# 9. MSPE RATIOS (SCATTER PLOT + HISTOGRAM)
###############################
# Scatter Plot
mspe_plot(placebo)

# Extract and convert MSPE ratios to numeric
mspe_values <- as.numeric(as.character(unlist(ratio$test)))
mspe_data   <- data.frame(MSPE_Ratio = mspe_values)

# Assume Georgia's MSPE is at index 15 (adjust index if needed)
georgia_mspe <- mspe_values[15]

# Plot histogram with Georgia's MSPE highlighted
ggplot(mspe_data, aes(x = MSPE_Ratio)) +
  geom_histogram(bins = 30, fill = "white", color = "black", alpha = 0.7) +
  geom_vline(xintercept = georgia_mspe, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = georgia_mspe, y = Inf, label = "Georgia",
           vjust = -0.5, color = "red") +
  labs(title = "Histogram of MSPE Ratios",
       x = "MSPE Ratio", y = "Frequency") +
  theme_minimal()

###############################
# 10. PLACEBO STUDIES: IN-TIME
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
            Legend = c("Treated", "Synthetic"), Ylim = c(1, 3.5))
  abline(v = py, col = "blue", lty = 2, lwd = 2)
}

###############################
# 11. LEAVE-ONE-OUT CROSS VALIDATION (LOOCV)
###############################
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
  
  # Plot LOOCV result
  path.plot(dataprep.res = dp_loocv, synth.res = synth_loocv,
            Ylab = "Fertility Rate", Xlab = "Year",
            Legend = c("Treated", "Synthetic"),
            Main = paste("LOOCV: Excluding", excluded_name),
            Ylim = c(1, 3.5))
  abline(v = 2007, col = "red", lty = 2, lwd = 2)
}

###############################
# 12. NA VALUES ANALYSIS
###############################
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
write.csv(na_by_country_variable, "na_by_country_variable.csv", row.names = FALSE)
