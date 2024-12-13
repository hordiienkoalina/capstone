# Load necessary libraries
library(Synth)
library(readr)
library(dplyr)
library(tidyr)
library(SCtools)
library(ggplot2)

### MODEL ###

# Set the file paths for the data
fertility_rate_file <- "data/Fertility rate, total (births per woman).csv"
gdp_growth_file <- "data/GDP per capita growth (annual %).csv"
urban_pop_growth_file <- "data/Urban population growth (annual %).csv"
unemployment_file <- "data/Unemployment, total (% of total labor force) (national estimate).csv"
female_labor_file <- "data/Labor force, female (% of total labor force).csv"
age_dependency_file <- "data/Age dependency ratio (% of working-age population).csv"
female_male_labor_ratio_file <- "data/Ratio of female to male labor force participation rate (%) (national estimate).csv"
net_migration_file <- "data/Net migration.csv"

# Load the data
fertility_rate <- read_csv(fertility_rate_file)
gdp_growth <- read_csv(gdp_growth_file)
urban_pop_growth <- read_csv(urban_pop_growth_file)
unemployment <- read_csv(unemployment_file)
female_labor <- read_csv(female_labor_file)
age_dependency <- read_csv(age_dependency_file)
female_male_labor_ratio <- read_csv(female_male_labor_ratio_file)
net_migration <- read_csv(net_migration_file)

# Define the donor pool countries
donor_countries <- c("Ireland", 
                     #"Iceland", 
                     #"France",
                     #"Sweden", 
                     #"Armenia", 
                     #"Azerbaijan", 
                     #"Ukraine", 
                     #"Russian Federation", 
                     "Moldova", 
                     #"Estonia",
                     "Latvia", 
                     "Lithuania",
                     "Kazakhstan"
                     #"Kyrgyz Republic",
                     #"Uzbekistan"
                     )

# Function to filter and clean each dataset for the donor pool
clean_and_filter_data <- function(data, countries) {
  # Keep only the necessary columns (drop redundant or NA columns)
  data <- data %>%
    select(`Country Name`, starts_with("19"), starts_with("20")) %>%
    filter(`Country Name` %in% c("Georgia", countries))
  
  return(data)
}

# Clean and filter each dataset
fertility_rate_filtered <- clean_and_filter_data(fertility_rate, donor_countries)
gdp_growth_filtered <- clean_and_filter_data(gdp_growth, donor_countries)
urban_pop_growth_filtered <- clean_and_filter_data(urban_pop_growth, donor_countries)
unemployment_filtered <- clean_and_filter_data(unemployment, donor_countries)
female_labor_filtered <- clean_and_filter_data(female_labor, donor_countries)
age_dependency_filtered <- clean_and_filter_data(age_dependency, donor_countries)
female_male_labor_ratio_filtered <- clean_and_filter_data(female_male_labor_ratio, donor_countries)
net_migration_filtered <- clean_and_filter_data(net_migration, donor_countries)

# Create a numeric identifier for the countries
country_codes <- data.frame(
  `Country Name` = c("Georgia", donor_countries),
  Country_Code = 1:(length(donor_countries) + 1)
)

# Prepare the data for SCM (reshaping to long format)
prepare_scm_data <- function(df, outcome_var) {
  df_long <- df %>%
    pivot_longer(cols = starts_with("19") | starts_with("20"), 
                 names_to = "Year", values_to = outcome_var) %>%
    mutate(Year = as.numeric(Year)) %>%
    filter(!is.na(get(outcome_var)))  # Remove NA values to keep the data clean
  return(df_long)
}

# Reshape all datasets
fertility_rate_long <- prepare_scm_data(fertility_rate_filtered, "Fertility_Rate")
gdp_growth_long <- prepare_scm_data(gdp_growth_filtered, "GDP_Per_Capita_Growth")
urban_pop_growth_long <- prepare_scm_data(urban_pop_growth_filtered, "Urban_Population_Growth")
unemployment_long <- prepare_scm_data(unemployment_filtered, "Unemployment_Rate")
female_labor_long <- prepare_scm_data(female_labor_filtered, "Female_Labor_Force")
age_dependency_long <- prepare_scm_data(age_dependency_filtered, "Age_Dependency_Ratio")
female_male_labor_ratio_long <- prepare_scm_data(female_male_labor_ratio_filtered, "Female_Male_Labor_Ratio")
net_migration_long <- prepare_scm_data(net_migration_filtered, "Net_Migration")

# Merge the covariates into a single dataframe
merged_data <- fertility_rate_long %>%
  left_join(gdp_growth_long, by = c("Country Name", "Year")) %>%
  left_join(urban_pop_growth_long, by = c("Country Name", "Year")) %>%
  left_join(unemployment_long, by = c("Country Name", "Year")) %>%
  left_join(female_labor_long, by = c("Country Name", "Year")) %>%
  left_join(age_dependency_long, by = c("Country Name", "Year")) %>%
  left_join(female_male_labor_ratio_long, by = c("Country Name", "Year")) %>%
  left_join(net_migration_long, by = c("Country Name", "Year"))

# Rename 'Country.Name' to 'Country Name' in the country_codes dataframe
country_codes <- country_codes %>%
  rename(`Country Name` = `Country.Name`)

# Add country codes to the merged data
merged_data <- merged_data %>%
  left_join(country_codes, by = "Country Name") %>%
  select(-starts_with("..."))  # Remove any redundant columns if necessary

# Ensure 'Country_Code' is numeric
merged_data <- merged_data %>%
  mutate(Country_Code = as.numeric(Country_Code))

# Ensure merged_data is a data frame (not a tibble)
merged_data <- as.data.frame(merged_data)

# Check if the conversion was successful
class(merged_data)  # Should return "data.frame"

# Run the dataprep function again
dataprep.out <- dataprep(
  foo = merged_data,
  predictors = c("GDP_Per_Capita_Growth", 
                 "Urban_Population_Growth", 
                 "Unemployment_Rate", 
                 "Female_Labor_Force", 
                 "Age_Dependency_Ratio", 
                 "Net_Migration"),
  predictors.op = "mean",
  dependent = "Fertility_Rate",
  unit.variable = "Country_Code",
  time.variable = "Year",
  treatment.identifier = 1,  # Georgia
  controls.identifier = 2:(length(donor_countries) + 1),
  time.predictors.prior = c(1960:2007),
  time.optimize.ssr = c(1960:2007),
  unit.names.variable = "Country Name",
  time.plot = c(1960:2022)
)

# Run the synthetic control analysis
synth.out <- synth(dataprep.out)

# Plot Treated vs Synthetic Georgia
path.plot(dataprep.res = dataprep.out, synth.res = synth.out, Ylab = "Fertility rate, total (births per woman)")

# Add a vertical line at 2007
abline(v = 2007, col = "black", lty = 2, lwd = 2)

### PLACEBO STUDIES ###

# PLACEBO IN-SPACE
dependent_var <- "Fertility_Rate"
predictors <- c("GDP_Per_Capita_Growth", 
                "Urban_Population_Growth",
                "Unemployment_Rate",
                "Female_Labor_Force",
                "Age_Dependency_Ratio",
                "Net_Migration")

treatment_unit <- 1 # Georgia
control_units <- 2:(length(donor_countries) + 1)
treatment_time <- 2007
pre_treatment_period <- 1960:2007
post_treatment_period <- 2008:2022
plot_period <- 1960:2022

# Run multiple.synth
res <- multiple.synth(
  foo = merged_data,
  predictors = predictors,
  predictors.op = "mean",
  dependent = dependent_var,
  unit.variable = "Country_Code",
  time.variable = "Year",
  special.predictors = NULL,
  treated.units = treatment_unit,
  control.units = control_units,
  time.predictors.prior = pre_treatment_period,
  time.optimize.ssr = pre_treatment_period,
  unit.names.variable = "Country Name",
  time.plot = plot_period,
  treatment.time = treatment_time,
  gen.placebos = FALSE,
  strategy = "sequential",
  Sigf.ipop = 5
)

placebo <- generate.placebos(dataprep.out, synth.out, Sigf.ipop = 2, strategy='multicore')
p <- plot_placebos(placebo, discard.extreme=FALSE, xlab='Year')

# Fixing the label color scale
p + scale_color_manual(
  name = "", 
  values = c("black", "grey"), 
  labels = c("Georgia", "Control units")
)

mspe_plot(placebo)
ratio <- mspe.test(placebo)
print(ratio$p.val)
print(ratio$test)

# PLACEBO IN TIME

# Define placebo treatment years
placebo_years <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006)
actual_treatment_year <- 2007

# Store results for comparison
placebo_results <- list()

for (placebo_year in placebo_years) {
  # Update dataprep for placebo year
  dataprep.placebo <- dataprep(
    foo = merged_data,
    predictors = c("GDP_Per_Capita_Growth", 
                   "Urban_Population_Growth", 
                   "Unemployment_Rate", 
                   "Female_Labor_Force", 
                   "Age_Dependency_Ratio", 
                   "Net_Migration"),
    predictors.op = "mean",
    dependent = "Fertility_Rate",
    unit.variable = "Country_Code",
    time.variable = "Year",
    treatment.identifier = 1,
    controls.identifier = 2:(length(donor_countries) + 1),
    time.predictors.prior = 1960:(placebo_year - 1),
    time.optimize.ssr = 1960:(placebo_year - 1),
    unit.names.variable = "Country Name",
    time.plot = c(1960:2022)
  )
  
  # Run the synthetic control analysis for the placebo
  synth.placebo <- synth(dataprep.placebo)
  
  placebo_results[[as.character(placebo_year)]] <- list(
    dataprep = dataprep.placebo,
    synth = synth.placebo
  )
  
  # Plot the results
  path.plot(dataprep.res = dataprep.placebo, synth.res = synth.placebo,
            Ylab = "Fertility Rate (Placebo)", Xlab = "Year",
            Legend = c("Treated", "Synthetic"))
  
  years <- seq(1960, 2022, by = 1)
  axis(1, at = years, labels = years, las = 2)
  abline(v = placebo_year, col = "blue", lty = 2, lwd = 2)
}

### LOOCV ###

# Initialize an empty list to store results
loocv_results <- list()

# Loop over each donor country to exclude it from the donor pool
for (country_code in control_units) {
  # Define the donor pool excluding the current country
  loocv_controls <- setdiff(control_units, country_code)
  
  # Prepare the data excluding the current country
  loocv_dataprep <- dataprep(
    foo = merged_data,
    predictors = c("GDP_Per_Capita_Growth", 
                   "Urban_Population_Growth", 
                   "Unemployment_Rate", 
                   "Female_Labor_Force", 
                   "Age_Dependency_Ratio", 
                   "Net_Migration"),
    predictors.op = "mean",
    dependent = "Fertility_Rate",
    unit.variable = "Country_Code",
    time.variable = "Year",
    treatment.identifier = 1,
    controls.identifier = loocv_controls,
    time.predictors.prior = c(1960:2007),
    time.optimize.ssr = c(1960:2007),
    unit.names.variable = "Country Name",
    time.plot = c(1960:2022)
  )
  
  # Run the synthetic control analysis
  loocv_synth <- synth(loocv_dataprep)
  
  # Extract the country name for the title
  excluded_country_name <- merged_data$`Country Name`[merged_data$Country_Code == country_code][1]
  
  # Store the results
  loocv_results[[excluded_country_name]] <- list(
    dataprep = loocv_dataprep,
    synth = loocv_synth
  )
  
  # Plot the results
  path.plot(
    dataprep.res = loocv_results[[excluded_country_name]]$dataprep,
    synth.res = loocv_results[[excluded_country_name]]$synth,
    Ylab = "Fertility Rate",
    Xlab = "Year",
    Legend = c("Treated", "Synthetic"),
    Main = paste("LOOCV: Excluding", excluded_country_name)
  )
  abline(v = 2007, col = "red", lty = 2, lwd = 2)
}