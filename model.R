# Load necessary libraries
library(Synth)
library(readr)
library(dplyr)
library(tidyr)

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
  controls.identifier = 2:(length(donor_countries) + 1),  # Donor Pool Countries
  time.predictors.prior = c(1960:2007),
  time.optimize.ssr = c(1960:2007),
  unit.names.variable = "Country Name",
  time.plot = c(1960:2022)
)

# Run the synthetic control analysis
synth.out <- synth(dataprep.out)

# Plot Real vs Synthetic Georgia
path.plot(dataprep.res = dataprep.out, synth.res = synth.out, Ylab = "Fertility rate, total (births per woman)")

# Add a vertical line at 2007
abline(v = 2007, col = "black", lty = 2, lwd = 2)