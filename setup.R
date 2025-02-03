# Load Packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(viridis)
library(readr)

list.files()

# Load Data
 # Define the GitHub raw file URL
url <- "https://raw.githubusercontent.com/melanie-vera/International-migration/main/International-migration-database.csv"
url2 <- "https://raw.githubusercontent.com/melanie-vera/International-migration/main/International-migration-database-citizenship.csv"

 # Read the CSV file directly from GitHub
data <- read_csv(url)
data2 <- read_csv(url2)
 # Quick view
head(data)

### 1. Inflows of foreign population into selected OECD countries

# Filter the data for the relevant years
filtered_data <- data[data$TIME_PERIOD %in% c(2019, 2020, 2021, 2022), ]
aggregated_table <- filtered_data %>%
  group_by(`Reference area`, TIME_PERIOD) %>%
  summarise(Total_OBS_VALUE = sum(OBS_VALUE, na.rm = TRUE)) %>%
  ungroup()

# Transpose years into columns
wide_table <- aggregated_table %>%
  pivot_wider(names_from = TIME_PERIOD, values_from = Total_OBS_VALUE)

# Calculate variations and percentage variations
wide_table <- wide_table %>%
  mutate(
    Var_2022_2021 = `2022` - `2021`,
    Perc_Var_2022_2021 = (`2022` - `2021`) / `2021` * 100,
    Var_2022_2019 = `2022` - `2019`,
    Perc_Var_2022_2019 = (`2022` - `2019`) / `2019` * 100
  )

# Calculate the total inflows for each year and overall variations
totals_row <- wide_table %>%
  summarise(
    `Reference area` = "Total OECD area",
    `2019` = sum(`2019`, na.rm = TRUE),
    `2020` = sum(`2021`, na.rm = TRUE),
    `2021` = sum(`2021`, na.rm = TRUE),
    `2022` = sum(`2022`, na.rm = TRUE)
  ) %>%
  mutate(
    Var_2022_2021 = `2022` - `2021`,
    Perc_Var_2022_2021 = (Var_2022_2021 / `2021`) * 100,
    Var_2022_2019 = `2022` - `2019`,
    Perc_Var_2022_2019 = (Var_2022_2019 / `2019`) * 100
  )

# Add the totals row to the final table
final_table <- bind_rows(wide_table, totals_row)

# Display the final table
print(final_table)

### 2. Inflows of foreign population by nationality (top 3 OECD receiving countries)

# Identify the top 3 countries with the highest inflows in 2022
top_3_2022 <- wide_table %>%
  arrange(desc(`2022`)) %>%  # Sort by 2022 inflows in descending order
  slice_head(n = 3)  # Select top 3 countries

# Save the country names (Reference_area) of the top 3 countries in a vector
top_3_countries <- top_3_2022$`Reference area`

# Now filter the original data for these top 3 countries
filtered_data_top_3 <- data2 %>%
  filter(`Reference area`%in% top_3_countries)

# Filter the data for 2022 and the selected variables
filtered_data_top_3_2022 <- filtered_data_top_3 %>%
  filter(TIME_PERIOD == 2022) %>%  # Keep only 2022
  select(`Reference area`, Citizenship, OBS_VALUE)  # Keep only the relevant columns

# Filter out rows where Citizenship is "World"
filtered_data_top_3_2022 <- filtered_data_top_3_2022 %>%
  filter(Citizenship != "World") # Keep only the rows where Citizenship is not "World"

# Calculate the percentage of each Citizenship within each Reference_area
filtered_data_top_3_2022 <- filtered_data_top_3_2022 %>%
  group_by(`Reference area`, Citizenship) %>%
  summarise(Total_OBS_VALUE = sum(OBS_VALUE, na.rm = TRUE)) %>%  # Sum OBS_VALUE for each Citizenship
  ungroup() %>%
  group_by(`Reference area`) %>%
  mutate(
    Total_area = sum(Total_OBS_VALUE, na.rm = TRUE),  # Total for each Reference_area
    Perc_Citizenship = (Total_OBS_VALUE / Total_area) * 100  # Calculate percentage for each Citizenship
  ) %>%
  ungroup()

# Keep only the top 10 Citizenship percentages for each Reference_area
top_10_citizenships <- filtered_data_top_3_2022 %>%
  group_by(`Reference area`) %>%
  top_n(10, Perc_Citizenship) %>%  # Select top 10 Citizenship by percentage
  ungroup()  # Ungroup after top_n operation

# Germany
germany_data <- top_10_citizenships %>%
  filter(`Reference area` == "Germany")

# Calculate the sum of the percentages for the top 10 nationalities
total_percentage <- sum(germany_data$Perc_Citizenship)

# Create a new row for "Other Citizenships" with the remaining percentage
other_citizenship <- data.frame(
  Reference.area = "Germany",
  Citizenship = "Other Citizenships",
  Total_OBS_VALUE = 0,  # This will be calculated later
  Total_area = sum(germany_data$Total_OBS_VALUE),
  Perc_Citizenship = 100 - total_percentage  # Remaining percentage to reach 100%
)
# Add this new row to the original Germany data
germany_data <- rbind(germany_data, other_citizenship)

# Create the pie chart with percentages displayed
ggplot(germany_data, aes(x = "", y = Perc_Citizenship, fill = Citizenship)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  # Converts to a pie chart
  theme_void() +  # Removes axes and background
  labs(title = "Top 10 Nationalities Citizenships recieved in Germany (2022)") +
  scale_fill_brewer(palette = "Set3")   # Color palette for better visualization
