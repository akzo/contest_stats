# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Read the Excel file into a data frame
my_data <- read_excel("R stat experiments/_NOIS2-137 Submission Form (Responses).xlsx")

# Define lists of countries (case-insensitive)
wto_gpa_countries <- tolower(c("Armenia", "united states", "u.s", "Aruba", "Australia", "Austria", "Belgium", "Bulgaria", "Canada", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hong Kong", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", "Korea (Republic of)", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova", "Montenegro", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Romania", "Singapore", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "Taiwan (known in the World Trade Organization as 'the Separate Customs Territory of Taiwan, Penghu, Kinmen and Matsu (Chinese Taipei)')", "Ukraine", "United Kingdom"))
fta_countries <- tolower(c("Perú", "Maroc", "Australia", "Bahrain", "Chile", "Colombia", "Costa Rica", "Dominican Republic", "El Salvador", "Guatemala", "Honduras", "Korea (Republic of)", "Mexico", "Morocco", "Nicaragua", "Oman", "Panama", "Peru", "Singapore"))
least_developed_countries <- tolower(c("Sénégal", "Afghanistan", "Angola", "Bangladesh", "Benin", "Bhutan", "Burkina Faso", "Burundi", "Cambodia", "Central African Republic", "Chad", "Comoros", "Democratic Republic of Congo", "Djibouti", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gambia", "Guinea", "Guinea-Bissau", "Haiti", "Kiribati", "Laos", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", "Mozambique", "Nepal", "Niger", "Rwanda", "Samoa", "Sao Tome and Principe", "Senegal", "Sierra Leone", "Solomon Islands", "Somalia", "South Sudan", "Tanzania", "Timor-Leste", "Togo", "Tuvalu", "Uganda", "Vanuatu", "Yemen", "Zambia"))
caribbean_basin_countries <- tolower(c("Antigua and Barbuda", "Aruba", "Bahamas", "Barbados", "Belize", "Bonaire", "British Virgin Islands", "Curacao", "Dominica", "Grenada", "Guyana", "Haiti", "Jamaica", "Montserrat", "Saba", "St. Kitts and Nevis", "St. Lucia", "St. Vincent and the Grenadines", "Sint Eustatius", "Sint Maarten", "Trinidad and Tobago"))

# Convert the column values to lowercase
my_data$`What is your country of residence?` <- tolower(my_data$`What is your country of residence?`)

# Filter by countries
filtered_data <- my_data %>%
  filter(`What is your country of residence?` %in% c(wto_gpa_countries, fta_countries, least_developed_countries, caribbean_basin_countries))

# Convert 'Timestamp' column to a Date
filtered_data$Timestamp <- as.Date(filtered_data$Timestamp)

# Create a sequence of dates covering the entire range
date_range <- seq(min(filtered_data$Timestamp), max(filtered_data$Timestamp), by = "1 day")

# Create a data frame with all dates
all_dates <- data.frame(Timestamp = date_range)

# Merge to fill in missing dates
entries_per_day <- all_dates %>%
  left_join(filtered_data %>%
              group_by(Timestamp) %>%
              summarise(Count = n())
  ) %>%
  replace_na(list(Count = 0))

# Create a line plot to visualize the trend of the number of entries per day
ggplot(entries_per_day, aes(x = Timestamp, y = Count)) +
  geom_line() +
  labs(title = "Number of Entries per Day",
       x = "Date",
       y = "Number of Entries") +
  scale_x_date(date_labels = "%b %d", date_breaks = "3 day", expand = c(0, 0))

# Create a bar chart to visualize the number of entries by country
entries_by_country <- filtered_data %>%
  group_by(`What is your country of residence?`) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

ggplot(entries_by_country, aes(x = reorder(`What is your country of residence?`, -Count), y = Count)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Entries by Country",
       x = "Country",
       y = "Number of Entries") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Filter the data based on the country of residence
#filtered_data <- filter(my_data, `What is your country of residence?` %in% c(wto_gpa_countries, fta_countries, least_developed_countries, caribbean_basin_countries))

# View the filtered data
#head(filtered_data)