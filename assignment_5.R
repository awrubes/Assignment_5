library(tidyverse)
library(dplyr)
library(DBI)
library(RMySQL)
library(ggplot2)

#user_name <- readline(prompt = "Enter database username: ")
#pass_word <- readline(prompt = "Enter database password: ")

user_name <- "new_user"
pass_word <- "Tigger123$%"

conn <- dbConnect(RMySQL::MySQL(), dbname = "tidy", host = "localhost", user = user_name, password = pass_word)

dbListTables(conn)

df <- dbReadTable(conn, "messy")
head(df)

#alternatively read in the CSV file
df_csv <- read_csv("/Users/alliewrubel/Desktop/mess_data.csv")
head(df_csv)

#get rid of empty row
df_csv_clean <- df_csv %>%
  filter(rowSums(is.na(.)) != ncol(.))

head(df_csv_clean)

colnames(df_csv_clean)

first_col <- colnames(df_csv_clean)[1]

#add airline name
df_csv_clean <- df_csv_clean %>%
  mutate(
    airline = ifelse(is.na(!!sym(first_col)), lag(!!sym(first_col)), !!sym(first_col))
  )%>%
  select(-all_of(first_col))

head(df_csv_clean)

#pivot to long format
df_csv_clean <-df_csv_clean %>%
  pivot_longer(
    cols = "Los_Angeles":"Seattle",
    names_to = "city",
    values_to = "flight_count"
  )

new_first_col <- colnames(df_csv_clean)[1]

df_csv_clean <- df_csv_clean %>%
  rename(status = all_of(new_first_col))

head(df_csv_clean)

#create a wide table with on-time and delayed columns for added analysis
df_csv_clean_wide <- df_csv_clean %>%
  pivot_wider(names_from = status, values_from = flight_count, names_prefix = "total_")

head(df_csv_clean_wide)
View(df_csv_clean_wide)

# Group data by airline and status (on time vs delayed)
summary_by_status <- df_csv_clean %>%
  group_by(airline, status) %>%
  summarize(total_flights = sum(flight_count, na.rm = TRUE)) %>%
  ungroup()

# View the summary
print(summary_by_status)

total_alaska <- summary_by_status %>%
  filter(airline == "ALASKA") %>%
  summarize(total_flights = sum(total_flights, na.rm = TRUE)) %>%
  pull(total_flights)  # `pull()` extracts the value as a scalar

total_amwest <- summary_by_status %>%
  filter(airline == "AM WEST") %>%
  summarize(total_flights = sum(total_flights, na.rm = TRUE)) %>%
  pull(total_flights)  # `pull()` extracts the value as a scalar

#calculate the percent of delays and on time by total number of flights per airline
summar_by_status_percent_total <- summary_by_status %>%
  mutate(
    percent_of_total = case_when(
      airline == "ALASKA" ~ (total_flights/total_alaska) * 100,
      airline == "AM WEST" ~ (total_flights/total_amwest) * 100
    )
  )

print(summar_by_status_percent_total)

#compare percent of delays by city
summary_by_percent <- df_csv_clean_wide %>%
  mutate(
    percent_delayed = (total_delayed / (`total_on time` + total_delayed))*100
  )

print(summary_by_percent)

#compare total percent delay overall for both airlines
overall_summary <- summary_by_percent %>%
  group_by(airline) %>%
  summarize(
    total_delayed = sum(total_delayed, na.rm=TRUE),
    total_on_time = sum(`total_on time`, na.rm = TRUE),
    overall_percent_delayed = (total_delayed / (total_delayed + total_on_time)) * 100
)

print(overall_summary)

# Calculate weighted average delay percentage for overall comparison
overall_weighted_avg <- summary_by_percent %>%
  summarize(
    total_delayed = sum(total_delayed),
    total_flights = sum(`total_on time` + total_delayed),
    weighted_avg_percent_delayed = (sum(total_delayed) / sum(`total_on time` + total_delayed)) * 100
  )


print(overall_weighted_avg)

ggplot(overall_summary, aes(x = airline, y = overall_percent_delayed, fill = airline)) +
  geom_col() +
  labs(title = "Overall Percentage of Delayed Flights by Airline", 
       y = "Percentage of Delayed Flights", x = "Airline") +
  theme_minimal()


ggplot(summary_by_percent, aes(x = city, y = percent_delayed, fill = airline)) +
  geom_col(position = "dodge") +
  labs(title = "Percentage of Delayed Flights by City and Airline", 
       y = "Percentage of Delayed Flights", x = "City") +
  theme_minimal()

