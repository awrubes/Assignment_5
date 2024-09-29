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
first_col

#add airline name
df_csv_clean <- df_csv_clean %>%
  mutate(
    airline = ifelse(is.na(!!sym(first_col)), lag(!!sym(first_col)), !!sym(first_col))
  )%>%
  select(-first_col)

head(df_csv_clean)

#pivot to long format
df_csv_clean <-df_csv_clean %>%
  pivot_longer(
    cols = "Los_Angeles":"Seattle",
    names_to = "city",
    values_to = "flight_count"
  )

new_first_col <- colnames(df_csv_clean)[1]
new_first_col

df_csv_clean <- df_csv_clean %>%
  rename(status = all_of(new_first_col))

head(df_csv_clean)

#create a wide table with on-time and delayed columns for added analysis
df_csv_clean_wide <- df_csv_clean %>%
  pivot_wider(names_from = status, values_from = flight_count, names_prefix = "total_")

head(df_csv_clean_wide)

# Group data by airline and status (on time vs delayed)
summary_by_status <- df_csv_clean %>%
  group_by(airline, status) %>%
  summarize(total_flights = sum(flight_count, na.rm = TRUE)) %>%
  ungroup()

# View the summary
print(summary_by_status)

# Group by airline and city to compare delays in each city
delays_by_city <- df_csv_clean %>%
  filter(status == "delayed") %>%
  group_by(airline, city) %>%
  summarize(total_delayed = sum(flight_count, na.rm = TRUE)) %>%
  ungroup()

# View the delays by city
print(delays_by_city)

ggplot(delays_by_city, aes(x = city, y = total_delayed, fill = airline)) +
  geom_col(position = "dodge") +
  labs(title = "Total Delayed Flights by City and Airline", 
       y = "Total Delayed Flights", x = "City") +
  theme_minimal()


ggplot(delays_by_city, aes(x = city, y = total_delayed, fill = airline)) +
  geom_col(position = "stack") +
  labs(title = "Total Delayed Flights by City (Stacked)", 
       y = "Total Delayed Flights", x = "City") +
  theme_minimal()

