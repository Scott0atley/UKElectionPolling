library(tidyverse)
library(readxl)
library(lubridate)
library(scales)

##devtools::install_github("jackobailey/britpol")
library(britpol)
library(ggplot2)
library(dplyr)


setwd("G:/Stata data and do/UK election Polling Data 1950-Present")
raw.dir <- "G:/Stata data and do/UK election Polling Data 1950-Present"




opinion_polls_wiki_df <- read_excel("UK Polling Data Raw.xlsx")


opinion_polls_long <- tidyr::pivot_longer(opinion_polls_wiki_df, -c(Month, Year), names_to = "Party", values_to = "Percentage")

###Mutate to data variable

# Check for missing or NA values
sum(is.na(opinion_polls_long$Year))
sum(is.na(opinion_polls_long$Month))

opinion_polls_long$Month[is.na(opinion_polls_long$Month)] <- "6"

sum(is.na(opinion_polls_long$Month))


# Create a new variable Month_N to represent numeric month values
opinion_polls_long$Month_N <- match(opinion_polls_long$Month, unique(opinion_polls_long$Month))

# Print the result
print(opinion_polls_long)

opinion_polls_long <- opinion_polls_long %>%
  mutate(Month_N = ifelse(Month_N >12, 6, Month_N))


opinion_polls_long$Year <- as.numeric(opinion_polls_long$Year)
opinion_polls_long$Month <- as.numeric(opinion_polls_long$Month_N)

###date

opinion_polls_long$Date <- as.Date(paste(opinion_polls_long$Year, opinion_polls_long$Month_N, '01', sep = '-'))

print(opinion_polls_long)



# Define colors for each party
party_colors <- c(Con = "blue", Lab = "red", LD = "orange", Green = "green", UKIP = "purple")

# Plot the time series scatter plot with all parties, individual colors, and trend lines
ggplot(opinion_polls_long, aes(x = Date, y = Percentage, color = Party)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +  # Add LOESS trend lines
  scale_color_manual(values = party_colors) +  # Assign specific colors to parties
  labs(title = "Poll Performance of Political Parties",
       x = "Date",
       y = "Performance") +
  theme_minimal() +
  theme(legend.position = "right")



# Filter the data to include only Lab, Con, and LD
main_parties <- c("Lab", "Con", "LD")
filtered_data <- opinion_polls_long[opinion_polls_long$Party %in% main_parties,]

# Plot the time series scatter plot with filtered data
ggplot(filtered_data, aes(x = Date, y = Percentage, color = Party)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +  # Add LOESS trend lines
  scale_color_manual(values = c(Lab = "red", Con = "blue", LD = "orange")) +  # Assign colors manually
  labs(title = "Poll Performance of Main Political Parties",
       x = "Date",
       y = "Performance") +
  theme_minimal() +
  theme(legend.position = "right") +
  geom_vline(xintercept = as.numeric(as.Date("2015-01-01")), linetype = "dotted") +  # Add vertical dotted line for Corbyn's election
  geom_text(aes(x = as.Date("2015-01-01"), y = min(filtered_data$Percentage), label = "Corbyn"), 
            vjust = 1.5, hjust = 0, color = "black", size = 3) +  # Add text annotation for Corbyn's election
  geom_vline(xintercept = as.numeric(as.Date("2020-04-01")), linetype = "dotted") +  # Add vertical dotted line for Starmer's election
  geom_text(aes(x = as.Date("2020-04-01"), y = max(filtered_data$Percentage), label = "Starmer"), 
            vjust = -0.5, hjust = 0, color = "black", size = 3)  # Add text annotation for Starmer's election


# Filter the data to include only Lab, Con, and LD
main_parties <- c("Lab", "Con", "LD")
filtered_data <- opinion_polls_long[opinion_polls_long$Party %in% main_parties,]

# Plot the time series scatter plot with filtered data
ggplot(filtered_data, aes(x = Date, y = Percentage, color = Party)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +  # Add LOESS trend lines
  scale_color_manual(values = c(Lab = "red", Con = "blue", LD = "orange")) +  # Assign colors manually
  labs(title = "Poll Performance of Main Political Parties",
       x = "Date",
       y = "Performance") +
  theme_minimal() +
  theme(legend.position = "right")



# Filter the data to include only Lab, Con, and LD, and from 2020-01-01 onwards
main_parties <- c("Lab", "Con", "LD", "Green","Reform")
filtered_data <- opinion_polls_long[opinion_polls_long$Party %in% main_parties & opinion_polls_long$Date >= as.Date("2020-01-01"),]

# Plot the time series scatter plot with filtered data
ggplot(filtered_data, aes(x = Date, y = Percentage, color = Party)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +  # Add LOESS trend lines
  scale_color_manual(values = c(Lab = "red", Con = "blue", LD = "orange", Green = "green", Reform ="pink" )) +  # Assign colors manually
  labs(title = "Poll Performance of Main Political Parties",
       x = "Date",
       y = "Performance") +
  theme_minimal() +
  theme(legend.position = "right")

