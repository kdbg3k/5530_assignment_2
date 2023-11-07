#Assignment 2
#Author: Katy Bohanan
#Email: kdbg3k@umsystem.edu

setwd("/Users/katybohanan/Desktop/5530_assignment_2")

df <- read.csv("train.csv")

#set missing values to NA
df[df == ''] <- NA
df[df == 0] <- NA

#find NA values in df
is.na(df)

#create function for extracting numerical values from columns
extract_numeric <- function(attribute) {
  as.numeric(gsub("[^0-9.]", "", attribute))
}

#apply to columns to remove non_numerical data
df$Mileage <- extract_numeric(df$Mileage)
df$Engine <- extract_numeric(df$Engine)
df$Power <- extract_numeric(df$Power)
df$New_Price <- extract_numeric(df$New_Price)

#convert zero values to NA
df[df == 0] <- NA

#impute missing values
df$Mileage <- ifelse(is.na(df$Mileage), mean(df$Mileage, na.rm = TRUE), df$Mileage)
df$Engine <- ifelse(is.na(df$Engine), mean(df$Engine, na.rm = TRUE), df$Engine)
df$Power <- ifelse(is.na(df$Power), mean(df$Power, na.rm = TRUE), df$Power)
df$Seats <- ifelse(is.na(df$Seats), mean(df$Seats, na.rm = TRUE), df$Seats)
### I chose to impute the mean for these columns as there were fewer values missing and mean will be mostly accurate

df$New_Price <- ifelse(is.na(df$New_Price), median(df$New_Price, na.rm = TRUE), df$New_Price)
### The New_Price column had many missing values and median would probably be more accurate than mean due to the smaller sample size

#print(df)

#Change the categorical variables (“Fuel_Type” and “Transmission”) into numerical one hot encoded value.

#install.packages("caret")
library(caret)

columns_to_encode <- c("Fuel_Type", "Transmission")
formula <- as.formula(paste("~", paste(columns_to_encode, collapse = " + ")))
encoded_data <- predict(dummyVars(formula, data = df), newdata = df)

# Add column for Car_Age using mutate function
library(dplyr)

df <- df %>%
  mutate(Car_Age = 2023 - Year)
