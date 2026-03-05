# =============================================================================
# Project: Manos Juntos STATCOM - Volunteer Check In/Out
# Script: volunteer_in_out.R
# Author: Caroline Gheen
# Date: 2026-03-01
# Purpose: Clean and prepare dataset for format in master dataset
# =============================================================================


# install packages
# install.packages("readxl")
#install.packages("dplyr")

# call packages
library(readxl)
library(dplyr)

# read in data
data_in <- read_excel('Volunteer_Checkin_Data.xlsx', sheet = 'Sign-In')
data_out <- read_excel('Volunteer_Checkin_Data.xlsx', sheet = 'Sign-Out')


# explore data (sign-in)
dim(data_in) #3805 and 10 variables
names(data_in) #time, email, first, last, dob, cell, email, verification, covid, mask

# explore data (sign-out)
dim(data_out) #3701 and 9 variables - 100 people did not check out
names(data_out) # time, email, first, last, dob, cell, verification, survey, department

# drop unnecessary columns sign in 
data_in <- data_in[, c("Timestamp", "Email Address", "First Name", "Last Name", "Date of Birth", "Cell Phone Number", "Email (non-school email please)")]

# rename timestamp 
data_in <- data_in %>%
  rename(Timestamp_in = Timestamp)

# drop unnecessary columns sign out 
data_out <- data_out[, c("Timestamp", "Email Address", "First Name", "Last Name", "Date of Birth", "Cell Phone Number")]

# rename timestamp 
data_out <- data_out %>%
  rename(Timestamp_out = Timestamp)

# show unique values of each 
nrow(unique(data_in[c("First Name","Last Name")])) #476 people 
nrow(unique(data_out[c("First Name","Last Name")])) #476 people 

# pull each names to be together 
checkin_sorted <- data_in %>% arrange("First Name", "Last Name")

# how many people used the check in 
nrow(unique(checkin_sorted)) #3805

# pull each names to be together 
checkout_sorted <- data_out %>% arrange("First Name", "Last Name")

# how many people used the check out 
nrow(unique(checkout_sorted)) #3701 - less people forgot to check out

# merge together based on first and last 
merged <- merge(checkin_sorted, checkout_sorted, by = c("First Name", "Last Name"))

# when i merge, something is happening to timestamp
str(data_in)
str(checkin_sorted)
merged$Timestamp.x <- as.POSIXct(merged$Timestamp.x, origin = "1970-01-01")

# i need to fix dob, telephone
