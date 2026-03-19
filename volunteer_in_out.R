# =============================================================================
# Project: Manos Juntos STATCOM - Volunteer Check In/Out
# Script: volunteer_in_out.R
# Author: Caroline Gheen
# Date: 2026-03-01
# Purpose: Clean and prepare dataset for format in master dataset
# =============================================================================


# install packages
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("lubridate", repos = "https://cloud.r-project.org")

# call packages
library(readxl)
library(dplyr)
library(lubridate)
library(stringr)

# read in data, telling each column desired structure
data_in <- read_excel(
  'Volunteer_Checkin_Data.xlsx', sheet = 'Sign-In',
  col_types = c(
    "date",    # Timestamp_in
    "text",    # Email Address
    "text",    # First Name
    "text",    # Last Name
    "date",    # Date of Birth
    "text",    # Cell Phone Number
    "text",    # Email (non-school...)
    "skip",    # verification question
    "skip",    # covid
    "skip"     # mask
  )
)
# 6 had dob that were outside the expected range - default changed these to be NA

# changes names to be in wanted format
data_in <- data_in %>%
  rename(
    timestamp_in   = `Timestamp`,
    email       = `Email Address`,
    first_name  = `First Name`,
    last_name   = `Last Name`,
    dob         = `Date of Birth`,
    phone       = `Cell Phone Number`,
    email_personal   = `Email (non-school email please)`
  )

# read in check out data, telling each column desired structure
data_out <- read_excel(
  'Volunteer_Checkin_Data.xlsx', sheet = 'Sign-Out',
  col_types = c(
    "date",    # Timestamp
    "text",    # Email Address
    "text",    # First Name
    "text",    # Last Name
    "date",    # Date of Birth
    "text",    # Cell Phone Number
    "skip",    # verification question
    "skip",    # optional
    "text"     # department
  )
)
# 3 DOB were out of range - default changed to NA

# changes names to be in wanted format
data_out <- data_out %>%
  rename(
    timestamp_out   = `Timestamp`,
    email       = `Email Address`,
    first_name  = `First Name`,
    last_name   = `Last Name`,
    dob         = `Date of Birth`,
    phone       = `Cell Phone Number`,
    department   = `Which department did you spend the majority of your time volunteering in today?`
  )

# fix phone numbers coming in as scientific notation
# strip all dashes and parentheses to leave only numbers 
data_in <- data_in %>%
  mutate(phone = str_replace_all(phone, "[^0-9]", ""))

# every phone number should have 10 digits 
data_in %>% count(nchar(phone))
data_in %>% filter(nchar(phone) == 13)


# add back in the leading zeros
df_in <- df_in %>%
  mutate(phone = as.character(as.numeric(phone))) %>%
  mutate(phone = ifelse(nchar(phone) == 9, paste0("0", phone), phone))

names(data_out)
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

# handle cell phone numbers

# pull each names to be together 
checkin_sorted <- data_in %>% arrange("First Name", "Last Name")

# how many people used the check in 
nrow(unique(checkin_sorted)) #3805

# pull each names to be together 
checkout_sorted <- data_out %>% arrange("First Name", "Last Name")

# how many people used the check out 
nrow(unique(checkout_sorted)) #3701 - less people, some forgot to check out

# merge together based on first and last 
merged <- merge(checkin_sorted, checkout_sorted, by = c("First Name", "Last Name"))

# when i merge, something is happening to timestamp
str(data_in)
str(checkin_sorted)
merged$Timestamp.x <- as.POSIXct(merged$Timestamp.x, origin = "1970-01-01")

# i need to fix dob, telephone

