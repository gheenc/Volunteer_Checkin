---
editor_options: 
  markdown: 
    wrap: 72
---
# =============================================================================
# Project: Manos Juntos STATCOM - Volunteer Check In/Out
# Script: volunteer_in_out.R
# Author: Caroline Gheen
# Date: 2026-03-01
# Purpose: Clean and prepare dataset for format in master dataset
# =============================================================================

# 1. Packages
# 2. Call in Data and Formatting
# 3. Note about DOB
# 4. Exploring Data
# 5. Merge Datasets with Different Apporoaches (email+date, first+last+date, first+last+dob+date, dob+date)
# 6. Examine Duplicates
# 7. Phone Numbers
# 8. Duration - Total Time Calculation
# 9. Unique ID Assignment

# PACKAGES
# install packages
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("lubridate", repos = "<https://cloud.r-project.org>")
# install.packages("writexl")

# call packages
library(readxl) 
library(dplyr) 
library(lubridate) 
library(stringr)
library(writexl)

# CALL IN DATA AND FORMAT
# read in data, telling each column desired structure
data_in <- read_excel('Volunteer_Checkin_Data.xlsx', 
  sheet ='Sign-In', col_types = c("date", # Timestamp_in 
    "text", # EmailAddress 
    "text", # First Name 
    "text", # Last Name 
    "date", # Date of Birth 
    "text", # Cell Phone Number 
    "text", # Email (non-school...)
    "skip", # verification question 
    "skip", # covid 
    "skip"  # mask ) ) 
#6 had dob that were outside the expected range - default changed these to be NA

#
data_in <- data_in %>% rename(timestamp_in = `Timestamp`, 
    email = `Email Address`, 
    first_name = `First Name`, 
    last_name = `Last Name`, 
    dob = `Date of Birth`, 
    phone = `Cell Phone Number`, 
    email_personal = `Email (non-school email please)` 
)

# trim and standardize all free text (first/last name, email)
data_in <- data_in %>% 
  mutate( 
    first_name = str_to_lower(str_trim(first_name)), 
    last_name = str_to_lower(str_trim(last_name)), 
    email = str_to_lower(str_trim(email)), 
    email_personal = str_to_lower(str_trim(email_personal)) 
)

# read in check out data, telling each column desired structure
data_out <- read_excel('Volunteer_Checkin_Data.xlsx', 
  sheet = 'Sign-Out', 
  col_types = c( 
    "date", # Timestamp 
    "text", # Email Address
    "text", # First Name 
    "text", # Last Name 
    "date", # Date of Birth
    "text", # Cell Phone Number 
    "skip", # verification question 
    "skip", # optional 
    "text" # department ) 
)
# 3 DOB were out of range - default changed to NA

# NOTE ABOUT DOB
## Some DOB were misstyped (ex: year is 0200 instead of 2000, assumably). Because there is a low amount, we could go back and manually correct each if needed. But as long as we don't match on dob, we should be fine to leave as is.

# changes names to be in wanted format
data_out <- data_out %>% 
  rename( 
    timestamp_out = `Timestamp`, 
    email =`Email Address`, 
    first_name = `First Name`, 
    last_name = `Last Name`, 
    dob = `Date of Birth`, 
    phone = `Cell Phone Number`, 
    department =`Which department did you spend the majority of your time volunteering in today?`
)

# trim and standardize all free text (first/last name, email)
data_out <- data_out %>% 
  mutate(first_name = str_to_lower(str_trim(first_name)), 
  last_name = str_to_lower(str_trim(last_name)), 
  email = str_to_lower(str_trim(email))
)

# EXPLORE DATA 
# explore data (sign-in)
dim(data_in) #3805 and 7 variables

# explore data (sign-out)
dim(data_out) #3701 and 9 variables - 104 people did not check out

# show unique values of each to determine how many different people used check in/out
nrow(unique(data_in[c("first_name","last_name")])) #463 people
nrow(unique(data_out[c("first_name","last_name")])) #457 people

# need to extract the date out of the timestamps so that they can match - if you try and match on timestamp outright there will be no matches as timestamp is very specific, down to the second
data_in <- data_in %>% mutate(date = as.Date(timestamp_in))
data_out <- data_out %>% mutate(date = as.Date(timestamp_out))

# MERGING DATASETS TOGETHER 
# merge datasets together
# Used left join so that people who checked in but not out, still remained in the merged dataset
# Goal is for nrow(data_in) == nrow(merged). using data_in as basis so matching all instances; if there are extra rows, it is due to a many-many issue of the matching not being specific enough and R creating all possible combinations (Ex: john and sara both born on same date volunteer same date, R make 4 possible combinations)

# nrow(data_in) = 3805
# look at accidental duplication due to system
## I wanted to try two ways to compare approaches, but only 19 people have an email in sign in... is this true??
sum(!is.na(data_in$email))
sum(is.na(data_in$dob)) nrow(data_in) #3805 
nrow(data_out)

# merge based on same email and date

merged_email <- data_in %>% 
left_join(data_out, by = c("email", "date"))

# merge based on same first, last and date

merged_name <- data_in %>% 
left_join(data_out, by = c("first_name","last_name", "date")) 
nrow(merged_name) # 3821

# merge based on same dob, first, last and date

merged_name_dob <- data_in %>% 
left_join(data_out, by = c("first_name", "last_name", "date", "dob")) 
nrow(merged_name_dob) #3818 - only 13 extra, best so far

# merge based on same dob, and date

merged_dob <- data_in %>% 
left_join(data_out, by = c("date", "dob"))
nrow(merged_dob) # 3894

# EXAMINE DUPLICATES 
## When we merge based on first and last name, there is a warning that some first+last+date combination appears multiple times. It could be people accidentally filling out the check in when they meant to scan the check out; people doing two shifts in one day; or people with the same name. Most likely the first one, but we shouldn't assume until we confirm

# find the duplicate in data_in

data_in %>% 
group_by(first_name, last_name, date) %>% 
filter(n() > 1) %>% 
arrange(last_name, first_name, date)

# find the duplicate in data_out

data_out %>% 
group_by(first_name, last_name, date) %>% 
filter(n() > 1) %>% 
arrange(last_name, first_name, date)

# Finding the 13 extra rows from best model: first+last+date+dob

merged_name_dob %>% 
group_by(first_name, last_name, date, dob) %>%
filter(n() > 1) %>% 
arrange(last_name, first_name, date) %>% 
#View()

## looking at the extra rows in merged based on name and dob, most are people submitting two check outs, a check in instead of a check out, etc. could be easily cleaned if we want to use this model by deleting duplicates, etc.

# This would require manual cleaning and if this is the method we wish to proceed in, could split up individual columns to clean

# If we do this, we do need to go back and clean up the incorrectly typed DOB

## check how many email personal in checkin match email in checkout


# PHONE NUMBERS 
# phone numbers are coming in as scientific notation
## The phone numbers are in the Excel correct, but they are not coming through correctly; we have enough defining features that we should not need the phone numbers, but if we want them to be perfect, we will need to go back and clean them further. Additionally will need to consider if displaying in dashboard. From comparing output to the spreadsheet, I believe it messes up a phone number the same way for all (Ex: Emma S. has a 0 added to the beginning and 1 added to the end of her phone number in all of her entries so her phone number ends up being wrong, but still consistently hers).

# strip all dashes and parentheses to leave only numbers

data_in <- data_in %>% 
mutate(phone = str_replace_all(phone, "[\^0-9]", ""))

# every phone number should have 10 digits

data_in %>% count(nchar(phone)) 
data_in %>% filter(nchar(phone) == 13)

# add back in the leading zeros

df_in <- df_in %>% 
mutate(phone = as.character(as.numeric(phone))) %>% 
mutate(phone = ifelse(nchar(phone) == 9, paste0("0", phone), phone)

# DURATION

# create total time by doing time out - time in

merged_name_dob <- merged_name_dob %>% 
  mutate(
  time_in = as.POSIXct(timestamp_in, format = "%Y-%m-%d %H:%M:%S"), 
  time_out = as.POSIXct(timestamp_out, format = "%Y-%m-%d %H:%M:%S"), 
  duration = as.numeric(difftime(timestamp_out, timestamp_in, units = "mins")),
  total_time = sprintf("%dh %dm", as.integer(duration %/% 60),
  as.integer(duration %% 60)) )

# explore duration data
range(na.omit(merged_name_dob$duration)) # -0.8 - 706.305

# graph duration data
hist(na.omit(merged_name_dob$duration))

# who has negative .08 hours 
merged_name_dob[merged_name_dob$duration == -0.8, ]

# how many are NAs
sum(is.na(merged_name_dob$duration)) # 365 NA - why this many....

# UNIQUE VOLUNTEER ID

# load data
database <- read_excel("Volunteer_Database_2024.xlsx", 
  col_types = c(
  "numeric", # ID 
  "text", # First Name 
  "text", # Last Name 
  "text", # Volunteer Address 1 
  "text", # Volunteer Address 2 
  "text", # City
  "text", # State 
  "text", # Zip Code 
  "text", # Cell 
  "text" # Email ) )

# changes names to be in wanted format

data_in <- data_in %>% 
    rename( 
    ID = `ID`, 
    first_name = `First Name`,
    last_name = `Last Name`, 
    address1 = `VolunteerAddress1`, 
    address2 =`VolunteerAddress2`, 
    city = `Volunteer City`,
    state = `State`, 
    zipcode =`Zip Code`, 
    phone = `Cell`, 
    email = `Volunteer e-mail`,
)

# trim and standardize all free text (first/last name, email)

database <- database %>% 
  mutate( 
    first_name =str_to_lower(str_trim(first_name)), 
    last_name = str_to_lower(str_trim(last_name)), 
    email = str_to_lower(str_trim(email)),
    address1 = str_to_lower(str_trim(address1)), 
    address2 = str_to_lower(str_trim(address2)), 
    city = str_to_lower(str_trim(city)),
    state = str_to_lower(str_trim(state)) 
)

# cycle through ID numbers, drop any duplicates and assign ID numbers to any blanks

# compare to merged dataset and apply next, sequential ID number to those who have not appeared yet

# probably shoudl add leading 0s? look up good practice for id numbers
