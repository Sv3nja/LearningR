
# Data Management and Wrangling -----------------------------------------------------------

# Load packages
library(tidyverse)
library(NHANES)

# Looking at data
glimpse(NHANES)

# selecting columns
select(NHANES, Age)

select(NHANES, Age, Weight, BMI)

select(NHANES, -HeadCirc)

select(NHANES, starts_with("BP"))

select(NHANES, ends_with("Day"))

select(NHANES, contains("Age"))

# Create smaller NHANES dataset
nhanes_small <- select(
  NHANES, Age, Gender, BMI, Diabetes, PhysActive, BPSysAve,
  BPDiaAve, Education
)

# Renaming columns
nhanes_small <- rename_with(
  nhanes_small,
  snakecase::to_snake_case
)

# Renaming specific columns
nhanes_small <- rename(nhanes_small, sex = gender)

# Trying out the pipe

colnames(nhanes_small)

nhanes_small %>%
  colnames()

nhanes_small %>%
  select(phys_active) %>%
  rename(physically_active = phys_active)

# Exercise 7.8

# 1
nhanes_small %>%
  select(bp_sys_ave, education)

# 2
nhanes_small %>%
  rename(
    bp_sys = bp_sys_ave,
    bp_dia = bp_dia_ave
  )

# 3 rewrite: select(nhanes_small, bmi, contains("age"))
nhanes_small %>%
  select(bmi, contains("age"))

nhanes_small %>%
  select(bmi, age)

# 4 blood_pressure <- select(nhanes_small, starts_with("bp_"))
# rename(blood_pressure, bp_systolic = bp_sys_ave)

blood_pressure <- nhanes_small %>%
  select(starts_with("bp_")) %>%
  rename(bp_systolic = bp_sys_ave)

# Filtering
nhanes_small %>%
  filter(phys_active != "No")

nhanes_small %>%
  filter(bmi >= 25)

# combining logical operators
nhanes_small %>%
  filter(bmi >= 25 & phys_active == "No")

nhanes_small %>%
  filter(bmi >= 25 | phys_active == "No")

# Arrange data

nhanes_small %>%
  arrange(age)

nhanes_small %>%
  arrange(desc(age))

nhanes_small %>%
  arrange(education, age)


# Transform data

nhanes_small %>%
  mutate(
    age = age * 12, # having the age in months and not years
    logged_bmi = log(bmi)
  )

nhanes_small %>%
  mutate(old = if_else(age >= 30, "Yes", "No"))


