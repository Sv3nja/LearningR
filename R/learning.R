
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


# Exercise 7.12 -----------------------------------------------------------

# 1. BMI between 20 and 40 with diabetes
nhanes_small %>%
    # Format should follow: variable >= number or character
    filter(bmi >= 20 & bmi<= 40 & diabetes == "Yes")

# Pipe the data into mutate function and:
nhanes_modified <- nhanes_small %>% # Specifying dataset
    mutate(
        # 2. Calculate mean arterial pressure
        ___ = ___,
        # 3. Create young_child variable using a condition
        ___ = if_else(___, "Yes", "No")
    )

nhanes_modified


#1 Filter nhanes_small so only those participants with a BMI of more
#than or equal to 20 and less than or equal to 40, and keep those who have diabetes.

nhanes_small %>%
    filter(bmi >= 20 & bmi<= 40 & diabetes == "Yes")

#2 Create a new variable called mean_arterial_pressure by applying the formula:
#(DBP = bp_dia_ave and SBP = bp_sys_ave) to calculate Mean Arterial Pressure.
#Hint: In R, use + to add, * to multiply, and / to divide.

nhanes_modified <- nhanes_small %>%
    mutate(MAP=((2*bp_sys_ave)+bp_dia_ave)/3)
#3: Create a new variable called young_child for cases where age is less than 6 years.

nhanes_modified <- nhanes_small %>% # dataset
    mutate(
        mean_arterial_pressure = ((2 * bp_dia_ave) + bp_sys_ave) / 3,
        young_child = if_else(age < 6, "Yes", "No")
    )

nhanes_modified



# Creating summary statistics ---------------------------------------------

nhanes_small %>%
    summarise(max_bmi = max(bmi)) #it has missing values

nhanes_small %>%
    summarise(max_bmi = max(bmi, na.rm = TRUE),#it should remove (rm) all na (na)values
              min_bmi = min(bmi, na.rm = TRUE))

nhanes_small %>%
    filter(!is.na(diabetes)) %>%
    group_by(diabetes) %>%
    summarise(mean_age = mean(age, na.rm=TRUE),
              mean_bmi = mean(bmi, na.rm=TRUE)) %>%
    ungroup() #it's good practice to ungroup, especially whem you make objects

# Saving data -------------------------------------------------------------
readr::write_csv(nhanes_small,
                 here::here("data/nhance_small.csv"))




