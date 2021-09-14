# BUSINESS UNDERSTANDING ----

# Libraries
library(tidyverse)
library(tidyquant)
library(readxl)
library(forcats)
library(stringr)

# Load Data
path_train <- "00_Data/telco_train.xlsx"
train_raw_tbl <- readxl::read_excel(path_train, sheet = 1)


# Data Subset
dept_job_role_tbl <- train_raw_tbl %>% 
  select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)


dept_job_role_tbl


# 1. Business Science Problem Framework ----

# 1A. View Business as a Machine ----

# Business Units: Department and Job Role
# Define Objectives: Retain High Performers
# Assess Outcomes: TBD

dept_job_role_tbl %>% 
  group_by(Attrition) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pct = n / sum(n))

# 1.B Understand the Drivers ----
  
# Investigate Objectives: 16% pct attrition
# Synthesize Outcomes: High Counts and High Percentages
# Hypothesize Drivers: Job Role and Departments
  
# Attrition by Department ----
dept_job_role_tbl %>% 
  
  group_by(Department, Attrition) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  
  group_by(Department) %>% 
  mutate(pct = n / sum(n))

# Attrition by Department and Job Role ----
dept_job_role_tbl %>% 
  
  group_by(Department, JobRole, Attrition) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  
  group_by(Department, JobRole) %>% 
  mutate(pct = n / sum(n)) %>% 
  ungroup() %>% 
  
  filter(Attrition %in% c("Yes")) %>% 
  arrange(desc(pct))
  
# 1C. Measure The Drivers ----

# Collect Information on Employee Attrition: On going task

# Develop KPI'S: Industry KPIs -> 8.8% is the benchmark of attrition for utilities company

dept_job_role_tbl %>% 
  
  group_by(Department, JobRole, Attrition) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  
  group_by(Department, JobRole) %>% 
  mutate(pct = n / sum(n)) %>% 
  ungroup() %>% 
  
  filter(Attrition %in% c("Yes")) %>% 
  arrange(desc(pct)) %>% 
  mutate(above_industry_avg = case_when(pct > 0.088 ~ "Yes", TRUE ~ "No"))


# 1D. Uncover Problems and Opportunities ----

calculate_attrition_cost <- function(
  
  # Employee
  n                    = 1,
  salary               = 80000,
  
  # Direct Costs
  separation_cost      = 500,
  vacancy_cost         = 10000,
  acquisition_cost     = 4900,
  placement_cost       = 3500,
  
  # Productivity Costs
  net_revenue_per_employee = 250000,
  workdays_per_year        = 240,
  workdays_position_open   = 40,
  workdays_onboarding      = 60,
  onboarding_efficiency    = 0.50
  
) {
  
  # Direct Costs
  direct_cost <- sum(separation_cost, vacancy_cost, acquisition_cost, placement_cost)
  
  # Lost Productivity Costs
  productivity_cost <- net_revenue_per_employee / workdays_per_year * 
    (workdays_position_open + workdays_onboarding * onboarding_efficiency) 
  
  # Savings of Salary & Benefits (Cost Reduction)
  salary_benefit_reduction <- salary / workdays_per_year * workdays_position_open
  
  # Estimated Turnover Per Employee
  cost_per_employee <- direct_cost + productivity_cost - salary_benefit_reduction
  
  # Total Cost of Employee Turnover
  total_cost <- n * cost_per_employee
  
  return(total_cost)
  
}
  

# Calculate Cost by Job Role ----

dept_job_role_tbl %>% 
  
  group_by(Department, JobRole, Attrition) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  
  group_by(Department, JobRole) %>% 
  mutate(pct = n / sum(n)) %>% 
  ungroup() %>% 
  
  filter(Attrition %in% c("Yes")) %>% 
  arrange(desc(pct)) %>% 
  mutate(above_industry_avg = case_when(pct > 0.088 ~ "Yes", TRUE ~ "No")) %>% 
  
  mutate(cost_of_attrition = calculate_attrition_cost(n = n)) %>% 
  arrange(desc(cost_of_attrition))
