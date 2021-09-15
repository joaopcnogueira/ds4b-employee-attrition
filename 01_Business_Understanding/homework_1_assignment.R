# HOMEWORK 1 ----

# Libraries ----
library(tidyverse)
library(tidyquant)
library(readxl)
library(forcats)
library(stringr)

# Source Scripts ----
source("00_Scripts/assess_attrition.R")

# Data ----
path_train     <- "00_Data/telco_train.xlsx"
train_raw_tbl  <- read_excel(path_train, sheet = 1)

dept_jobrole_tbl <- train_raw_tbl %>%
    select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)

kpi_industry_turnover_pct <- 0.088

# Productivity Cost by Role ----

productivity_cost_by_role_tbl <- read_excel("00_Data/productivity_cost_by_role.xlsx")
productivity_cost_by_role_tbl


# Q1: Which Job Role has the highest total cost of attrition? ----

dept_jobrole_productivity_tbl <- dept_jobrole_tbl %>% 
  count(Department, JobRole, Attrition) %>% 
  count_to_pct(Department, JobRole) %>% 
  assess_attrition(Attrition, "Yes", baseline_pct = kpi_industry_turnover_pct) %>% 
  
  left_join(productivity_cost_by_role_tbl, by = c("Department", "JobRole")) %>% 
  
  mutate(attrition_cost = calculate_attrition_cost(n = n,
                                                   salary = Salary_Average,
                                                   net_revenue_per_employee = Revenue_Average))

  
dept_jobrole_productivity_tbl %>% 
  plot_attrition(Department, JobRole, .value = attrition_cost)


# Q2: What is the total cost of attrition for the Research & Development: Research Scientist job role? ----

dept_jobrole_productivity_tbl %>% 
  plot_attrition(Department, JobRole, .value = attrition_cost, units = "M")


# Q3: What percentage do the top four Job Roles account for in terms of the total cost of attrition? ----

dept_jobrole_productivity_tbl %>% 
  
  # Data Manipulation
  arrange(desc(attrition_cost)) %>% 
  mutate(row_number = row_number()) %>% 
  mutate(is_top_4 = case_when(row_number <= 4 ~ "Yes", TRUE ~ "No")) %>% 
  
  # Summarize Cost by Top 4
  group_by(is_top_4) %>% 
  summarise(total_attrition_cost = sum(attrition_cost)) %>% 
  ungroup() %>% 
  
  # Calculate percentage of total
  mutate(total_attrition_pct = total_attrition_cost / sum(total_attrition_cost))
  
  
# Alternative Method
dept_jobrole_productivity_tbl %>% 
  # Rank
  arrange(desc(attrition_cost)) %>%
  mutate(cumulative_attrition = cumsum(attrition_cost)) %>% 
  mutate(cumulative_percent = cumulative_attrition / sum(attrition_cost)) %>% 
  select(Department, JobRole, n, attrition_cost:cumulative_percent)
  


# Q4. Which Department has the highest total cost of attrition? ----



# Q5: What percentage does the top Department account for in terms of the total cost of attrition? ----


