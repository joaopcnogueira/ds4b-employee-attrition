# Setup ----

# Libraries
library(readxl)
library(tidyverse)
library(tidyquant)
library(recipes)

# Data
path_train            <- "00_Data/telco_train.xlsx"
path_test             <- "00_Data/telco_test.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl <- read_excel(path_train, sheet = 1)
test_raw_tbl  <- read_excel(path_test, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

# Processing Pipeline
source("00_Scripts/data_processing_pipeline.R")
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)

# Plot Faceted Histogram Function ----
source("00_Scripts/plot_hist_facet.R")

train_raw_tbl %>% 
  plot_hist_facet(bins = 10, ncol = 5)


# Data Preprocessing with Recipes ----

# Plan: Correlation Analysis

# 1. Zero Variance Features ----

# create the instructions with recipes and steps
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>% 
  step_zv(all_predictors())

recipe_obj

# prepare the recipe
recipe_obj %>% 
  prep()

# bake new data
recipe_obj %>% 
  prep() %>%
  bake(new_data = train_readable_tbl)

recipe_obj %>% 
  prep() %>%
  bake(new_data = test_readable_tbl)


# 2. Transformations ----

skewed_feature_names <- train_readable_tbl %>% 
  select_if(is.numeric) %>% 
  map_df(skewness) %>% 
  gather() %>% 
  arrange(desc(value)) %>% 
  filter(value >= 0.8) %>% 
  pull(key) %>% 
  as.character()

train_readable_tbl %>% 
  select(skewed_feature_names) %>% 
  plot_hist_facet()

!skewed_feature_names %in% c("JobLevel", "StockOptionLevel")

skewed_feature_names <- train_readable_tbl %>% 
  select_if(is.numeric) %>% 
  map_df(skewness) %>% 
  gather() %>% 
  arrange(desc(value)) %>% 
  filter(value >= 0.8) %>% 
  filter(!key %in% c("JobLevel", "StockOptionLevel")) %>% 
  pull(key) %>% 
  as.character()

skewed_feature_names

factor_names <- c("JobLevel", "StockOptionLevel")

factor_names


recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>% 
  step_zv(all_predictors()) %>% 
  step_YeoJohnson(skewed_feature_names) %>% 
  step_mutate_at(factor_names, fn = as.factor)

recipe_obj %>% 
  prep() %>% 
  bake(train_readable_tbl) %>% 
  select(skewed_feature_names) %>% 
  plot_hist_facet()


# 3. Center/Scaling ----

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>% 
  step_zv(all_predictors()) %>% 
  step_YeoJohnson(skewed_feature_names) %>% 
  step_mutate_at(factor_names, fn = as.factor) %>% 
  step_center(all_numeric()) %>% 
  step_scale(all_numeric())

recipe_obj$steps[[4]]

prepared_recipe <- recipe_obj %>% 
  prep()

prepared_recipe$steps[[4]]

prepared_recipe %>% 
  bake(train_readable_tbl)


# 4. Dummy Variables ----

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>% 
  step_zv(all_predictors()) %>% 
  step_YeoJohnson(skewed_feature_names) %>% 
  step_mutate_at(factor_names, fn = as.factor) %>% 
  step_center(all_numeric()) %>% 
  step_scale(all_numeric()) %>% 
  step_dummy(all_nominal())

recipe_obj %>% 
  prep() %>% 
  juice() %>% 
  select(contains("JobRole")) %>% 
  glimpse()


# Final Recipe ----

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>% 
  step_zv(all_predictors()) %>% 
  step_YeoJohnson(skewed_feature_names) %>% 
  step_mutate_at(factor_names, fn = as.factor) %>% 
  step_center(all_numeric()) %>% 
  step_scale(all_numeric()) %>% 
  step_dummy(all_nominal()) %>% 
  prep()

recipe_obj

train_tbl <- recipe_obj %>% 
  bake(train_readable_tbl)

test_tbl <- recipe_obj %>% 
  bake(test_readable_tbl)


# Correlation Analysis ----
source("00_Scripts/plot_cor.R")

train_tbl %>% 
  get_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)

train_tbl %>% 
  plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)
  

train_tbl %>% 
  select(Attrition_Yes, contains("JobRole")) %>% 
  plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)










