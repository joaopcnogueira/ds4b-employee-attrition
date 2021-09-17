# H2O MODELING ----

# 1. Setup ----

# Load Libraries
library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)
library(stringr)
library(forcats)
library(cowplot)
library(fs)
library(glue)

# Load Data
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

# ML Preprocessing

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>% 
  step_zv(all_predictors()) %>% 
  step_mutate_at(c("JobLevel", "StockOptionLevel"), fn = as.factor) %>% 
  prep()
  
recipe_obj

train_tbl <- recipe_obj %>% bake(train_readable_tbl)
test_tbl  <- recipe_obj %>% bake(test_readable_tbl)


# 2. Modeling ----

h2o.init()

# converting tibbles to h2o objects
split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.85), seed = 1234)

train_h2o <- split_h2o[[1]]   # used to develop the model
valid_h2o <- split_h2o[[2]]   # used to tune hyperparameters via grid search
test_h2o  <- as.h2o(test_tbl) # test set completely held out from model training & tuning

# target and features names
y <- "Attrition"
X <- setdiff(names(train_h2o), y)

# run automl
automl_models_h2o <- h2o.automl(
  x = X,
  y = y,
  training_frame = train_h2o,
  validation_frame = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs = 30,
  nfolds = 5
)

typeof(automl_models_h2o)

slotNames(automl_models_h2o)

# leaderboard is made on test data
automl_models_h2o@leaderboard 

automl_models_h2o@leader

# Extracts and H2O model name by a position so can more easily use h2o.getModel()
extract_h2o_model_name_by_position <- function(h2o_leaderboard, n = 1, verbose = TRUE) {
  
  model_name <- h2o_leaderboard %>%
    as_tibble() %>%
    slice(n) %>%
    pull(model_id) 
  
  if (verbose) message(model_name)
  
  return(model_name)
  
}

automl_models_h2o@leaderboard %>% 
  extract_h2o_model_name_by_position(2)

# Saving Model ----
automl_models_h2o@leaderboard %>% 
  as_tibble() %>% 
  view()


h2o.getModel("DeepLearning_grid__1_AutoML_20210916_220156_model_3") %>% 
  h2o.saveModel(path = "04_Modeling/h2o_models/employee_attrition/")

h2o.getModel("StackedEnsemble_BestOfFamily_AutoML_20210916_220156") %>% 
  h2o.saveModel(path = "04_Modeling/h2o_models/employee_attrition/")

h2o.getModel("GLM_1_AutoML_20210916_220156") %>% 
  h2o.saveModel(path = "04_Modeling/h2o_models/employee_attrition/")

h2o.getModel("GBM_grid__1_AutoML_20210916_220156_model_8") %>% 
  h2o.saveModel(path = "04_Modeling/h2o_models/employee_attrition/")

# Load Model ----
h2o.loadModel("04_Modeling/h2o_models/employee_attrition/DeepLearning_grid__1_AutoML_20210916_220156_model_3")

# Make Predictions ----
deeplearning_h2o <- h2o.loadModel("04_Modeling/h2o_models/employee_attrition/DeepLearning_grid__1_AutoML_20210916_220156_model_3")
deeplearning_h2o

predictions_tbl <- deeplearning_h2o %>% 
  h2o.predict(newdata = as.h2o(test_tbl)) %>% 
  as_tibble()

predictions_tbl


# Inspecting Model Parameters ----
deeplearning_h2o <- h2o.loadModel("04_Modeling/h2o_models/employee_attrition/DeepLearning_grid__1_AutoML_20210916_220156_model_3")
deeplearning_h2o

deeplearning_h2o@parameters


# 3. Visualizing the Leaderboard ----

data_transformed <- automl_models_h2o@leaderboard %>% 
  as_tibble() %>% 
  mutate(model_type = str_split(model_id, "_", simplify = T) %>% .[,1]) %>% 
  slice(1:20) %>% 
  rownames_to_column() %>% 
  mutate(
    model_id   = as_factor(model_id) %>% reorder(auc),
    model_type = as_factor(model_type) 
  ) %>% 
  gather(key = key, value = value, -c(model_id, model_type, rowname), factor_key = T) %>% 
  filter(key %in% c("auc", "logloss")) %>% 
  mutate(model_id = paste0(rowname, ". ", model_id) %>% as_factor() %>% fct_rev())


data_transformed %>% 
  ggplot(aes(value, model_id, color = model_type)) +
  geom_point(size = 3) +
  geom_label(aes(label = round(value, 2), hjust = "inward")) +
  facet_wrap(~ key, scales = "free_x") +
  theme_tq() +
  scale_color_tq() +
  labs(title = "H2O Leaderboard Metrics",
       subtitle = paste0("Ordered by: AUC"),
       y = "Model Position, Model ID", x = "")


# using the function provided
source("00_Scripts/h2o_modeling.R")

automl_models_h2o@leaderboard %>% 
  plot_h2o_leaderboard(order_by = "auc")


# 4. Grid Search ----
deeplearning_h2o <- h2o.loadModel("04_Modeling/h2o_models/employee_attrition/DeepLearning_grid__1_AutoML_20210916_220156_model_3")
deeplearning_h2o

h2o.performance(deeplearning_h2o, newdata = as.h2o(test_tbl))

?h2o.grid()

deeplearning_grid_01 <- h2o.grid(
  algorithm = "deeplearning",
  x = X,
  y = y,
  grid_id = "deeplearning_grid_01",
  training_frame = train_h2o,
  nfolds = 5,
  hyper_params = list(
    hidden = list(c(10, 10, 10), c(50, 20, 10), c(20, 20, 20)),
    epochs = c(10, 50, 100)
  )
)

deeplearning_grid_01

h2o.getGrid(grid_id = "deeplearning_grid_01", sort_by = "auc", decreasing = TRUE)

deeplearning_grid_01_model_7 <- h2o.getModel("deeplearning_grid_01_model_7")

deeplearning_grid_01_model_7 %>% 
  h2o.auc(train = T)

h2o.performance(deeplearning_grid_01_model_7, newdata = as.h2o(test_tbl))
