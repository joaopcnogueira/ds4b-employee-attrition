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

# 1. Setup
abt_tbl   <- read_csv("00_Data/propensao_revenda_abt.csv")
train_raw_tbl <- abt_tbl %>% filter(data_ref_safra < "2018-03-01")
test_raw_tbl  <- abt_tbl %>% filter(data_ref_safra == "2018-03-01")

recipe_obj <- recipe(nao_revendeu_next_6m ~ ., data = train_raw_tbl) %>% 
  step_mutate_at(nao_revendeu_next_6m, fn = as.factor) %>%
  step_rm(data_ref_safra, seller_id) %>% 
  prep()

train_tbl <- recipe_obj %>% bake(train_raw_tbl)
test_tbl  <- recipe_obj %>% bake(test_raw_tbl)


# 2. Modeling ----

h2o.init()

# converting tibbles to h2o objects
train_h2o <- as.h2o(train_tbl)
test_h2o  <- as.h2o(test_tbl) # test set completely held out from model training & tuning

# target and features names
y <- "nao_revendeu_next_6m"
X <- setdiff(names(train_h2o), y)

# run automl
automl_models_h2o <- h2o.automl(
  x = X,
  y = y,
  training_frame = train_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs = 30,
  nfolds = 5
)

slotNames(automl_models_h2o)

# leaderboard is made on test data
automl_models_h2o@leaderboard

automl_models_h2o@leaderboard %>% 
  as_tibble() %>% 
  view()

automl_models_h2o@leader

# getting a specific model
h2o.getModel(model_id = "GBM_3_AutoML_20210916_213405")


automl_models_h2o@leaderboard %>% 
  as_tibble() %>% 
  slice(1) %>% 
  pull(model_id) %>% 
  h2o.getModel()


extract_h2o_model_name_by_position <- function(h2o_leaderboard, n = 1, verbose = TRUE) {
  
  model_id <- h2o_leaderboard %>% 
    as_tibble() %>% 
    slice(n) %>% 
    pull(model_id)
  
  if (verbose) message(model_id)
  
  return(model_id)
  
}

automl_models_h2o@leaderboard %>% 
  extract_h2o_model_name_by_position(2)


# Saving Model ----
h2o.getModel("StackedEnsemble_AllModels_AutoML_20210916_213405") %>% 
  h2o.saveModel(path = "04_Modeling/h2o_models/propensao_revenda/")

h2o.getModel("GBM_3_AutoML_20210916_213405") %>% 
  h2o.saveModel(path = "04_Modeling/h2o_models/propensao_revenda/")

h2o.getModel("DeepLearning_1_AutoML_20210916_213405") %>% 
  h2o.saveModel(path = "04_Modeling/h2o_models/propensao_revenda/")

h2o.getModel("GLM_1_AutoML_20210916_213405") %>% 
  h2o.saveModel(path = "04_Modeling/h2o_models/propensao_revenda/")


# Load Model ----
h2o.loadModel("04_Modeling/h2o_models/propensao_revenda/DeepLearning_1_AutoML_20210916_213405")


# Make Predictions ----
stacked_ensemble_h2o <- h2o.loadModel("04_Modeling/h2o_models/propensao_revenda/StackedEnsemble_AllModels_AutoML_20210916_213405")
stacked_ensemble_h2o

predictions_tbl <- stacked_ensemble_h2o %>% 
  h2o.predict(newdata = as.h2o(test_tbl)) %>% 
  as_tibble()

predictions_tbl


