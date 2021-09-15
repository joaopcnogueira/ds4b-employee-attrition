# DATA UNDERSTANDING ----

# Libraries
library(tidyverse)
library(tidyquant)
library(readxl)
library(skimr)
library(GGally)

# Load Data
path_train            <- "00_Data/telco_train.xlsx"
path_data_definitions <- "00_data/telco_data_definitions.xlsx"

train_raw_tbl <- readxl::read_excel(path_train, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

train_raw_tbl %>% 
  glimpse()

definitions_raw_tbl


# Exploratory Data Analysis (EDA) ----

# Step 1: Data Summarization ----

skim(train_raw_tbl)


# Character Data Type

train_raw_tbl %>% 
  select_if(is.character) %>% 
  glimpse()

train_raw_tbl %>% 
  select_if(is.character) %>% 
  map(unique)

train_raw_tbl %>% 
  select_if(is.character) %>% 
  map(~ table(.) %>% prop.table())

# Numeric Data Type

train_raw_tbl %>% 
  select_if(is.numeric) %>% 
  map(~ unique(.) %>% length())

train_raw_tbl %>% 
  select_if(is.numeric) %>% 
  map_df(~ unique(.) %>% length()) %>% 
  gather() %>% 
  arrange(value) %>% 
  filter(value <= 10)


# Step 2: Data Visualization ----

train_raw_tbl %>% 
  select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>% 
  ggpairs()

train_raw_tbl %>% 
  select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>% 
  ggpairs(aes(color = Attrition), lower = "blank", legend = 1,
          diag = list(continuous = wrap("densityDiag", alpha = 0.5))) +
  theme(legend.position = "bottom")


plot_ggpairs <- function(data, color = NULL, density_alpha = 0.5) {
  
  color_expr <- enquo(color)
  
  if (rlang::quo_is_null(color_expr)) {
    
    g <- data %>%
      ggpairs(lower = "blank") 
    
  } else {
    
    color_name <- quo_name(color_expr)
    
    g <- data %>%
      ggpairs(mapping = aes_string(color = color_name), 
              lower = "blank", legend = 1,
              diag = list(continuous = wrap("densityDiag", 
                                            alpha = density_alpha))) +
      theme(legend.position = "bottom")
  }
  
  return(g)
  
}

train_raw_tbl %>% 
  select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>% 
  plot_ggpairs(color = Attrition)

