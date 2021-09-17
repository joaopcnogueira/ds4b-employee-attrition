# Libraries
import numpy as np
import pandas as pd


# Load Data
train_raw_df = pd.read_excel("00_Data/telco_train.xlsx")
test_raw_df  = pd.read_excel("00_Data/telco_test.xlsx")
definitions_raw_df = pd.read_excel("00_Data/telco_data_definitions.xlsx", skiprows=1)


# Helper function to split one column into two
def separate(data: pd.DataFrame, col: str, sep: str, remove: bool = True) -> pd.DataFrame:
  
  data_col_splitted = data[col].str.split(sep, expand=True)
  return data.join(data_col_splitted).drop(col, axis="columns")



def process_hr_data_readable(df: pd.DataFrame, definitions_df: pd.DataFrame) -> pd.DataFrame:
  
  # Data Cleaning of definitions_raw_df
  definitions_cleaned_df = (
      definitions_df
      .rename(columns={"Unnamed: 0": "column_name", "Unnamed: 1": "column_value"})
      .assign(column_name = lambda df: df['column_name'].interpolate(method='pad'))
      .query("column_value.notna()")
      .pipe(separate, col="column_value", sep=" '")
      .rename(columns={0: "key", 1: "value"})
      .assign(value = lambda df: df["value"].str.replace("'", ""))
      .assign(key = lambda df: df['key'].astype('int64'))
  )  
  
  # Data Cleaning of train_raw_df
  df_merged = (
      df
      .merge(definitions_cleaned_df.query("column_name == 'Education'"), how="left", left_on="Education", right_on="key")
      .assign(Education = lambda df: df["value"])
      .drop(["key", "value", "column_name"], axis="columns")
      
      .merge(definitions_cleaned_df.query("column_name == 'EnvironmentSatisfaction'"), how="left", left_on="EnvironmentSatisfaction", right_on="key")
      .assign(EnvironmentSatisfaction = lambda df: df["value"])
      .drop(["key", "value", "column_name"], axis="columns")
      
      .merge(definitions_cleaned_df.query("column_name == 'JobInvolvement'"), how="left", left_on="JobInvolvement", right_on="key")
      .assign(JobInvolvement = lambda df: df["value"])
      .drop(["key", "value", "column_name"], axis="columns")
      
      .merge(definitions_cleaned_df.query("column_name == 'JobSatisfaction'"), how="left", left_on="JobSatisfaction", right_on="key")
      .assign(JobSatisfaction = lambda df: df["value"])
      .drop(["key", "value", "column_name"], axis="columns")
      
      .merge(definitions_cleaned_df.query("column_name == 'PerformanceRating'"), how="left", left_on="PerformanceRating", right_on="key")
      .assign(PerformanceRating = lambda df: df["value"])
      .drop(["key", "value", "column_name"], axis="columns")
      
      .merge(definitions_cleaned_df.query("column_name == 'RelationshipSatisfaction'"), how="left", left_on="RelationshipSatisfaction", right_on="key")
      .assign(RelationshipSatisfaction = lambda df: df["value"])
      .drop(["key", "value", "column_name"], axis="columns")
      
      .merge(definitions_cleaned_df.query("column_name == 'WorkLifeBalance'"), how="left", left_on="WorkLifeBalance", right_on="key")
      .assign(WorkLifeBalance = lambda df: df["value"])
      .drop(["key", "value", "column_name"], axis="columns")
  )

  return df_merged

if __name__ == '__main__':
  train_readable_df = process_hr_data_readable(train_raw_df, definitions_raw_df)
  test_readable_df = process_hr_data_readable(test_raw_df, definitions_raw_df)
