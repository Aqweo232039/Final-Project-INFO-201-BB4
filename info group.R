library(dplyr)
library(stringr)
library(testthat)
library(ggplot2)
library(shiny)
df4 <- read.csv("PER_CAPITA_INCOME_and_AGGREGATE_INCOME_IN_THE_PAST_12_MONTHS_(IN_INFLATION-ADJUSTED_DOLLARS).csv")
df10 <- read.csv("TRAVEL_TIME_TO_WORK_(B08303).csv")
Per_capita_income_df<-df4
Influencing_factor_df<-df10
Per_capita_income_and_influencing_factors<-Per_capita_income_df %>%
 # full_join(Influencing_factor_df,by =c( "GEOID","NAME","ACS_VINTAGE"))
full_join(Influencing_factor_df,by =c( "GEOID","NAME","ACS_VINTAGE","JURISDICTION", "CRA_NO", "CRA_GRP", "GEN_ALIAS","DETL_NAMES", "TRACT_LABEL"))
df <- subset(Per_capita_income_and_influencing_factors, select = OBJECTID.y)
#ACS_VINTAGE is year   GEOID IS city code
Trends_in_the_economy <- function(name, last_year) {
  df_last_year <- filter(Per_capita_income_and_influencing_factors, GEOID == name, ACS_VINTAGE == last_year)
  df_first_year <- filter(Per_capita_income_and_influencing_factors, GEOID == name, ACS_VINTAGE == "5Y10")
  if (nrow(df_last_year) == 0 || nrow(df_first_year) == 0) {
    return(NA)
  }
  amount_of_growth <- df_last_year$B19301_001E - df_first_year$B19301_001E
  return(amount_of_growth)
}
#B19301_001E is Per capita income
Per_capita_income_and_influencing_factors <- Per_capita_income_and_influencing_factors %>%
  rowwise() %>%
  mutate(
    Trends_in_the_economy = Trends_in_the_economy(GEOID, ACS_VINTAGE),
    Economic_growth = !is.na(Trends_in_the_economy) && Trends_in_the_economy > 0
  ) %>%
  ungroup()
  
  
  
  
  write.csv(Per_capita_income_and_influencing_factors, file = "Per_capita_income_and_influencing_factors.csv", row.names = FALSE)
  
  
  
  
  
  
  
  
  