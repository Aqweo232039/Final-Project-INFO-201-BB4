library(dplyr)
library(stringr)
library(testthat)
library(ggplot2)
library(shiny)
df1 <- read.csv("DISABILITY_CHARACTERISTICS_(S1810).csv")
df2 <- read.csv("HEALTH_INSURANCE_BY_EMPLOYMENT_STATUS_(B27011).csv")
df3 <- read.csv("LANGUAGE_SPOKEN_AT_HOME_FOR_THE_POPULATION_5_YEARS_AND_OVER_(C16001).csv")
df4 <- read.csv("PER_CAPITA_INCOME_and_AGGREGATE_INCOME_IN_THE_PAST_12_MONTHS_(IN_INFLATION-ADJUSTED_DOLLARS).csv")
df5 <- read.csv("POVERTY_STATUS_OF_THE_POPULATION_(B17001).csv")
df6 <- read.csv("Selected_Demographic_and_Housing_Estimates_(DP05).csv")
df7 <- read.csv("Selected_Economic_Characteristics_(DP03).csv")
df8 <- read.csv("Selected_Housing_Characteristics_(DP04).csv")
df9 <- read.csv("Selected_Social_Characteristics_(DP02).csv")
df10 <- read.csv("TRAVEL_TIME_TO_WORK_(B08303).csv")
Per_capita_income_df<-df4
Influencing_factor_df<-df10 %>%
  full_join(df2, by =c( "GEOID","NAME","ACS_VINTAGE")) %>%
  full_join(df3, by =c( "GEOID","NAME","ACS_VINTAGE")) %>%
  full_join(df5, by =c( "GEOID","NAME","ACS_VINTAGE")) %>%
  full_join(df6, by =c( "GEOID","ACS_VINTAGE")) %>%
  full_join(df7, by =c( "GEOID","ACS_VINTAGE")) %>%
  full_join(df8, by =c( "GEOID","ACS_VINTAGE")) %>%
 full_join(df9, by =c( "GEOID","ACS_VINTAGE")) 
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Per_capita_income_and_influencing_factors<-Per_capita_income_df %>%
  full_join(Influencing_factor_df,by =c( "GEOID","NAME","ACS_VINTAGE"))
#ACS_VINTAGE is year   GEOID IS city code 
Trends_in_the_economy<-function(name,last_year){
  df_name_lastyear1<-filter(Per_capita_income_and_influencing_factors,
         GEOID==name,
     )
  df_name_lastyear2<-filter(df_name_lastyear1,
    ACS_VINTAGE==last_year)
  df_name_firstyear1<-filter(Per_capita_income_and_influencing_factors,
                             GEOID==name,
    )  
  df_name_firstyear2<-filter(df_name_firstyear1,
    ACS_VINTAGE=="5Y10"
  )
  Amount_of_growth<-df_name_lastyear2$B19301_001E-df_name_firstyear2$B19301_001E
  return(Amount_of_growth)
}
#B19301_001E is Per capita income
mutate(
  Per_capita_income_and_influencing_factors<-Per_capita_income_df,
  Trends_in_the_economy=0)
for (i in 1:nrow(Per_capita_income_and_influencing_factors)) {
  Per_capita_income_and_influencing_factors$Trends_in_the_economy[i] <-Trends_in_the_economy(Per_capita_income_and_influencing_factors$GEOID[i], Per_capita_income_and_influencing_factors$ACS_VINTAGE[i])
  }

mutate(
  Per_capita_income_and_influencing_factors<-Per_capita_income_df,
  Economic_growth=FALSE)
for (i in 1:nrow(Per_capita_income_and_influencing_factors)) {
  if(Per_capita_income_and_influencing_factors$Trends_in_the_economy[i]>0)
  {Per_capita_income_and_influencing_factors$Economic_growth[i]<-TRUE
  }
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  