library(dplyr)
library(stringr)
library(testthat)
library(ggplot2)
library(shiny)
library(DT)
df4 <- read.csv("PER_CAPITA_INCOME_and_AGGREGATE_INCOME_IN_THE_PAST_12_MONTHS_(IN_INFLATION-ADJUSTED_DOLLARS).csv")
df10 <- read.csv("TRAVEL_TIME_TO_WORK_(B08303).csv")
Per_capita_income_df<-df4
Influencing_factor_df<-df10
Per_capita_income_and_influencing_factors<-Per_capita_income_df %>%
full_join(Influencing_factor_df,by =c( "GEOID","NAME","ACS_VINTAGE","JURISDICTION", "CRA_NO", "CRA_GRP", "GEN_ALIAS","DETL_NAMES", "TRACT_LABEL"))

Trends_in_the_economy <- function(name, last_year) {
  df_last_year <- filter(Per_capita_income_and_influencing_factors, GEOID == name, ACS_VINTAGE == last_year)
  df_first_year <- filter(Per_capita_income_and_influencing_factors, GEOID == name, ACS_VINTAGE == "5Y10")
  if (nrow(df_last_year) == 0 || nrow(df_first_year) == 0) {
    return(NA)
  }
  amount_of_growth <- df_last_year$B19301_001E - df_first_year$B19301_001E
  return(amount_of_growth)
}

Per_capita_income_and_influencing_factors <- Per_capita_income_and_influencing_factors %>%
  rowwise() %>%
  mutate(
    Trends_in_the_economy = Trends_in_the_economy(GEOID, ACS_VINTAGE),
    Economic_growth = !is.na(Trends_in_the_economy) && Trends_in_the_economy > 0
  ) %>%
  ungroup()
  
Per_capita_income_and_influencing_factors<- df <- select(Per_capita_income_and_influencing_factors,-OBJECTID.y)
  
Per_capita_income_and_influencing_factors <- Per_capita_income_and_influencing_factors %>%
  mutate(
    The_average_commute_time = ((B08303_002E * 2.5) + (B08303_003E * 7) + (B08303_004E * 12) + 
                                  (B08303_005E * 17) + (B08303_006E * 22) + (B08303_007E * 27) + 
                                  (B08303_008E * 32) + (B08303_009E * 37) + (B08303_010E * 42) + 
                                  (B08303_011E * 52) + (B08303_012E * 74.5) + (B08303_013E * 90)) / B01001_001E 
  )

Per_capita_income_and_influencing_factors <- Per_capita_income_and_influencing_factors %>%
  group_by(GEOID) %>%
  mutate(
    Changes_in_the_average_commute_time = The_average_commute_time - 
      first(The_average_commute_time[ACS_VINTAGE == "5Y10"])
  ) %>%
  ungroup()
  
Per_capita_income_and_influencing_factors <- Per_capita_income_and_influencing_factors %>%
  mutate(
    Changes_in_average_commute_time_Trends_in_commute_time = Changes_in_the_average_commute_time > 0
  )
df_Plus <- Per_capita_income_and_influencing_factors %>%
  filter(!is.na(Changes_in_the_average_commute_time) & !is.na(Trends_in_the_economy))
df_name <- df_Plus %>%
  distinct(GEOID, NAME)
library(shiny)

ui <- fluidPage(
  titlePanel("Data Analysis Dashboard"),
  
  navbarPage("Navigation",
             tabPanel("Commute Time",
                      selectInput("geoID", "Select Geography ID:", choices = df_name$GEOID),
                      selectInput("vintage", "Select Year:", choices = c("5Y15", "5Y20", "5Y21")),
                      plotOutput("commutePlot"),
                      DTOutput("commuteTable")
             ),
             tabPanel("Impact Chart",
                      selectInput("year", "Select Year for Impact Chart:", choices = c("5Y15", "5Y20", "5Y21")),
                      plotOutput("impactChart")
             ),
             tabPanel("Average Wage Change",
                      selectInput("geoIDWage", "Select Geography ID:", choices = df_name$GEOID),
                      plotOutput("wagePlot")
             )
  )
)
server <- function(input, output) {

  output$commutePlot <- renderPlot({

    selected_data <- filter(df_Plus, GEOID == input$geoID, ACS_VINTAGE == input$vintage)
    ggplot(selected_data, aes(x = 1, y =1 )) +
      geom_bar(stat = "identity")
  })
  
  output$commuteTable <- renderDT({

    selected_data <- filter(df_Plus, GEOID == input$geoID, ACS_VINTAGE == input$vintage)
    datatable(selected_data)
  })
  output$impactChart <- renderPlot({

    selected_year_data <- filter(df_Plus, ACS_VINTAGE == input$year)

    ggplot(selected_year_data, aes(x = "", fill = interaction(Economic_growth, Changes_in_average_commute_time_Trends_in_commute_time))) +
      geom_bar(width = 1) +
      coord_polar(theta = "y") +
      theme_void()
  })

  output$wagePlot <- renderPlot({

    selected_geo_data <- filter(df_Plus, GEOID == input$geoIDWage)

    ggplot(selected_geo_data, aes(x = 1, y =1)) + 
      geom_bar(stat = "identity")
  })
}
shinyApp(ui, server)