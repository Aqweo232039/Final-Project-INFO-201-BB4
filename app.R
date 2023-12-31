library(dplyr)
library(stringr)
library(testthat)
library(ggplot2)
library(shiny)
library(DT)
library(tidyr)
library(rsconnect)

# deploy io information here:
rsconnect::setAccountInfo(name='arcanaj',
                          token='28247543C5089681F5FEFB5BA462DFD9',
                          secret='yvVHNSHW/8VQkHePJSqK75+B6Gdabxp4j0hR/elo')

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
  distinct(GEOID, NAME,DETL_NAMES)
# ------------- Below here is data integration part ---------------
# ------------- Here goes to shinyApp part ----------------

ui <- fluidPage(
  titlePanel("Data Analysis Dashboard"),
  
  navbarPage("Navigation",
             tabPanel("Introduction",
                    helpText("In the hustle and bustle of today's fast-paced world, the time individuals spend commuting to work is a critical factor influencing their personal economic growth. This relationship between daily travel and financial well-being is an important aspect of modern life and prosperity.

Commuting time is more than just minutes on the clock, it's a key element in the decisions individuals make about where to live and work. As cities expand and job markets become more competitive, understanding how the daily journey affects one's economic trajectory is essential.

This exploration aims to illustrate on the connection between commuting time and personal economic growth. We'll delve into the ways in Seattle  in which the time spent in transit impacts personal economic development."),
                    helpText("Therefore, we utilized data from the ", HTML ("<a href ='https://catalog.data.gov/dataset/travel-time-to-work-b08303'> American Community Survey (ACS)</a>"), "for the years 2010, 2015, 2020, and 2021, aggregating survey reports based on Geography IDs for investigation."),
                    helpText("Additionally, we acquired data from the ", HTML("<a href = 'https://catalog.data.gov/dataset/per-capita-income-and-aggregate-income-in-the-past-12-months-in-inflation-adjusted-dollars'> Per Capita Income and Aggregate Income in the Past 12 Months in Inflation-Adjusted Dollars </a>"),"."),
                    
                    helpText(" They include data on total income per person and commuting times to work for each 
area in Seattle and surrounding areas.")
                    ),
             
             tabPanel("Commute Time",
                      fluidRow(
                        column (4,
                                selectInput ("geoID", "Select Geography ID:", choices = df_name$GEOID),
                                textOutput("selectedTractName"),
                                textOutput("selectedAreaName"),
                                selectInput("vintage","Select Year:", choices = c('2015'='5Y15','2020'='5Y20','2021'='5Y21')),
                                helpText ("In this section, you can select a specific Geography ID. Below the ID, you will see the Tract Name and the actual location of this ID. Then, you can choose a Year to view data. 
                                          The data displayed on the right indicates the population distribution of commute times 
                                          for the selected location in the chosen year, along with the number of people in each sample category.")
                                ),
                        column(6,
          
                               plotOutput("commutePlot")
                               )
                      )
                      
             ),
             tabPanel("Average Wage Change",
                      fluidRow(
                        column (4,
                                selectInput("geoIDWage", "Select Geography ID:", choices = df_name$GEOID),
                     
                                helpText ("In this section, you can select a specific Geography ID to observe changes in the per capita annual income levels in that area from 2010 to 2015, then to 2020 and 2021. 
                                          Please note that our data is sourced from the American Community Survey (ACS), and the ACS reports are only available for the years 2010, 2015, 2020, and 2021. 
                                          Therefore, the data we provide is not continuous over the years. Please not that for some areas, data from 2015 to 2021 is not avaliable due to missing information
                                          in the ACS (American Community Survey)")
                        ),
                        column(6,
                               # selectInput("vintageWage","Select Year:",
                               #             choice = c('2015'='5Y15','2020'='5Y20','2021'='5Y21')),
                               plotOutput("wagePlot")
                               )
                      ),
             ),
             tabPanel("Impact Chart",
                      selectInput("year", "Select Year for Impact Chart:", choices = c('2015'='5Y15', '2020'='5Y20', '2021'='5Y21')),
                      helpText("In this section, you will select three years: 2015, 2020, and 2021. 
                               These years will be compared with the data from 2010, and the comparison will be displayed in the pie chart on the right. 
                               In the pie chart, the first True or False value indicates whether there has been economic growth in your selected year compared to 2010. 
                               The second True or False value shows whether the average commute time has increased in your selected year compared to 2010."),
                      plotOutput("impactChart"),
             ),
           tabPanel("Conclusion",
                    helpText("According to the first figure, it can be seen that in 2015,2020 and 2021, the largest proportion of attendance time is 20-24 minutes and 30-34 minutes. 
                  There are also many people's attendance time is 45-59 minutes. Which means most of people are not live closed to their workplace.
                  The increase of commuting time and personal economic growth have little impact. Even though we can see in the chart that most of the people in the chart have high personal economic growth as their commute time goes up. 
                  But we can't lose sight of the fact that the average wage also goes up from year to year. So travel time is not a major consideration for economic growth."),
           )
        )
)

server <- function(input, output) {
  
  output$selectedTractName <- renderText({
    selected_geo <- filter(df_name, GEOID == input$geoID)
    selected_geo$NAME 
  })
  
  output$selectedAreaName <- renderText({
    selected_geo <- filter(df_name, GEOID == input$geoID)
    selected_geo$DETL_NAMES
  })
  
  output$commutePlot <- renderPlot({
    
    selected_data <- filter(df_Plus, GEOID == input$geoID, ACS_VINTAGE == input$vintage)
    
    selected_data_long <- pivot_longer(selected_data, 
                                       cols = starts_with("B08303_"), 
                                       names_to = "Commute_Category", 
                                       values_to = "Count")
    commute_labels <- c(B08303_001E = "Total popular",
                        B08303_002E = "Less than 5 minutes commute",
                        B08303_003E = "5 to 9 minutes commute",
                        B08303_004E = "10 to 14 minutes commute",
                        B08303_005E = "15 to 19 minutes commute",
                        B08303_006E = "20 to 24 minutes commute",
                        B08303_007E = "25 to 29 minutes commute",
                        B08303_008E = "30 to 34 minutes commute",
                        B08303_009E = "35 to 39 minutes commute",
                        B08303_010E = "40 to 44 minutes commute",
                        B08303_011E = "45 to 59 minutes commute",
                        B08303_012E = "60 to 89 minutes commute",
                        B08303_013E = "90 or more minutes commute")
    
    selected_data_long$Commute_Category <- recode(selected_data_long$Commute_Category, !!!commute_labels)
    
    ggplot(selected_data_long, aes(x = Commute_Category, y = Count)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label=Count), vjust = -0.5)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      labs(x = "Commute Time", y = "Number of People", title = "Commute Time Distribution")
  })
  
  
  output$impactChart <- renderPlot({
    
    selected_year_data <- filter(df_Plus, ACS_VINTAGE == input$year)
    
    selected_year_data$group <- interaction(selected_year_data$Economic_growth,selected_year_data$Changes_in_average_commute_time_Trends_in_commute_time)
    
    group_freq <- selected_year_data %>%
      count(group) %>%
      mutate(freq = n/sum(n))
    
    ggplot(group_freq, aes(x = "", fill = group, y=freq)) +
      geom_bar(stat = "identity", width = 1) +
      geom_text (aes(label = scales::percent(freq)),position =position_stack(vjust = 0.5)) + coord_polar(theta = "y")+
      theme_void()
  })
  
  output$wagePlot <- renderPlot({
    selected_geo_data <- filter(df_Plus, GEOID == input$geoIDWage)
    vintage_map <- c('5Y10'= '2010', '5Y15'='2015','5Y20'='2020','5Y21'='2021')
    selected_geo_data$ACS_VINTAGE <- vintage_map[selected_geo_data$ACS_VINTAGE]
    ggplot(selected_geo_data, aes(x = ACS_VINTAGE, y = B19301_001E, group = GEOID)) +
      geom_line() + geom_point() +
      labs(x = "Year", y = "Average Wage", title = "Average Wage Change Over Years")
  })
}

shinyApp(ui, server)

rsconnect::deployApp(appDir = "C:/Users/Lenovo/Desktop/UW/LIGHT course/CSE141/Final-Project-INFO-201-BB4")
