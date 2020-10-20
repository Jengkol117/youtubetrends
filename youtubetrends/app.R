library(lubridate)
library(tidyverse)
library(plotly)
library(scales)
library(glue)
library(treemapify)
library(shiny)
library(shinydashboard)

youtube <- read.csv("youtubetrends.csv")
youtube_clean <- youtube %>% 
    mutate(trending_date = ymd(trending_date),
           publish_time = ymd_hms(publish_time),
           publish_month = floor_date(publish_time, "month"),
           trending_month = floor_date(trending_date, "month"),
           year = year(trending_date)) %>% 
    mutate_at(vars(category_id, channel_title, publish_when, timetotrend), as.factor) %>% 
    filter(video_error_or_removed == F & comments_disabled == F & ratings_disabled == F)
top9 <- youtube_clean %>% 
    group_by(channel_title) %>% 
    count() %>% 
    arrange(desc(n)) %>% 
    head(9)



ui <- dashboardPage(
    dashboardHeader(title = "Youtube Trends"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Categorical Analytics", tabName = "category", icon = icon("chart-bar"))
        )
    ),
    
    dashboardBody(
        tabItems(
            
            tabItem(tabName = "home",
                    h2(strong("Youtube Trends: November 2017 - January 2018")),
                    h4("This dashboard provides a few analytical snapshots of trending videos on Youtube for the years 2017-2018"),
                    h1(""),
                    h4(strong("9 Most Viral Youtube Channels of the Period"), align = "center"),
                    fluidRow(
                      infoBoxOutput("rank1"),
                      infoBoxOutput("rank2"),
                      infoBoxOutput("rank3")
                    ),
                    fluidRow(
                      infoBoxOutput("rank4"),
                      infoBoxOutput("rank5"),
                      infoBoxOutput("rank6")
                    ),
                    fluidRow(
                      infoBoxOutput("rank7"),
                      infoBoxOutput("rank8"),
                      infoBoxOutput("rank9")
                    )
                    
            ),
            
            tabItem(tabName = "category",
                    sidebarLayout(
                      sidebarPanel(width = 2,
                        checkboxGroupInput(inputId = "cat_input",
                                           label = "Categories",
                                           choices = unique(youtube_clean$category_id),
                                           selected = unique(youtube_clean$category_id))
                      ),
                      mainPanel(width = 10,
                        verticalLayout(
                          plotlyOutput("cat_plot1"),
                          plotlyOutput("cat_plot2")
                        )
                      )
                    )
            )
        )
    )
)



server <- function(input, output, session) {
  
    output$rank1 <- renderInfoBox({
        infoBox("#1", top9[1,1], icon = icon("youtube"), fill = TRUE)
    })
    
    output$rank2 <- renderInfoBox({
      infoBox("#2", top9[2,1], icon = icon("youtube"), color = "purple", fill = TRUE)
    })
    
    output$rank3 <- renderInfoBox({
      infoBox("#3", top9[3,1], icon = icon("youtube"), fill = TRUE)
    })
    
    output$rank4 <- renderInfoBox({
      infoBox("#4", top9[4,1], icon = icon("youtube"), color = "purple", fill = TRUE)
    })
    
    output$rank5 <- renderInfoBox({
      infoBox("#5", top9[5,1], icon = icon("youtube"), fill = TRUE)
    })
    
    output$rank6 <- renderInfoBox({
      infoBox("#6", top9[6,1], icon = icon("youtube"), color = "purple", fill = TRUE)
    })
    
    output$rank7 <- renderInfoBox({
      infoBox("#7", top9[7,1], icon = icon("youtube"), fill = TRUE)
    })
    
    output$rank8 <- renderInfoBox({
      infoBox("#8", top9[8,1], icon = icon("youtube"), color = "purple", fill = TRUE)
    })
    
    output$rank9 <- renderInfoBox({
      infoBox("#9", top9[9,1], icon = icon("youtube"), fill = TRUE)
    })
    
    output$cat_plot1 <- renderPlotly({
      plot_freq <- youtube_clean %>% 
        group_by(category_id) %>% 
        count() %>%
        filter(category_id %in% input$cat_input) %>% 
        ggplot(aes(x = reorder(category_id, n), y = n, text = glue("{category_id}: {n}"))) +
        geom_col(fill = "blue") +
        labs(title = "Number of Trending Videos per Category",
             x = NULL,
             y = NULL) 
      ggplotly(plot_freq, tooltip = "text") %>% 
        config(displayModeBar = F) %>% 
        layout(xaxis = list(tickangle = 45))
    })
    
    output$cat_plot2 <- renderPlotly({
      plot_views <- youtube_clean %>% 
        filter(category_id %in% input$cat_input) %>% 
        ggplot(aes(x = category_id, y = log(views))) +
        geom_boxplot(fill = "purple") +
        labs(title = "Logged Distribution of Views per Video in each Category",
             x = NULL,
             y = NULL) 
      ggplotly(plot_views) %>% 
        config(displayModeBar = F) %>% 
        layout(xaxis = list(tickangle = 45))
    })
        
}



shinyApp(ui, server)