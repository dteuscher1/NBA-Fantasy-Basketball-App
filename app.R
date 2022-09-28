library(shiny)
library(shinythemes)
library(shinydashboard)
library(tidyverse)
library(DT)
library(lubridate)
library(zoo)

data <- read.csv("game_stats.csv") %>% 
    mutate(date = str_extract(date, "[0-9\\-]+"), 
           date = ymd(date) - 1)
moving_average <- function(df, width){
  N <- nrow(df)
  df %>%
    transmute(Game = rollmean(1:N, k = width, fill = NA),
              Fantasy_Pts = rollmean(fantasy_pts, width,fill = NA))
}

ui <- dashboardPage(
    dashboardHeader(title = "Fantasy Basketball"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Intro", tabName = "intro", icon = icon("chart-line")),
            menuItem("Compare", tabName = "compare", icon = icon("calculator"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "intro",
                    fluidRow(
                        column(1),
                        box(
                            width = 10,
                            h1(strong("Fantasy Basketball App"))
                        ),
                        box(
                            width = 5,
                            selectInput('time', "Choose a time period", c("Days", "Games"), "Games"),
                            selectInput('number', "Select a number of days/games", c(1:82, "Season"), "Season"),
                            actionButton('update1', 'Update')
                        )),
                    fluidRow(
                        box(width = 12,
                            DT::dataTableOutput("selected")
                        )
                    )
            ),
            tabItem(
                tabName = "compare",
                br(),
                fluidRow(
                    column(
                        width = 6,
                        column(
                            width = 12,
                            selectInput(
                                inputId = 'Player1', 
                                label = 'Player', 
                                choices = unique(sort(data$athlete_display_name))
                            ),
                            # Add metrics here
                            conditionalPanel(
                                condition = "input.Player1 != ''",
                                column( width = 6,
                                  infoBoxOutput('Player1Box')
                                ),
                                column(width = 6,
                                  infoBoxOutput('Player1Box10')
                                )
                            ),
                            actionButton('update2', 'Update')
                        ),
                        column(
                            width = 6,
                            selectInput('Player2', 'Player', unique(sort(data$athlete_display_name)))
                            # Add metrics here
                        )
                    ),
                    column(
                        width = 6,
                        plotOutput("moving_average")
                        
                    )
                )
            )
        )
    )
)


server <- function(input, output, session) {
    
    rplot_selected <- eventReactive(input$update1, {
        if (input$time == "Games") {
          if(input$number == "Season"){
            output <- data %>% 
              arrange(desc(date)) %>%
              group_by(athlete_display_name) %>% 
              top_n(input$number, date) %>%
              summarize(games_played = n(),
                        avg = round(mean(fantasy_pts), digits = 1),
                        sd = round(sd(fantasy_pts), digits = 1),
                        single_measure = round(avg/sd, digits = 1)) %>% 
              arrange(desc(avg)) 
          } else {
            num_games <- as.numeric(input$number)
            output <- data %>% 
              arrange(desc(date)) %>%
              group_by(athlete_display_name) %>% 
              top_n(num_games, date) %>%
              summarize(games_played = n(),
                        avg = round(mean(fantasy_pts), digits = 1),
                        sd = round(sd(fantasy_pts), digits = 1),
                        single_measure = round(avg/sd, digits = 1)) %>% 
              arrange(desc(avg))
          }
        }
      if(input$time == "Days"){
        cut_date <- as.Date("2022-02-12") - as.numeric(input$number)
        output <- data %>% 
          arrange(desc(date)) %>%
          group_by(athlete_display_name) %>% 
          filter(date >= cut_date) %>%
          summarize(games_played = n(),
                    avg = round(mean(fantasy_pts), digits = 1),
                    sd = round(sd(fantasy_pts), digits = 1),
                    single_measure = round(avg/sd, digits = 1)) %>% 
          arrange(desc(avg))
      }
    })
    plot_ma <- eventReactive(input$update2, {
      data %>%
        filter(athlete_display_name == input$Player1) %>%
        moving_average(5) %>%
        ggplot(aes(Game, Fantasy_Pts)) + geom_line()
      
    })
    
    output$selected <- renderDataTable({
        datatable(rplot_selected(), rownames = FALSE, options = list(scrollX = '400px'))
    })
    
    output$Player1Box <- renderInfoBox({
        infoBox(
            title = 'Average FPTS', 
            value = round(unlist(data %>% filter(athlete_display_name == input$Player1) %>% select(fantasy_pts)) %>% mean(), 1),
            # subtitle = input$Player1,
            #icon = icon('chart-column'),
            #color = 'light-blue',
            width = 6,
            fill = TRUE
        )
    })
    
    output$Player1Box10 <- renderInfoBox({
        infoBox(
            title = 'Previous 10 Game FPTS Average', 
            value = round(
                unlist(
                    data %>% 
                        filter(athlete_display_name == input$Player2) %>% 
                        select(fantasy_pts) %>% 
                        tail(10)
                ) %>% 
                    mean(),
                1
            ),
            # subtitle = input$Player1,
            #icon = icon('chart-column'),
            #color = 'light-blue',
            width = 6,
            fill = TRUE
        )
    })
    
    output$moving_average <- renderPlot(plot_ma())
    
}


shinyApp(ui = ui, server = server)
