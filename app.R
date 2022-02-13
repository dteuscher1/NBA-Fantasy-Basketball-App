library(shiny)
library(shinythemes)
library(shinydashboard)
library(tidyverse)
library(DT)
library(lubridate)

data <- read.csv("game_stats.csv") %>% 
    mutate(date = str_extract(date, "[0-9\\-]+"), 
           date = ymd(date) - 1)
ui <- dashboardPage(
    dashboardHeader(title = "Fantasy Basketball"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Intro", tabName = "intro", icon = icon("chart-line"))
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
            )
        )
    )
)


server <- function(input, output, session){
    rplot_selected <- eventReactive(input$update1, {
        if(input$time == "Games"){
            output <- data %>% 
                arrange(desc(date)) %>%
                group_by(athlete_display_name) %>% 
                top_n(input$number, date) %>%
                summarize(games_played = n(),
                          avg = round(mean(fantasy_pts), digits = 1),
                          sd = round(sd(fantasy_pts), digits = 1),
                          single_measure = round(avg/sd, digits = 1)) %>% 
                arrange(desc(avg))
        }
    })
    output$selected <- renderDataTable({
        datatable(rplot_selected(), rownames = FALSE, options = list(scrollX='400px'))
    })
}

shinyApp(ui = ui, server = server)
