library(shiny)
library(shinythemes)
library(shinydashboard)
library(tidyverse)
library(DT)

data <- read.csv("game_stats.csv")
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
                            width = 6,
                            selectInput(
                                inputId = 'Player1', 
                                label = 'Player', 
                                choices = unique(sort(data$athlete_display_name))
                            ),
                            # Add metrics here
                            conditionalPanel(
                                condition = "input.Player1 != ''",
                                infoBoxOutput('Player1Box'),
                                infoBoxOutput('Player1Box10')
                            )
                        ),
                        column(
                            width = 6,
                            selectInput('x', 'Player', unique(sort(data$athlete_display_name)))
                            # Add metrics here
                        )
                    ),
                    column(
                        width = 6
                        # Add graph here
                    )
                )
            )
        )
    )
)


server <- function(input, output, session) {
    
    rplot_selected <- eventReactive(input$update1, {
        if (input$time == "Games") {
            output <- data %>% 
                arrange(desc(game_id)) %>%
                group_by(athlete_display_name) %>% 
                top_n(input$number, game_id) %>%
                summarize(avg = round(mean(fantasy_pts), digits = 1),
                          variance = round(var(fantasy_pts), digits = 1)) %>% 
                arrange(desc(avg))
        }
    })
    
    output$selected <- renderDataTable({
        datatable(rplot_selected(), rownames = FALSE, options = list(scrollX = '400px'))
    })
    
    output$Player1Box <- renderInfoBox({
        infoBox(
            title = 'Average FPTS', 
            value = round(unlist(game_info %>% filter(athlete_display_name == input$Player1) %>% select(fantasy_pts)) %>% mean(), 1),
            # subtitle = input$Player1,
            # icon = icon('fa-basketball'),
            color = 'light-blue',
            width = 6,
            fill = TRUE
        )
    })
    
    output$Player1Box10 <- renderInfoBox({
        infoBox(
            title = 'Previous 10 Game FPTS Average', 
            value = round(
                unlist(
                    game_info %>% 
                        filter(athlete_display_name == input$Player1) %>% 
                        select(fantasy_pts) %>% 
                        tail(10)
                ) %>% 
                    mean(),
                1
            ),
            # subtitle = input$Player1,
            # icon = icon('fa-basketball'),
            color = 'light-blue',
            width = 6,
            fill = TRUE
        )
    })
    
}

shinyApp(ui = ui, server = server)
