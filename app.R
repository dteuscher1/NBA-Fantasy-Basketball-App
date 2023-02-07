## Test Cases to make sure the app is correct:
# 1. Check the player table with Days/Season option
# 2. Check the player table with Days/ 1 game
# 3. Check the player table with Days/ 5 games
# 4. Check the player table with Game/Season option
# 5. Check the player table with Game/1 game
# 6. Check the player table with Game/ 5 game
# 7. Check the player information for player compare shows up
# 8. Check that the graph displays the two players

# Last updated: 20.01.23
######################################################################

library(shiny)
library(shinythemes)
library(shinydashboard)
library(tidyverse)
library(DT)
library(lubridate)
library(zoo)
library(gt)


#data <- read.csv("game_stats.csv") %>% 
#    mutate(date = str_extract(date, "[0-9\\-]+"), 
#           date = ymd(date) - 1)
data <- read.csv("fantasy_data.csv") %>% 
      mutate(date = ymd(game_date) - 1) %>%
  rename(fantasy_pts = fpts)

team_names <- data %>%
  group_by(athlete_display_name, team_name) %>%
  summarize(Games = n()) %>%
  arrange(athlete_display_name, Games) %>%
  top_n(1)

summary_col_names <- c("Name", "Team", "Games Played", "Average Fantasy Points", 
                       "Variance of Fantasy Points", "Standardized Score")
moving_average <- function(df, width){
  width <- as.numeric(width)
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
                            selectInput('time', "Choose a time period", c("Days", "Game"), "Days"),
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
                        width = 12,
                        column(
                            width = 4,
                            selectInput(
                                inputId = 'Player1', 
                                label = 'Player', 
                                choices = unique(sort(data$athlete_display_name))
                            )),
                        column(width = 4,
                            # # Add metrics here
                            # conditionalPanel(
                            #     condition = "input.Player1 != ''",
                            #     column( width = 6,
                            #       infoBoxOutput('Player1Box')
                            #     ),
                            #     column(width = 6,
                            #       infoBoxOutput('Player1Box10')
                            #     )
                            # ),
                            selectInput('Player2', 'Player', unique(sort(data$athlete_display_name)))),
                        column(width = 4,
                            selectInput('window', "Window for plot:", 2:20, selected = 5)),
                        column(width = 4,
                            actionButton('update2', 'Update')
                        ),
                        br(),
                        # ),
                        # column(
                        #     width = 6,
                        #     selectInput('Player2', 'Player', unique(sort(data$athlete_display_name))),
                        #     selectInput('window', "Window for plot:", 2:20, selected = 5)
                        #     # Add metrics here
                        # )
                    ),
                    column(
                        width = 12,
                        style='padding-left:10px; padding-right:10px; padding-top:10px; padding-bottom:10px',
                        plotOutput("moving_average")
                        
                    ),
                    column(
                      width = 12,
                      style='padding-left:0px; padding-right:1px; padding-top:5px; padding-bottom:5px',
                      gt::gt_output("compare")
                    )
                )
            )
        )
    )
)


server <- function(input, output, session) {
    
    rplot_selected <- eventReactive(input$update1, {
      if(input$time == "Days"){
        if(input$number == "Season"){
          output <- data %>%
            group_by(athlete_display_name) %>%
            summarize(games_played = n(),
                      avg = round(mean(fantasy_pts), digits = 1),
                      sd = round(sd(fantasy_pts), digits = 1),
                      single_measure = round(avg/sd, digits = 1)) %>% 
            arrange(desc(avg)) %>%
            inner_join(team_names, by = 'athlete_display_name') %>%
            select(athlete_display_name, team_name, games_played,
                   avg, sd, single_measure)
        } else{
          cut_date <- as.Date("2022-02-12") - as.numeric(input$number)
          output <- data %>% 
            arrange(desc(date)) %>%
            group_by(athlete_display_name) %>% 
            filter(date >= cut_date) %>%
            summarize(games_played = n(),
                      avg = round(mean(fantasy_pts), digits = 1),
                      sd = round(sd(fantasy_pts), digits = 1),
                      single_measure = round(avg/sd, digits = 1)) %>% 
            arrange(desc(avg)) %>%
            inner_join(team_names, by = 'athlete_display_name') %>%
            select(athlete_display_name, team_name, games_played,
                   avg, sd, single_measure)
        }
      } else{
          if(input$number == "Season"){
            output <- data %>%
              arrange(desc(date)) %>%
              group_by(athlete_display_name) %>%
              summarize(games_played = n(),
                        avg = round(mean(fantasy_pts), digits = 1),
                        sd = round(sd(fantasy_pts), digits = 1),
                        single_measure = round(avg/sd, digits = 1)) %>%
              arrange(desc(avg)) %>%
              inner_join(team_names, by = 'athlete_display_name') %>%
              select(athlete_display_name, team_name, games_played,
                     avg, sd, single_measure)
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
              arrange(desc(avg)) %>%
              inner_join(team_names, by = 'athlete_display_name') %>%
              select(athlete_display_name, team_name, games_played,
                     avg, sd, single_measure)
          }
      }
    })
    plot_ma <- eventReactive(input$update2, {
      player1 <- data %>%
        filter(athlete_display_name == input$Player1) %>%
        moving_average(width = input$window) %>%
        mutate(Player = input$Player1)
      
      player2 <- data %>%
        filter(athlete_display_name == input$Player2) %>%
        moving_average(width = input$window) %>%
        mutate(Player = input$Player2)
      
      player1 %>% bind_rows(player2) %>%
        ggplot(aes(Game, Fantasy_Pts, color = Player)) + geom_line() +
        labs(x = "Game Number", y = "Fantasy Points", caption = paste0(input$window, " game window")) +
        theme_minimal()
      
    })
    
    compare <- eventReactive(input$update2, {
      df <- data %>% filter(athlete_display_name %in% c(input$Player1, input$Player2)) %>%
        group_by(athlete_display_name) %>%
        summarize(fantasy_pts = mean(fantasy_pts),
                  points = mean(pts),
                  reb = mean(reb),
                  ast = mean(ast),
                  stl = mean(stl),
                  blk = mean(blk), 
                  to = mean(to)
        )
      
      col_name <- pull(df, athlete_display_name)
      df <- data.frame(t(df[,-1]))
      names(df) <- col_name
      row_names <- c("Fantasy Points", "Points", "Rebounds", "Assists", "Steals", "Blocks", "Turnovers")
      row.names(df) <- row_names
      df <- round(df, digits = 2)
      names(df) <- c("player1", "player2")
      out <- gt(df, rownames_to_stub = TRUE) %>%
        tab_style(
          style = list(cell_fill(color = "#228B22"),
                       cell_text(color = "white")),
          locations = cells_body(columns = player1,
                                 rows = player1 > player2)
        ) %>%
        tab_style(
          style = list(cell_fill(color = "#228B22"),
                       cell_text(color = "white")),
          locations = cells_body(columns = player2,
                                 rows = player2 > player1) 
        ) %>%
        cols_label(player1 = input$Player1,
                   player2 = input$Player2)
    })
    
    output$selected <- renderDataTable({
        datatable(rplot_selected(), rownames = FALSE, options = list(scrollX = '400px'),
                  colnames = summary_col_names, class = 'cell-border stripe')
    })
    
    output$compare <- render_gt({
      compare()
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

