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
data <- read.csv("https://raw.githubusercontent.com/dteuscher1/NBA-Fantasy-Basketball-App/main/fantasy_data.csv") %>% 
  #read.csv("fantasy_data.csv") %>%    
  mutate(date = ymd(game_date) - 1) %>%
  filter(!(team_abbreviation  %in% c("GIA", "LEB"))) %>%
  mutate(team_abbreviation = ifelse(team_abbreviation == "UTAH", "UTA", 
                                    ifelse(team_abbreviation == "GS", "GSW", 
                                           ifelse(team_abbreviation == "NY", "NYK",
                                                  ifelse(team_abbreviation == "PHI", "PHL",
                                                         ifelse(team_abbreviation == "WSH", "WAS",
                                                                ifelse(team_abbreviation == "NO", "NOP",
                                                                       ifelse(team_abbreviation == "SA", "SAS",
                                                                              ifelse(team_abbreviation == "PHX", "PHO", team_abbreviation))))))))) %>%
  rename(fantasy_pts = fpts)

rosters <- read.csv("team_rosters.csv") 
team_names <- data %>%
  group_by(athlete_display_name, team_name) %>%
  summarize(Games = n()) %>%
  arrange(athlete_display_name, Games) %>%
  top_n(1)

team_abbreviations <-  data %>%
  group_by(athlete_display_name, team_abbreviation) %>%
  summarize(Games = n()) %>%
  arrange(athlete_display_name, Games) %>%
  top_n(1)

usage_rate <- read.csv("https://raw.githubusercontent.com/dteuscher1/NBA-Fantasy-Basketball-App/main/usage_rates.csv") %>%
  dplyr::select(GAME_ID, GAME_DATE, PLAYER_ID, PLAYER_NAME, USG_PCT)
summary_col_names <- c("Name", "Position", "Team", "Games Played", "Average Fantasy Points", 
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
            menuItem("Compare", tabName = "compare", icon = icon("calculator")),
            menuItem("Players", tabName = "players", icon = icon("person")),
            menuItem("Usage Rate", tabName = "usage")
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
                            selectInput('time', "Choose a time period", c("Days", "Game")),
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
                        width = 8,
                        align = 'center',
                        offset = 2,
                        style='padding-left:10px; padding-right:10px; padding-top:10px; padding-bottom:10px',
                        plotOutput("moving_average")
                        
                    ),
                    column(
                      width = 12,
                      style='padding-left:0px; padding-right:1px; padding-top:5px; padding-bottom:5px',
                      gt::gt_output("compare")
                    ),
                    column(
                      width = 8,
                      align = 'center',
                      offset = 2,
                      style='padding-left:10px; padding-right:10px; padding-top:10px; padding-bottom:10px',
                      plotOutput("plot_density")
                    )  
                )
            ),
            tabItem(tabName = "players",
                    box(
                      width = 5,
                      selectInput('available', "Select all available players, all players from a team, or all players", c("All Players", unique(rosters$Team)), "Available"),
                      selectInput('number3', "Select a number of games", c(1:82, "Season"), "Season"),
                      actionButton('update3', 'Update')
                    ),
                    fluidRow(
                      box(width = 12,
                          DT::dataTableOutput("box")
                      )
                    )  
            ),
            tabItem(tabName = "usage",
              fluidRow(
                    column(
                      width = 12,
                      column(
                        width = 6,
                        selectInput(
                          inputId = 'Player3', 
                          label = 'Player', 
                          choices = unique(sort(data$athlete_display_name))
                        )),
                      column(width = 6,
                             selectInput('Player4', 'Player', unique(sort(data$athlete_display_name)))),
                      column(width = 6,
                             dateRangeInput('usagedate', "Choose a range of dates", start = '2023-10-24', end = Sys.Date())),
                      column(width = 3,
                             actionButton('update4', 'Update')),
                      br(),
                    ),
                    column(
                      width = 8,
                      align = 'center',
                      offset = 2,
                      style='padding-left:10px; padding-right:10px; padding-top:10px; padding-bottom:10px',
                      plotOutput("usage_density")
                    ),
                    column(
                      width = 8,
                      align = 'center',
                      offset = 2,
                      plotOutput("usage_over_time")
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
            inner_join(rosters %>% dplyr::select(Player_Name, Position), by = c('athlete_display_name' = 'Player_Name')) %>%
            select(athlete_display_name, Position, team_name, games_played,
                   avg, sd, single_measure)
        } else{
          cut_date <- Sys.Date() - as.numeric(input$number)
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
            inner_join(rosters %>% dplyr::select(Player_Name, Position), by = c('athlete_display_name' = 'Player_Name')) %>%
            select(athlete_display_name, Position, team_name, games_played,
                   avg, sd, single_measure)
        }
      } else if(input$time == 'Game'){
          if(input$number == "Season"){
            output <- data %>%
              arrange(desc(date)) %>%
              group_by(athlete_display_name) %>%
              summarize(games_played = n(),
                        avg = round(mean(fantasy_pts), digits = 1),
                        sd = round(sd(fantasy_pts), digits = 1),
                        single_measure = round(avg/sd, digits = 1)) %>%
              arrange(desc(avg)) %>%
              inner_join(team_names, by = 'athlete_display_name')  %>%
              inner_join(rosters %>% dplyr::select(Player_Name, Position), by = c('athlete_display_name' = 'Player_Name')) %>%
              select(athlete_display_name, Position, team_name, games_played,
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
              inner_join(team_names, by = 'athlete_display_name')  %>%
              inner_join(rosters %>% dplyr::select(Player_Name, Position), by = c('athlete_display_name' = 'Player_Name')) %>%
              select(athlete_display_name, Position, team_name, games_played,
                     avg, sd, single_measure)
          }
      }
    })
    
    roster_table <- eventReactive(input$update3, {
      if(input$number3 == "Season"){
        output <- data %>%
          arrange(desc(date)) %>%
          group_by(athlete_display_name) %>%
          summarize(games_played = n(),
                    avg = round(mean(fantasy_pts), digits = 1),
                    sd = round(sd(fantasy_pts), digits = 1),
                    single_measure = round(avg/sd, digits = 1)) %>%
          arrange(desc(avg)) %>%
          inner_join(team_names, by = 'athlete_display_name') %>%
          inner_join(team_abbreviations, by = 'athlete_display_name') %>%
          select(athlete_display_name, team_name, team_abbreviation, games_played,
                 avg, sd, single_measure) %>%
          left_join(rosters, by = c('athlete_display_name' = 'Player_Name', 'team_abbreviation' = 'NBA.Team'), multiple = "all") %>%
          select(-team_abbreviation, -X) %>%
          relocate(Position, .after = athlete_display_name)
      } else {
        num_games <- as.numeric(input$number3)
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
          inner_join(team_abbreviations, by = 'athlete_display_name') %>%
          select(athlete_display_name, team_name, team_abbreviation, games_played,
                 avg, sd, single_measure) %>%
          left_join(rosters, by = c('athlete_display_name' = 'Player_Name', 'team_abbreviation' = 'NBA.Team'), multiple = "all") %>%
          select(-team_abbreviation, -X) %>%
          relocate(Position, .after = athlete_display_name)
      }
      if(input$available != "All Players"){
        output1 <- output %>%
          filter(Team == input$available) %>%
          relocate(Position, .after = athlete_display_name)
      } else{
        output %>%
          relocate(Position, .after = athlete_display_name)
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
    
    plot_density <- eventReactive(input$update2, {
      df <- data %>% filter(athlete_display_name %in% c(input$Player1, input$Player2))
      ggplot(df, aes(x = fantasy_pts, color = athlete_display_name)) + 
        stat_density(geom = "line", position = 'identity', adjust = .5) +
        scale_color_discrete(name = "Player") +
        theme_minimal() + 
        labs(x = "Fantasy Points", y = "Density")
    })
    
    usage_density <- eventReactive(input$update4, {
      df <- usage_rate %>% filter(PLAYER_NAME %in% c(input$Player3, input$Player4))
      ggplot(df, aes(x = USG_PCT, color = PLAYER_NAME)) + 
        stat_density(geom = "line", position = 'identity', adjust = .5) +
        scale_color_discrete(name = "Player") +
        theme_minimal() + 
        labs(x = "Usage Rate", y = "Density")
    })
    
    usage_over_time <- eventReactive(input$update4, {
      df <- usage_rate %>% filter(PLAYER_NAME %in% c(input$Player3, input$Player4)) %>%
        filter(GAME_DATE >= input$usagedate[1], GAME_DATE <= input$usagedate[2])
      ggplot(df, aes(as.Date(GAME_DATE), USG_PCT, color = PLAYER_NAME)) + geom_line() +
        labs(x = "Date", y = "Usage Percentage", color = "Player") +
        theme_minimal()
    })
    
    compare <- eventReactive(input$update2, {
      df <- data %>% filter(athlete_display_name %in% c(input$Player1, input$Player2)) %>%
        #data %>% filter(athlete_display_name %in% c(player1, player2)) %>%
        group_by(athlete_display_name) %>%
        summarize(fantasy_pts = mean(fantasy_pts),
                  points = mean(pts),
                  reb = mean(reb),
                  ast = mean(ast),
                  stl = mean(stl),
                  blk = mean(blk), 
                  to = mean(to)
        )
      
      row_name <- dplyr::pull(df, athlete_display_name)
      df <- data.frame(df[,-1])
      row.names(df) <- row_name
      col_names <- c("Fantasy_Points", "Points", "Rebounds", "Assists", "Steals", "Blocks", "Turnovers")
      names(df) <- col_names
      df <- round(df, digits = 2)
      out <- gt(df, rownames_to_stub = TRUE) %>%
        tab_style(
          style = list(cell_fill(color = "#228B22"),
                       cell_text(color = "white")),
          locations = cells_body(columns = Fantasy_Points,
                                 rows = Fantasy_Points == max(Fantasy_Points))
        ) %>%
        tab_style(
          style = list(cell_fill(color = "#228B22"),
                       cell_text(color = "white")),
          locations = cells_body(columns = Points,
                                 rows = Points == max(Points)) 
        ) %>%
        tab_style(
          style = list(cell_fill(color = "#228B22"),
                       cell_text(color = "white")),
          locations = cells_body(columns = Rebounds,
                                 rows = Rebounds == max(Rebounds)) 
        ) %>%
        tab_style(
          style = list(cell_fill(color = "#228B22"),
                       cell_text(color = "white")),
          locations = cells_body(columns = Assists,
                                 rows = Assists == max(Assists)) 
        ) %>%
        tab_style(
          style = list(cell_fill(color = "#228B22"),
                       cell_text(color = "white")),
          locations = cells_body(columns = Steals,
                                 rows = Steals == max(Steals)) 
        ) %>%
        tab_style(
          style = list(cell_fill(color = "#228B22"),
                       cell_text(color = "white")),
          locations = cells_body(columns = Blocks,
                                 rows = Blocks == max(Blocks)) 
        ) %>%
        tab_style(
          style = list(cell_fill(color = "#D22B2B"),
                       cell_text(color = "white")),
          locations = cells_body(columns = Turnovers,
                                 rows = Turnovers == max(Turnovers)) 
        ) %>%
        cols_label(Fantasy_Points = "Fantasy Points")
    })
    
    output$selected <- renderDataTable({
        datatable(rplot_selected(), rownames = FALSE, options = list(scrollX = '400px'),
                  colnames = summary_col_names, class = 'cell-border stripe', filter = 'top')
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
    
    output$plot_density <- renderPlot(plot_density())
    
    output$usage_density <- renderPlot(usage_density())
    
    output$usage_over_time <- renderPlot(usage_over_time())
    
    output$box <- renderDataTable(datatable(roster_table(), rownames = FALSE, options = list(scrollX = '400px'),
                                            colnames = c(summary_col_names, "Team"), class = 'cell-border stripe', filter = 'top'))
    
}


shinyApp(ui = ui, server = server)


