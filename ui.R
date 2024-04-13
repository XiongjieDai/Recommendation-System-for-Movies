## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')


genres = c(
  "Action",
  "Adventure",
  "Animation",
  "Children's",
  "Comedy",
  "Crime",
  "Documentary",
  "Drama",
  "Fantasy",
  "Film-Noir",
  "Horror",
  "Musical",
  "Mystery",
  "Romance",
  "Sci-Fi",
  "Thriller",
  "War",
  "Western"
)

# Display UI
shinyUI(dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Movie Recommender"),
  dashboardSidebar(sidebarMenu(
    menuItem("Recommender by Genre", tabName = "system_i"),
    menuItem("Recommender by Rating", tabName = "system_ii")
  )),
  dashboardBody(includeCSS("css/movies.css"),
                tabItems(
                  tabItem(tabName = "system_i",
                          fluidRow(
                            box(
                              width = 12,
                              title = "Step 1: Select a genre to get recommendations",
                              status = "info",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              selectInput("genre", NULL,
                                          genres)
                            )
                          ),
                          fluidRow(
                            useShinyjs(),
                            box(
                              width = 12,
                              status = "info",
                              solidHeader = TRUE,
                              title = "Step 2: Discover movies you might like",
                              br(),
                              withBusyIndicatorUI(
                                actionButton("genre_btn", "Click here to get your recommendations", class = "btn-warning")
                              ),
                              br(),
                              tableOutput("genre_results")
                            )
                          )),
                  tabItem(tabName = "system_ii",
                          fluidRow(
                            box(
                              width = 12,
                              title = "Step 1: Rate as many movies as possible (at least three)",
                              status = "info",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              div(class = "rateitems",
                                  uiOutput('ratings'))
                            )
                          ),
                          fluidRow(
                            useShinyjs(),
                            box(
                              width = 12,
                              status = "info",
                              solidHeader = TRUE,
                              title = "Step 2: Discover movies you might like",
                              br(),
                              withBusyIndicatorUI(
                                actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                              ),
                              br(),
                              tableOutput("results")
                            )
                          ))
                ))
)) 