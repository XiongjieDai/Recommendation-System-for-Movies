## server.R

library(dplyr)
library(recommenderlab)

# read in movies data
myurl = "data/"
movies = readLines(paste0(myurl, 'movies.dat'))
movies = strsplit(movies,
                  split = "::",
                  fixed = TRUE,
                  useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

# read in movie image data
small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID,
                          function(x)
                            paste0(small_image_url, x, '.jpg?raw=true'))

# read in movie ratings
ratings = read.csv(
  paste0(myurl, 'ratings.dat'),
  sep = ':',
  colClasses = c('integer', 'NULL'),
  header = FALSE
)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'DateTime')

# join ratings and movies data
ratings_movies = ratings %>%
  group_by(MovieID) %>%
  summarize(ratings_per_movie = n(),
            avg_rating = round(mean(Rating), dig = 3)) %>%
  inner_join(movies, by = "MovieID") %>%
  arrange(desc(ratings_per_movie))

# Load data and pre-trained model for doing IBCF predictions
load(paste0(myurl, 'movie_cols.RData'))
load(paste0(myurl, 'IBCF.RData'))

# Fetch user ratings from the UI
get_user_ratings = function(value_list) {
  dat = data.table(
    MovieID = sapply(strsplit(names(value_list), "_"),
                     function(x)
                       ifelse(length(x) > 1, x[[2]], NA)),
    Rating = unlist(as.character(value_list))
  )
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
  new.ratings = rep(NA, length(movie_cols))
  for (i in 1:nrow(dat)) {
    rating_i = which(movie_cols == paste0('m', dat[i, 1]))
    new.ratings[rating_i] = dat$Rating[i]
  }
  
  new.user = matrix(
    new.ratings,
    nrow = 1,
    ncol = length(movie_cols),
    dimnames = list(user = paste('new'),
                    item = movie_cols)
  )
  new.Rmat = as(new.user, 'realRatingMatrix')
  list(Rmat = new.Rmat, ratings = dat)
}

# Fetch movies by genre
get_popular_movies = function(genre = NULL,
                                       n_recommendations = 10,
                                       min_rating = 3) {
  
  selected_movies = ratings_movies
  if (!is.null(genre)) {
    selected_movies = selected_movies %>%
      filter(grepl(genre, Genres))
  }
  
  selected_movies = selected_movies %>%
    filter(avg_rating >= min_rating) %>%
    head(n_recommendations)
  
  data.table(
    Rank = 1:n_recommendations,
    MovieID = selected_movies$MovieID,
    Title = selected_movies$Title,
    image_url = selected_movies$image_url
  )
}

# Ensure recommended results do not contain user ratings
# Fill NA predictions with popular movies if recommended ratings are all NA
# Or supplement with popular movies from the same genre of movies that the user
# rated.
ensure_non_na_results = function(rated_movies, recom_results, n_recommendations = 10) {
  if (sum(is.na(recom_results$Predicted_rating)) <= 0) {
    new_results = recom_results
  } else {
    user_genres = unique(unlist(strsplit(rated_movies$Genres, "[|]")))
  
    if (length(user_genres) > 0) {
        popular_movies = data.frame()
        for (i in 1:length(user_genres)) {
          popular_movies = get_popular_movies(genre = user_genres[i], n_recommendations = 100) %>%
            bind_rows(popular_movies)
        }
        popular_movies = popular_movies %>%
          sample_n(100)  
    } else {
      popular_movies = get_popular_movies(n_recommendations = 100)
    }
    
    popular_movies = popular_movies %>%
      filter(!MovieID %in% recom_results$MovieID)
    
    new_results = recom_results %>%
      filter(!is.na(Predicted_rating)) %>%
      bind_rows(popular_movies) %>%
      filter(!MovieID %in% rated_movies$MovieID) %>%
      head(n_recommendations) 
  }
  
  new_results$Rank = 1:n_recommendations
  new_results
}

# Start shiny server
shinyServer(function(input, output, session) {
  # display the genre recommendations
  output$genre_results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    genre_recom_result <- genre_df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(
          width = 2,
          status = "success",
          solidHeader = TRUE,
          title = paste0("Rank ", (i - 1) * num_movies + j),
          
          div(style = "text-align:center",
              a(
                img(src = genre_recom_result$image_url[(i - 1) * num_movies + j], height = 150)
              )),
          div(style = "text-align:center; font-size: 100%",
              strong(genre_recom_result$Title[(i - 1) * num_movies + j]))
          
        )
      }))) # columns
    }) # rows
    
  })
  
  # show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(
          width = 2,
          div(style = "text-align:center", img(
            src = movies$image_url[(i - 1) * num_movies + j], height = 150
          )),

          div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
          div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(
            paste0("select_", movies$MovieID[(i - 1) * num_movies + j]),
            label = "",
            dataStop = 5
          ))
        )) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when the genre submit button is clicked
  genre_df <- eventReactive(input$genre_btn, {
    withBusyIndicatorServer("genre_btn", {
      # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <-
        "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the selected genre
      get_popular_movies(genre = input$genre)
    }) # still busy
    
  }) # clicked on button
  
  # Calculate recommendations using IBCF when submit button is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", {
      # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <-
        "document.querySelector('#shiny-tab-system_ii [data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      p.IBCF <- predict(full_IBCF, user_ratings$Rmat, type = "ratings")
      p.IBCF <- as.numeric(as(p.IBCF, "matrix"))
      predicted_movies = data.frame(
        MovieID = as.integer(substring(movie_cols, 2)),
        Predicted_rating = p.IBCF
      ) %>%
        inner_join(ratings_movies, by = "MovieID") %>%
        arrange(desc(Predicted_rating)) %>%
        head(10)
      
      rated_movies = user_ratings$ratings %>%
        inner_join(movies, by = "MovieID")
  
      recom_results <- data.table(
        Rank = 1:10,
        MovieID = predicted_movies$MovieID,
        Title = predicted_movies$Title,
        Predicted_rating =  predicted_movies$Predicted_rating,
        image_url = predicted_movies$image_url
      )
      
      ensure_non_na_results(rated_movies, recom_results)
    }) # still busy
    
  }) # clicked on button
  
  
  # display the IBCF recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
  
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(
          width = 2,
          status = "success",
          solidHeader = TRUE,
          title = paste0("Rank ", (i - 1) * num_movies + j),
          
          div(style = "text-align:center",
              a(
                img(src = recom_result$image_url[(i - 1) * num_movies + j], height = 150)
              )),
          div(style = "text-align:center; font-size: 100%",
              strong(recom_result$Title[(i - 1) * num_movies + j]))
          
        )
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function
