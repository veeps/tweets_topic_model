library(shiny)
library(tidyverse)
library(janitor)
library(plotly)
library(DT)
library(tidytext)
library(textdata)
library(lubridate)

# Read in data ------------------------------------------------

landmine <- read_csv("landmine_tweets.csv") |>
  select(date, handle, tweet, query_term) |>
  rename(topic = query_term)

# read afinn words
afinn <- readr::read_csv("./data/afinn.csv")

# UI ----------------------------------------------------------

ui <- fluidPage(
  fluidRow(style="padding:40px; background: #0084b4; text-align: center; color: #ffffff",
           h1("Exploring Tweets on Landmines")),
  fluidRow(style="padding-left:40px; padding-right:40px; padding-bottom:60px;",
           div(
             h3(style = "text-align: center;", "Click on a point in the chart to explore tweets from that date"),
             plotlyOutput("landmine_timeline"))
           ), 
  fluidRow(style="padding:40px; background:	#c0deed; color: #000000",
           column(6,div(
                  h4("Here's a common theme on what happened on that day (click on point in chart above)"),
                  h4(style="padding:20px; background: #ffffff; color:#000000;",textOutput("common_tweet1"))
           )),
           column(6,div(
                  h4("Here are the most common words that appeared in the tweets (click on row to google the term)"),
                  DTOutput("top_words")
                  ))
          ), #end fluidRow
  fluidRow(style="padding:40px;background: #f2f4f4; ",
           div(
             h3(style = "text-align: center; padding:20px;",  "Here are all the tweets from ", textOutput("date")),
             DTOutput("landmine_tweets"))
        ) # end fluidRow
)

# Server -----------------------------------------------------


server <- function(input, output, session) {
  
  # create summary table
  summary <- landmine |>
    group_by(date, topic) |>
    summarise(total = as.numeric(n()))
  
  # render time series of tweets
  output$landmine_timeline <- renderPlotly(
  plot_ly( 
    data = summary |> filter(topic != "misc"), 
    x = ~date,
    y = ~total,
    height = 450,
    legendgroup = ~as.factor(topic),
    color = ~topic, 
    type = 'scatter',
    mode = 'line', 
    showlegend =  T,
    opacity = 1  
  ) |>
    layout(
      xaxis=list(title = "Date"),
      yaxis=list(title = "Number of Tweets")
    ) # end layout
  )
  
  # record click event to select topic
  select_topic <- reactive({
    summary |> filter (date == event_data("plotly_click")$x  & total ==event_data("plotly_click")$y) |> pull(topic)
  })
  
  # connect click event to original landmine tweets df
  landmine_r <- reactive({
    landmine |> filter (topic == select_topic() & date == event_data("plotly_click")$x) |> select(-topic)
    
  })
  
  # render datatable for all tweets
  output$landmine_tweets <- DT::renderDataTable({
    DT::datatable(
      if(!is.null(event_data("plotly_click"))) {
        landmine_r() |> select(-date)
      } else {
        landmine |> select(-topic) |> select(-date)
      },
      rownames=FALSE,
      options = list(info = FALSE)

    ) # end datatable
    })
  
  # date selected
  output$date <- renderText({
    req(event_data("plotly_click"))
    ymd(event_data("plotly_click")$x) |> format('%b %d, %Y')
  })

  
  # common word analysis -------------------
  
  custom_stopwords <- c("http", "https", "rt", "t.co", "ied", "bomb", "explode", "injured", "blast", "blasts")
  
  
  #analyze most common words
  words <- reactive({
    req(landmine_r())
    landmine_r() %>%
    unnest_tokens(word, tweet) %>%
    filter(!word %in% custom_stopwords) %>%
    anti_join(stop_words) %>%
    count(word, sort = TRUE) %>%
    head(5)
  })
  
  output$top_words <- DT::renderDataTable({
    req(event_data("plotly_click"))
    datatable(
      words() |> select (word) |> mutate(link = google_this(words()$word)),
      rownames = FALSE,
      escape = FALSE,
      options = list(searching = FALSE, paging = FALSE, sort = FALSE, info = FALSE,
                     columnDefs = list(list(className = 'dt-center', targets = '_all')),
                     headerCallback = JS(
                       "function(thead, data, start, end, display){",
                       "  $(thead).remove();",
                       "}")))
    })
  
  # create custom gradient palette 
  colors <-colorRampPalette(c("#30cbcf", "#7cd8c9", "#b4e3c5"))
  
  # plot for most common words
  # output$plot_words <- renderPlot({
  #   req(event_data("plotly_click"))
  #   ggplot(words(), aes(x=word, y = n, fill = word)) + geom_bar(stat="identity", show.legend = F) + 
  #     scale_fill_manual(values = colors(10)) + 
  #     theme(plot.title = element_text(hjust = 0.5, size=18)) +
  #     ylab("Number of Times Words Appeared") +
  #     ggtitle("Most Common Words") + 
  #     coord_flip()
  # })
  
  # find the most common tweets to see what's happening that day
  common <-  reactive({
    landmine_r() |>
    unnest_tokens(ngram, tweet, token = "ngrams", n = 15) |>
    count(ngram, sort = T) |>
    head(3)
  })
  
  #get text of common tweet
  output$common_tweet1 <- renderText({req(event_data("plotly_click"))
    common()[1,1] |> pull(ngram)})
  
  # Google search -----------------------------------------------------
  
  google_this <- function(search_term) {
    sprintf('<a href="https://www.google.com/?q=%s" target="_blank" class="btn btn-xs">google this</a>',search_term)
  }
  
  # # add observe event for row click
  # observeEvent(input$top_words_rows_selected, {
  #   showModal(modalDialog(
  #     h3("Here's what this term means"),
  #     tags$iframe(src = "https://google.com"),
  #           easyClose = TRUE))
  # })
  

}

shinyApp(ui, server)