library(shiny)
library(dplyr)
library(readr)
library(magrittr)
library(janitor)
library(plotly)
library(DT)
library(tidytext)
library(lubridate)
library(WikipediR)
library(purrr)
library(stringr)

# Automatically defined env docker build
if (Sys.getenv("DOCKER_RUNNING") == "true"){
  
  system("/bin/get-s3-dynamics.sh") 
} else{
  
  # Assumes working dir is dashboard/shiny-app
  #here::i_am("dashboard/shiny-app/global.R")
  system("bash ../get-s3-dynamics.sh")
}


# Word formatting ----------------------------------------------------
wrap_in <- function(.df, column, word, tag){
  class<-""
  if(grepl("\\.", tag)) {
    class <- sub(".+?\\.(.+)", " class='\\1'", tag)
    tag <- sub("\\..+", "", tag)
  }
  .df[[column]] <-  gsub(sprintf("\\b(%s)\\b", paste0(word,collapse="|")), sprintf("<%1$s%2$s>\\1</%1$s>", tag, class), .df[[column]])
  .df
}

# Google search -----------------------------------------------------

google_this <- function(search_term) {
  sprintf('<a href="https://www.google.com/search?q=%s" target="_blank" class="btn btn-xs">google this</a>',search_term)
}

translate_this <- function(search_term) {
  sprintf('<a href="https://translate.google.com/?sl=auto&tl=en&text=%s" target="_blank" class="btn btn-xs">translate</a>',search_term)
}

# Twitter URL  -----------------------------------------------------

get_tweet <- function(id) {
  sprintf('<a href="https://twitter.com/anon/status/%s" target="_blank" class="btn btn-xs">see tweet</a>',id)
}

# Read in data ------------------------------------------------

df <- read_csv("main.csv") |>
  select(date, handle, tweet, query_term, id) |>
  rename(topic = query_term, document_id = id) |>
  htmlwidgets::prependContent(htmltools::tags$style("mark {font-weight: 700; background-color: yellow}"))  #|> wrap_in("tweet", "IED", "mark")


ner <- read_csv("ner_tagged_landmine_tweets.csv") |>
  rename( tweet = sentence_text)

landmine <- left_join(ner, df, by = c("document_id", "tweet")) |>
  filter(!grepl('scooby doo', tagged_text)) 


# read afinn words
#afinn <- readr::read_csv("./data/afinn.csv")

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
                  h4(style="padding:20px; background: #ffffff; color:#000000;",textOutput("common_tweet1")),
                  uiOutput("translate_button")
           )),
           column(6,div(
                  h4("Common names, places, and orgs that appeared in these tweets (click on row to google the term)"),
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
    filter(topic != "misc") |> 
    group_by(date) |>
    summarise(total = as.numeric(n()))
  
  # render time series of tweets
  output$landmine_timeline <- renderPlotly(
  plot_ly( 
    data = summary, 
    x = ~date,
    y = ~total,
    height = 450,
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
  # select_topic <- reactive({
  #   summary |> filter (date == event_data("plotly_click")$x  & total ==event_data("plotly_click")$y) |> pull(topic)
  # })
  
  # connect click event to original landmine tweets df
  landmine_r <- reactive({
    landmine |> filter (date == event_data("plotly_click")$x) 
    
  })
  
  # render datatable for all tweets
  output$landmine_tweets <- DT::renderDataTable({
    DT::datatable(
      if(!is.null(event_data("plotly_click"))) {
        landmine_r()  |> distinct(handle, tweet, .keep_all = T) |>
          mutate(link = get_tweet(document_id)) |> select(handle, tweet, link)
      } else {
        landmine  |> distinct(handle, tweet, .keep_all = T) |>
          mutate(link = get_tweet(document_id)) |> select(handle, tweet, link)
      },
      rownames=FALSE,
      options = list(info = FALSE),
      escape = FALSE

    ) # end datatable
    })
  
  
  # date selected
  output$date <- renderText({
    req(event_data("plotly_click"))
    ymd(event_data("plotly_click")$x) |> format('%b %d, %Y')
  })

  
  # common word analysis -------------------
  
  custom_stopwords <- c("http", "https", "rt", "t.co", "ied", "bomb", "explode", "injured", "blast", "blasts")
  

  
  words <- reactive({
    req(landmine_r())
    landmine_r() |>
      group_by(tagged_text, ner_tag) |>
      summarise(total = n()) |>
      arrange(desc(total)) |>
      head(5) |>
      mutate(search_text = str_replace_all(tagged_text, " ", "+"))
  })
  
  output$top_words <- DT::renderDataTable({
    req(event_data("plotly_click"))
    datatable(
      words() |> mutate(link = google_this(search_text),
                        translate = translate_this(search_text)) |> select(tagged_text, link, translate),
      rownames = FALSE,
      escape = FALSE,
      selection = "single",
      options = list(searching = FALSE, paging = FALSE, sort = FALSE, info = FALSE,
                     columnDefs = list(list(className = 'dt-center', targets = '_all')),
                     headerCallback = JS(
                       "function(thead, data, start, end, display){",
                       "  $(thead).remove();",
                       "}")))
    })
  
  #output$word <- renderText({words()$word[input$top_words_rows_selected]})
  
  # Wiki search -----------------------------------------------------------
  wiki <- function(word){unlist(page_content("en", "wikipedia", page_name = word))}
  wiki_no_error <- possibly(wiki, otherwise="No Wikipedia Page Exists")
  
  
  # # add observe event for row click
  observeEvent(input$top_words_rows_selected, {
    word <- words()$tagged_text[input$top_words_rows_selected] 
    
    showModal(modalDialog(
      HTML(wiki_no_error(word)),
            easyClose = TRUE))
  })
  
  # create custom gradient palette 
  colors <-colorRampPalette(c("#30cbcf", "#7cd8c9", "#b4e3c5"))
  
  
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
  
  # create render button
  output$translate_button <- renderUI({
    req(event_data("plotly_click"))
    shiny::a(
      h4(icon(class = "font-awesome", name = "globe"),
         "Translate",
         class = "btn btn-default action-button",
         style = "fontweight:800; background: #0084b4; color:#ffffff; border: 0px; border-radius: 0px"),
      target = "_blank",
      href = paste0("https://translate.google.com/?sl=auto&tl=en&text=",common()[1,1] |> pull(ngram))
    )
  })
  
  




}

shinyApp(ui, server)