library(tidyverse)
library(janitor)
library(plotly)
library(tidytext)
library(textdata)

landmine <- read_csv("landmine_tweets.csv") |>
  select(date, handle, tweet, query_term) |>
  rename(topic = query_term)

mm <- read_csv("mm_tweet_topics.csv") |>
  clean_names() |>
  select(-topic) |>
  rename(handle = foundation_name, 
         date = tweet_date,
         topic = name) |>
  select(date, handle, tweet, topic, handle_type, region) |>
  mutate(topic = gsub("^.{0,2}", "", topic)) |>
  mutate(topic = gsub("_people_at_to_in", "misc", topic)) |>
  mutate(topic = gsub("_", " ", topic)) 

df <- bind_rows(mm, landmine) |>
  mutate(date = as.Date(date))

summary <- landmine |>
  group_by(date, topic) |>
  summarise(total = as.numeric(n()))

test <- summary |> 
  filter(topic == "myanmar burma military myanmars")

class(summary$total)

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
  )




# read afinn words
afinn <- readr::read_csv("./data/afinn.csv")

# update date options
#df$date <- ymd(as.Date(df$date)) %>% format("%d-%m-%Y")

#rename date column
df<- df %>% 
  rename(
    tweet_date= date
  )

#update data format
df$tweet_date <- lubridate::ymd(as.Date(df$tweet_date))

custom_stopwords <- c("http", "https", "rt", "t.co", "ied", "bomb", "explode", "injured", "blast", "blasts")


quads <-  landmine |>
  filter(date == "2023-02-02") |>
  unnest_tokens(quad, tweet, token = "ngrams", n = 15) |>
  count(quad, sort = T) |>
  head(3)

  separate(quad, into = c("word1", "word2", "word3", "word4"), sep = " ") |>
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word,
         !word4 %in% stop_words$word) %>%
  unite(quad, c(word1, word2, word3, word4), sep = " ") |>
  head(25)

feb <- landmine |> filter(date == "2023-02-02")

#analyze words
words <- landmine %>%
  unnest_tokens(word, tweet) %>%
  filter(!word %in% custom_stopwords) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  head(10)

# create custom gradient palette 
colors <-colorRampPalette(c("#30cbcf", "#7cd8c9", "#b4e3c5"))

# plot
output$plot_words <- renderPlot({
  ggplot(words, aes(x=word, y = n, fill = word)) + geom_bar(stat="identity", show.legend = F) + 
    scale_fill_manual(values = colors(10)) + 
    theme(plot.title = element_text(hjust = 0.5, size=18)) +
    ylab("Number of Times Words Appeared") +
    ggtitle("Most Common Words")
})

# join the words with AFINN library
afinn_words <- df %>%
  unnest_tokens(word, text) %>%
  filter (!word %in% custom_stop_words) %>%
  inner_join(afinn, by = "word")


# try to combine this back to original df
df <- afinn_words %>%
  group_by(tweet_date, user) %>%
  summarise(sentiment = sum(value)) %>%
  right_join(df, by = c("user", "tweet_date"))

########### Get the sum of scores for each day
word_scores <- afinn_words %>%
  group_by(week=floor_date(tweet_date, "week")) %>% # group by date
  summarise(score = mean(value)) # add all values for the score

# plot by date
output$plot_sentiment<- renderPlot({
  ggplot(word_scores, aes(x = week, y = score, fill=ifelse(score > 0, T, F))) + geom_bar(stat="identity", show.legend = F) +
    ylim(-2.5,2.5) + theme(plot.title = element_text(hjust = 0.5, size=18)) +
    ggtitle("Average Sentiment Score of Tweets Per Week")
})

# get tweets table by user


output$tweets_df <- renderDT(
  if(input$checkGroup == "No Filter"){
    DT::datatable(df %>% select(tweet_date, user, text, sentiment)) %>%
      formatStyle('text', 'sentiment', color = styleEqual(c(-6,-5,-4,-3,-2,-1,1,2,3,4,5,6, 7,8,9), c('#f8766d','#f8766d','#fb9ca3','#fb9ca3','#fb9ca3','#fb9ca3','#01bfc4','#01bfc4','#01bfc4','#01bfc4','#01bfc4','#01bfc4','#01bfc4','#01bfc4','#01bfc4')))
  } else 
    DT::datatable(df %>% filter(user %in% input$checkGroup) %>% select(tweet_date, user, text, sentiment)) %>%
    formatStyle('text', 'sentiment', color = styleEqual(c(-6,-5,-4,-3,-2,-1,1,2,3,4,5,6,7,8,9), c('#f8766d','#f8766d','#fb9ca3','#fb9ca3','#fb9ca3','#fb9ca3','#01bfc4','#01bfc4','#01bfc4','#01bfc4','#01bfc4','#01bfc4','#01bfc4','#01bfc4','#01bfc4')))
  , options=list(info = F, paging = T, searching = T) 
)





