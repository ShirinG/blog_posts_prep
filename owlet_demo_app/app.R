load("friends.RData")
load("followers.RData")
load("tidy_descr.RData")
load("topics_beta.RData")
load("topics_gamma.RData")
load("bigram_counts.RData")

library(shiny)
library(shinythemes)

library(tidyverse)
library(tidyquant)
library(tidytext)
library(wordcloud)
library(SnowballC)
library(igraph)
library(ggraph)
library(topicmodels)
library(reshape2)

fr_fol <- friends_df$screenName[which(unique(friends_df$screenName) %in% unique(followers_df$screenName))]
fr_only <- friends_df$screenName[which(!unique(friends_df$screenName) %in% unique(followers_df$screenName))]

friends_followers <- rbind(friends_df, followers_df) %>%
  mutate(group = ifelse(screenName %in% fr_fol, "friend_follower", ifelse(screenName %in% fr_only, "friend", "follower"))) %>%
  arrange(desc(followersCount)) %>%
  distinct(screenName, .keep_all = TRUE)

dataset <- friends_followers %>%
  filter(group != "friend") %>%
  count(lang)

bigrams_separated <- followers_df %>%
  unnest_tokens(bigram, description, token = "ngrams", n = 2) %>%
  filter(!grepl("\\.|http", bigram)) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 == "not") %>%
  filter(!word2 %in% stop_words$word)

#count(bigrams_separated, word1, word2, sort = TRUE)

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

tidy_descr_sentiment <- tidy_descr %>%
  left_join(select(bigrams_separated, word1, word2), by = c("word" = "word2")) %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  rename(nrc = sentiment.x, bing = sentiment.y) %>%
  mutate(nrc = ifelse(!is.na(word1), NA, nrc),
         bing = ifelse(!is.na(word1) & bing == "positive", "negative",
                       ifelse(!is.na(word1) & bing == "negative", "positive", bing)))

tidy_descr_sentiment_count <- tidy_descr_sentiment %>%
  filter(nrc != "positive") %>%
  filter(nrc != "negative") %>%
  gather(x, y, nrc, bing) %>%
  count(x, y, sort = TRUE)

# Define the UI
ui <- fluidPage(theme = shinytheme("paper"),

  titlePanel("Data Science Demo - Owlet",
             windowTitle = "Data Science Demo - Owlet"
             ),

  p("Analysis of Twitter followers can characterize and define customer target groups."),

  tabsetPanel(
    tabPanel("Languages", fluidRow(

      br(),

      column(3,
             h4("Follower language explorer"),
             sliderInput('n_lang', 'Number of followers bigger than',
                         min=min(dataset$n), max=20, value=20,
                         step=1, round=0)),

      column(9,
              plotOutput("plot_lang"))
      )),

    tabPanel("Influencers", fluidRow(

      br(),

      column(3,
             h4("Follower network explorer"),

             p("Followers who have many followers themselves (2nd degree followers) and who tweet regularly are likely most influential among your network and most important for spreading information/advertisement."),

             sliderInput('n_fol', 'Number of 2nd degree followers bigger than',
                         min=min(filter(friends_followers, group != "friend")$followersCount),
                         max=max(filter(friends_followers, group != "friend")$followersCount), value=1000,
                         step=10, round=0)),

      column(9,
             plotOutput("plot_infl"),
             br(),
             plotOutput("plot_infl_tweets"))

    )),

    tabPanel("Common words", fluidRow(

      br(),

      column(3,
             h4("Common words explorer"),

             p("Frequent words and word pairs in your followers' descriptions indicate interests of your target customers."),

             sliderInput('n_com_bi', 'Word pair count bigger than',
                         min=min(bigram_counts$n),
                         max=max(bigram_counts$n)-1, value=1,
                         step=1, round=0),
             br(),
             sliderInput('n_com', 'Wordstem count bigger than',
                         min=min(count(tidy_descr, word_stem, sort = TRUE)$n),
                         max=max(count(tidy_descr, word_stem, sort = TRUE)$n)-1, value=2,
                         step=1, round=0)),

      column(9,
             plotOutput("plot_common_bigram")),

      column(7,
             br(),

             plotOutput("plot_common_bar")),

      column(5,
             plotOutput("plot_common_cloud"),
             br())
    )),

    tabPanel("Sentiments", fluidRow(

      br(),

      column(12,
             p("Sentiment analysis of your followers' descriptions further characterizes your target customers.")),

      column(8,
             sliderInput('n_sent', 'Sentiment count bigger than',
                         min=min(tidy_descr_sentiment_count$n),
                         max=max(tidy_descr_sentiment_count$n)-1, value=min(tidy_descr_sentiment_count$n),
                         step=1, round=0)),
      column(4,
             sliderInput('maxwords', 'Max. number of words to display in wordcloud:',
                         min=1,
                         max=500, value=500,
                         step=10, round=0)),
      br(),

      column(8,
             plotOutput("plot_sentiment")),

      column(4,
             plotOutput("plot_sentiment_cloud")),

      column(12,
             br(),
             plotOutput("plot_sentiment_net"))
    )),

    tabPanel("Topic modeling", fluidRow(

      br(),

      column(12,
             p("Topic modeling uses algorithms to group followers according to topics covered in their descriptions. Beta values (upper plot) describe how characteristic different words are for each topic (higher beta == higher specficity). Gamme values (lower plot) describe followers are with strongest topic affiliations (higher gamma == stronger affiliation).")),

      column(6,
             sliderInput('n_top_words', 'Number of words with highest beta (~ occurrence in topics 1-5):',
                         min=1,
                         max=10, value=4,
                         step=1, round=0)),

      column(6,
             sliderInput('n_top_fol', 'Number of followers with highest gamma (~ affiliation with topics 1-5):',
                         min=5,
                         max=15, value=15,
                         step=1, round=0)),

      br(),

      column(12,
             plotOutput("plot_topic_1")),

      column(12,
             plotOutput("plot_topic_2"))
    ))

    ),

  hr(),
  p("Dr. Shirin Glander"),
  p("June 12, 2017")
)


# Define the server code
server <- function(input, output, session) {

  output$plot_lang <- renderPlot({
    friends_followers %>%
      filter(group != "friend") %>%
      count(lang) %>%
      filter(n < input$n_lang) %>%
      droplevels() %>%
      ggplot(aes(x = reorder(lang, desc(n)), y = n)) +
      geom_bar(stat = "identity", color = palette_light()[1], fill = palette_light()[1], alpha = 0.8) +
      theme_tq() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      labs(x = "language ISO 639-1 code",
           y = "number of followers",
           title = "What languages do your followers speak?",
           subtitle = paste0("Predominant languages of your ", length(unique(friends_followers$screenName)), " followers."),
           caption = "Data: Twitter followers of @SchrederGroup (June 12th 2017)")
  })

  output$plot_infl <- renderPlot({
    friends_followers %>%
      filter(group != "friend") %>%
      ggplot(aes(x = log2(followersCount))) +
      geom_density(color = palette_light()[1], fill = palette_light()[1], alpha = 0.8) +
      theme_tq() +
      labs(x = "log2 of number of followers",
           y = "density",
           title = "How far does your network of followers reach?",
           subtitle = "The majority of your followers is followed by up to ~ 1000 people.",
           caption = "Data: Twitter 2nd degree followers of @SchrederGroup (June 12th 2017)")
  })

  output$plot_infl_tweets <- renderPlot({
    friends_followers %>%
      filter(group != "friend" & followersCount > input$n_fol) %>%
      mutate(date = as.Date(created, format = "%Y-%m-%d"),
             today = as.Date("2017-06-05", format = "%Y-%m-%d"),
             days = as.numeric(today - date),
             statusesCount_pDay = statusesCount / days) %>%
      ggplot(aes(x = statusesCount_pDay)) +
      geom_density(color = palette_light()[1], fill = palette_light()[1], alpha = 0.8) +
      theme_tq() +
      labs(x = "number of tweets per day",
           y = "density",
           title = "How active are your most influential followers?",
           subtitle = paste0("Number of tweets per day of your followers with > ", input$n_fol, " followers."),
           caption = paste("Data: Tweets by", nrow(filter(friends_followers, group != "friend" & followersCount > input$n_fol)), "followers with >", input$n_fol, "followers of @SchrederGroup (June 12th 2017)"))
  })

  output$plot_common_bar <- renderPlot({
    tidy_descr %>%
      count(word_stem, sort = TRUE) %>%
      filter(n > input$n_com) %>%
      ggplot(aes(x = reorder(word_stem, n), y = n)) +
      geom_col(color = palette_light()[1], fill = palette_light()[1], alpha = 0.8) +
      coord_flip() +
      theme_tq() +
      labs(x = "",
           y = "count of word stem in all followers' descriptions",
           title = "What are the most commonly used words in your followers' descriptions?",
           caption = paste("Data: Twitter follower descriptions (btw.", summary(count(tidy_descr, screenName, sort = TRUE)$n)[1], "and", summary(count(tidy_descr, screenName, sort = TRUE)$n)[6], "words) of @SchrederGroup (June 12th 2017)"))
  })

  output$plot_common_cloud <- renderPlot({
    maxwords <- tidy_descr %>%
      count(word_stem, sort = TRUE) %>%
      filter(n > input$n_com)

    tidy_descr %>%
      count(word_stem) %>%
      with(wordcloud(word_stem, n, max.words = nrow(maxwords), colors = palette_light()))
  })

  output$plot_common_bigram <- renderPlot({
    bigram_graph <- bigram_counts %>%
      filter(n > input$n_com_bi) %>%
      graph_from_data_frame()

    set.seed(1)

    a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

    ggraph(bigram_graph, layout = "fr") +
      geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                     arrow = a, end_cap = circle(.07, 'inches')) +
      geom_node_point(color =  palette_light()[1], size = 5, alpha = 0.8) +
      geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
      theme_void() +
      labs(title = "Most commonly used word pairs in followers' descriptions",
           caption = paste("Data: Twitter follower descriptions (btw.", summary(count(tidy_descr, screenName, sort = TRUE)$n)[1], "and", summary(count(tidy_descr, screenName, sort = TRUE)$n)[6], "words) of @SchrederGroup (June 12th 2017)"))
  })

  output$plot_sentiment <- renderPlot({
    tidy_descr_sentiment_count %>%
      filter(n > input$n_sent) %>%
      ggplot(aes(x = reorder(y, n), y = n)) +
      facet_wrap(~ x, scales = "free") +
      geom_col(color = palette_light()[1], fill = palette_light()[1], alpha = 0.8) +
      coord_flip() +
      theme_tq() +
      labs(x = "",
           y = "count of sentiment in followers' descriptions",
           title = "What's the predominant sentiment in your followers' descriptions?",
           subtitle = "Most common sentiments in your followers' descriptions characterize your target customers.",
           caption = "Data: Twitter follower descriptions (corrected for negated meaning) of @SchrederGroup (June 12th 2017)")
  })

  output$plot_sentiment_cloud <- renderPlot({
    tidy_descr_sentiment %>%
      count(word, bing, sort = TRUE) %>%
      acast(word ~ bing, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = palette_light()[1:2],
                       max.words = input$maxwords)
  })

  output$plot_sentiment_net <- renderPlot({
    tidy_descr_sentiment %>%
      count(screenName, word, bing) %>%
      group_by(screenName, bing) %>%
      summarise(sum = sum(n)) %>%
      spread(bing, sum, fill = 0) %>%
      mutate(sentiment = positive - negative) %>%
      ggplot(aes(x = sentiment)) +
      geom_density(color = palette_light()[1], fill = palette_light()[1], alpha = 0.8) +
      theme_tq() +
      labs(title = "Are followers' descriptions mostly positive or negative?",
           subtitle = "The majority of your followers have predominantly positive descriptions.",
           caption = "Data: Twitter follower descriptions (corrected for negated meaning) of @SchrederGroup (June 12th 2017)")
  })

  output$plot_topic_1 <- renderPlot({
    topics_beta %>%
      group_by(topic) %>%
      top_n(input$n_top_words, beta) %>%
      ungroup() %>%
      arrange(topic, -beta) %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(term, beta, color = factor(topic), fill = factor(topic))) +
      geom_col(show.legend = FALSE, alpha = 0.8) +
      scale_color_manual(values = palette_light()) +
      scale_fill_manual(values = palette_light()) +
      facet_wrap(~ topic, ncol = 5) +
      coord_flip() +
      theme_tq() +
      labs(x = "",
           y = "beta (~ occurrence in topics 1-5)",
           title = "Topic modeling: are there groups of followers with specific interests?",
           subtitle = "The top most characteristic words describe topic categories.")

  })

  output$plot_topic_2 <- renderPlot({

    user_topic <- topics_gamma %>%
      group_by(document) %>%
      top_n(1, gamma)

    user_topic %>%
      #filter(gamma > 0.25) %>%
      group_by(topic) %>%
      top_n(input$n_top_fol, gamma) %>%
      ggplot(aes(x = reorder(document, -gamma), y = gamma, color = factor(topic))) +
      facet_wrap(~ topic, scales = "free", ncol = 5) +
      geom_point(show.legend = FALSE, size = 4, alpha = 0.8) +
      scale_color_manual(values = palette_light()) +
      scale_fill_manual(values = palette_light()) +
      theme_tq() +
      coord_flip() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      labs(x = "",
           y = "gamma (~ affiliation with topics 1-5)",
           subtitle = "Followers' affiliation with topics could be used for targeted advertisement.",
           caption = "Data: Twitter follower descriptions of @SchrederGroup (June 12th 2017)")
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)
