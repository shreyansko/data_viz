
# Importing Libraries
library(shiny)
library(fresh)
library(urbnmapr)
library(leaflet)
library(RColorBrewer)
library (ggplot2)
library(tidyverse)
library(readxl)
library(shinyWidgets)
library(shinydashboard)
library(ggplot2)
library(maps)
library(viridis)
library(DT)
library(dplyr)
library(igraph)
library(ggnetwork)
library(tidygraph)
library(visNetwork)
library(shinycssloaders)
library(profvis)
library(tidytext)
library(ggthemes)
library(plotly)
library(memoise)
library(tm)
library(qdapRegex)
library(stringr)
library(wesanderson)
library(wordcloud)
library(ggthemes)
#library(hrbrthemes)
#library(systemfonts)
#options(hrbrthemes.loadfonts = TRUE)
#hrbrthemes::import_roboto_condensed()
#hrbrthemes::import_plex_sans()
library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("summarize", "dplyr")
conflict_prefer("box", "shinydashboard")
conflict_prefer("simplify", "igraph")
conflict_prefer("layout", "graphics")
#library(shinydashboardPlus)
#conflict_prefer("dashboardHeader", "shinydashboardPlus")
#conflict_prefer("dashboardSidebar", "shinydashboardPlus")

######################################################## DATA ######################################################## 
# Importing  and cleaning data


######################################################## MAPS

gis <- read_csv("gis_tweet.csv")
gis <- na.omit(gis)
gis <- gis %>%
  filter(user_reported_location_gis !="Mexico, MD")

######################################################## TEXT

############# Wordcloud
getTransformations()

years <<- list("2011" = "MX_0621_tweets_csv_hashed_2011.csv",
               "2012" = "MX_0621_tweets_csv_hashed_2012.csv",
               "2014" = "MX_0621_tweets_csv_hashed_2014.csv",
               "2016" = "MX_0621_tweets_csv_hashed_2016.csv",
               "2019" = "MX_0621_tweets_csv_hashed_2019.csv",
               "2020" = "MX_0621_tweets_csv_hashed_2020.csv",
               "2021" = "MX_0621_tweets_csv_hashed_2021.csv")

getTermMatrix <- memoise(function(year) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(year %in% years))
    stop("Unknown year")
  tweets <- read.csv(year, encoding="UTF-8")
  tweet_text <- select(tweets, tweetid, tweet_text, account_language)
  tweet_text = rename(tweet_text, doc_id = "tweetid", text = "tweet_text")
  df_source <- DataframeSource(tweet_text)
  myCorpus <- VCorpus(df_source)
  
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, "", x))
  myCorpus <- tm_map(myCorpus, toSpace, "@J34OVIQnhNUKlu61gcWfSMl8k3vmgbISNVaLrJD8Z5s=") 
  myCorpus <- tm_map(myCorpus, toSpace, "@NAMXjUkWRJCiDQgDtoJHqjYI63VL9IG5Vi3HNfozclM=:")
  myCorpus <- tm_map(myCorpus, toSpace, "@NgkLmJyBy2mRGrhvjK4uqGBQl1c31sBEikwRkpIk1o=:")
  myCorpus <- tm_map(myCorpus, toSpace,"@kbNmGjiiUM1H2JM87sCoNrRufKhEHQ2b4zHezdHCX0U=")
  myCorpus <- tm_map(myCorpus, content_transformer(rm_twitter_url))
  myCorpus <- tm_map(myCorpus, content_transformer(rm_url))
  myCorpus <- tm_map(myCorpus, content_transformer(removePunctuation))
  myCorpus <- tm_map(myCorpus, toSpace, "¿") 
  myCorpus <- tm_map(myCorpus, toSpace, "¡") 
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  stop_words_s <- read.delim("spanish.txt", header = FALSE)
  stop_words_s <- unlist(stop_words_s)
  print(stop_words_s)
  myCorpus <- tm_map(myCorpus, removeWords, c(stopwords("spanish")))
  myCorpus <- tm_map(myCorpus, removeWords, c(stop_words_s))
  myCorpus <- tm_map(myCorpus, removeWords, c("ighugitodiaz")) 
  myCorpus <- tm_map(myCorpus, removeWords, c("tenientedaaaaan"))
  myCorpus <- tm_map(myCorpus, content_transformer(removeNumbers))
  
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
  
})


############ text barplot
WordCount_2011 = read.csv("WordCount_2011.csv", encoding="UTF-8")
WordCount_2012 = read.csv("WordCount_2012.csv", encoding="UTF-8")
WordCount_2014 = read.csv("WordCount_2014.csv", encoding="UTF-8")
WordCount_2016 = read.csv("WordCount_2016.csv", encoding="UTF-8")
WordCount_2019 = read.csv("WordCount_2019.csv", encoding="UTF-8")
WordCount_2020 = read.csv("WordCount_2020.csv", encoding="UTF-8")
WordCount_2021 = read.csv("WordCount_2021.csv", encoding="UTF-8")



WordCount_2011 <- WordCount_2011 %>% arrange(desc(count)) %>% head(50)
WordCount_2012 <- WordCount_2012 %>% arrange(desc(count)) %>% head(50)
WordCount_2014 <- WordCount_2014 %>% arrange(desc(count)) %>% head(50)
WordCount_2016 <- WordCount_2016 %>% arrange(desc(count)) %>% head(50)
WordCount_2019<- WordCount_2019 %>% arrange(desc(count)) %>% head(50)
WordCount_2020 <- WordCount_2020 %>% arrange(desc(count)) %>% head(50)
WordCount_2021 <- WordCount_2021 %>% arrange(desc(count)) %>% head(50)


######################################################## STATIC

############# Sentiment Hist

tweets_2011 <- read_csv("MX_0621_tweets_csv_hashed_2011.csv")
tweets_2011 <- tweets_2011 %>%
  dplyr::select(tweetid, tweet_text, userid, user_display_name, follower_count,
                following_count, account_creation_date, tweet_time) %>%
  mutate(userid = as.character(userid),
         year = 2011)

tweets_2012 <- read_csv("MX_0621_tweets_csv_hashed_2012.csv")
tweets_2012 <- tweets_2012 %>%
  dplyr::select(tweetid, tweet_text, userid, user_display_name, follower_count,
                following_count, account_creation_date, tweet_time) %>%
  mutate(userid = as.character(userid),
         year = 2012)

tweets_2014 <- read_csv("MX_0621_tweets_csv_hashed_2014.csv")
tweets_2014 <- tweets_2014 %>%
  dplyr::select(tweetid, tweet_text, userid, user_display_name, follower_count,
                following_count, account_creation_date, tweet_time) %>%
  mutate(userid = as.character(userid),
         year = 2014)

tweets_2016 <- read_csv("MX_0621_tweets_csv_hashed_2016.csv")
tweets_2016 <- tweets_2016 %>%
  dplyr::select(tweetid, tweet_text, userid, user_display_name, follower_count,
                following_count, account_creation_date, tweet_time) %>%
  mutate(userid = as.character(userid),
         year = 2016)

tweets_2019 <- read_csv("MX_0621_tweets_csv_hashed_2019.csv")
tweets_2019 <- tweets_2019 %>%
  dplyr::select(tweetid, tweet_text, userid, user_display_name, follower_count,
                following_count, account_creation_date, tweet_time) %>%
  mutate(userid = as.character(userid),
         year = 2019)

tweets_2020 <- read_csv("MX_0621_tweets_csv_hashed_2020.csv")
tweets_2020 <- tweets_2020 %>%
  dplyr::select(tweetid, tweet_text, userid, user_display_name, follower_count,
                following_count, account_creation_date, tweet_time) %>%
  mutate(userid = as.character(userid),
         year = 2020)

tweets_2021 <- read_csv("MX_0621_tweets_csv_hashed_2021.csv")
tweets_2021 <- tweets_2021 %>%
  dplyr::select(tweetid, tweet_text, userid, user_display_name, follower_count,
                following_count, account_creation_date, tweet_time) %>%
  mutate(userid = as.character(userid),
         year = 2021)

tweets_all <- bind_rows(tweets_2011, tweets_2012, tweets_2014, tweets_2016,
                        tweets_2019, tweets_2020, tweets_2021)

#cleaning combined df

tweets_clean_all <- tolower(tweets_all$tweet_text)
tweets_clean_all <- gsub("rt", " ", tweets_clean_all)
tweets_clean_all <- gsub("@\\w+", " ", tweets_clean_all)
tweets_clean_all <- gsub("http.+ |http.+$", " ", tweets_clean_all)
tweets_clean_all <- gsub("[[:punct:]]", " ", tweets_clean_all)
tweets_clean_all <- gsub("[ |\t]{2,}", " ", tweets_clean_all)
tweets_clean_all <- gsub("amp", " ", tweets_clean_all)
tweets_clean_all <- gsub("\n ", "", tweets_clean_all)
tweets_clean_all <- gsub("^ ", "", tweets_clean_all)
tweets_clean_all <- gsub(" $", "", tweets_clean_all)
tweets_clean_all <- gsub(" +", " ", tweets_clean_all)
#tweets_clean_all <- unique(tweets_clean_all)  # Now get rid of duplicates!

clean_tweet_all <- as_tibble(tweets_clean_all)
clean_tweet_all$doc_id <- 1:nrow(clean_tweet_all)

tweets_all_comb <- bind_cols(clean_tweet_all, tweets_all)
tweets_all_comb <- tweets_all_comb %>%
  separate(tweet_time, into = c("TweetDate", "TweetTime"), sep = " ") %>%
  mutate(account_creation_date = as.Date(account_creation_date),
         TweetDate = as.Date(TweetDate))
tweets_all_comb <- tweets_all_comb %>%
  mutate(account_Tweet_time = TweetDate - account_creation_date) %>%
  separate(account_Tweet_time, into = c("account_Tweet_time", "day"),
           sep = " ") %>%
  select(-day) %>% mutate(account_Tweet_time = as.numeric(account_Tweet_time))

positive <- read.delim("positive_words_es.txt", header = FALSE)
positive <- positive %>% transmute(word = V1,
                                   value = 1)
negative <- read.delim("negative_words_es.txt", header = FALSE)
negative <- negative %>% transmute(word = V1,
                                   value = -1)

stop_words <- read.delim("spanish.txt", header = FALSE)
stop_words <- stop_words %>% transmute(word = V1)

sentiment <- bind_rows(positive, negative)

sentiment_all <- tweets_all_comb %>%
  unnest_tokens(word, value) %>%
  anti_join(stop_words) %>%
  left_join(sentiment) %>%
  group_by(doc_id) %>%
  summarise(sent_total = sum(value, na.rm = TRUE)) %>%
  left_join(tweets_all_comb)  %>%
  mutate(Sentiment = case_when(sent_total > 0 ~ "Positive",
                               sent_total < 0 ~ "Negative",
                               sent_total == 0 ~ "Neutral"))

wes_pal <- wes_palette("Darjeeling1", 5)
#wes_pal_wordcloud <- wes_palette("Darjeeling1", 8)



############# Cumulative tweets
tweets_ct <- read_csv("tweet count by month.csv")

tweets_ct <- tweets_ct %>%
  dplyr::group_by(Year, Month) %>%
  dplyr::summarise(n = sum(n))

tweets_ct <- tweets_ct %>% # number of tweets month to month
  pivot_wider(id_cols = Year, names_from = Month,
              values_from = n, values_fill = 0) %>%
  dplyr::select(Year, `01`, `02`, `03`, `04`, `05`, `06`, `07`,
                `08`, `09`, `10`, `11`, `12`) %>%
  ungroup() %>%
  add_row(Year = 2013, `01` = NA, `02` = NA, `03` = NA, `04`= NA, `05`= NA, `06` = NA,
          `07` = NA, `08` = NA, `09` = NA, `10` = NA, `11` = NA, `12` = NA) %>%
  add_row(Year = 2015, `01` = NA, `02` = NA, `03` = NA, `04`= NA, `05`= NA, `06` = NA,
          `07` = NA, `08` = NA, `09` = NA, `10` = NA, `11` = NA, `12` = NA) %>%
  add_row(Year = 2017, `01` = NA, `02` = NA, `03` = NA, `04`= NA, `05`= NA, `06` = NA,
          `07` = NA, `08` = NA, `09` = NA, `10` = NA, `11` = NA, `12` = NA) %>%
  add_row(Year = 2018, `01` = NA, `02` = NA, `03` = NA, `04`= NA, `05`= NA, `06` = NA,
          `07` = NA, `08` = NA, `09` = NA, `10` = NA, `11` = NA, `12` = NA) %>%
  pivot_longer(2:13, names_to = "Month") %>%
  ungroup() %>%
  arrange(Year) %>%
  head(125) %>%
  mutate(cumsum = cumsum(ifelse(is.na(value), 0, value)) + value*0,
         order = 1:125) %>%
  unite("MonYr", Month:Year, sep= "/", remove = FALSE)


############# Pos Neg Wordcloud

allvar_all <- read_csv("allvar_all.csv")

tweets_clean_all <- tolower(allvar_all$tweet_text)
tweets_clean_all <- gsub("rt", " ", tweets_clean_all)
tweets_clean_all <- gsub("@\\w+", " ", tweets_clean_all)
tweets_clean_all <- gsub("http.+ |http.+$", " ", tweets_clean_all)
tweets_clean_all <- gsub("[[:punct:]]", " ", tweets_clean_all)
tweets_clean_all <- gsub("[ |\t]{2,}", " ", tweets_clean_all)
tweets_clean_all <- gsub("amp", " ", tweets_clean_all)
tweets_clean_all <- gsub("\n ", "", tweets_clean_all)
tweets_clean_all <- gsub("^ ", "", tweets_clean_all)
tweets_clean_all <- gsub(" $", "", tweets_clean_all)
tweets_clean_all <- gsub(" +", " ", tweets_clean_all)

clean_tweet_all <- as_tibble(tweets_clean_all)
clean_tweet_all$doc_id <- 1:nrow(clean_tweet_all)

tweets_all_comb <- bind_cols(clean_tweet_all, allvar_all)
tweets_all_comb <- tweets_all_comb %>%
  separate(tweet_time, into = c("TweetDate", "TweetTime"), sep = " ") %>%
  mutate(account_creation_date = as.Date(account_creation_date),
         TweetDate = as.Date(TweetDate))
tweets_all_comb <- tweets_all_comb %>%
  mutate(account_Tweet_time = TweetDate - account_creation_date) %>%
  separate(account_Tweet_time, into = c("account_Tweet_time", "day"),
           sep = " ") %>%
  select(-day) %>% mutate(account_Tweet_time = as.numeric(account_Tweet_time))

positive <- read.delim("positive_words_es.txt", header = FALSE)
positive <- positive %>% transmute(word = V1,
                                   value = 1)
negative <- read.delim("negative_words_es.txt", header = FALSE)
negative <- negative %>% transmute(word = V1,
                                   value = -1)

stop_words <- read.delim("spanish.txt", header = FALSE)
stop_words <- stop_words %>% transmute(word = V1)

sentiment <- bind_rows(positive, negative)

sentiment_all_word <- tweets_all_comb %>%
  unnest_tokens(word, value) %>%
  anti_join(stop_words) %>%
  left_join(sentiment) %>%
  group_by(doc_id) %>%
  summarise(sent_total = sum(value, na.rm = TRUE)) %>%
  left_join(tweets_all_comb)

sentiment_all_word <- sentiment_all_word %>%
  mutate(sent_cat = case_when(sent_total > 0 ~ "Positive",
                              sent_total < 0 ~ "Negative",
                              sent_total == 0 ~ "Neutral"))
positive_tweets <- sentiment_all_word %>%
  filter(sent_cat == "Positive") %>%
  select(doc_id, value, sent_total, sent_cat) %>%
  unnest_tokens(word, value) %>%
  anti_join(stop_words) %>%
  #  semi_join(dictionary, by = c("word" = "value")) %>%
  select(word) %>%
  t() %>%
  as.tibble() %>%
  unite(col = "Positive", V1:V49964, sep = " ")

negative_tweets <- sentiment_all_word %>%
  filter(sent_cat == "Negative") %>%
  select(doc_id, value, sent_total, sent_cat) %>%
  unnest_tokens(word, value) %>%
  anti_join(stop_words) %>%
  #  semi_join(dictionary, by = c("word" = "value")) %>%
  select(word) %>%
  t() %>%
  as.tibble() %>%
  unite(col = "Negative", V1:V40044, sep = " ")

negative_corpus <- SimpleCorpus(VectorSource(negative_tweets$Negative))
positive_corpus <- SimpleCorpus(VectorSource(positive_tweets$Positive))
combined_corpus <- tm:::c.VCorpus(negative_corpus, positive_corpus)
corpus <- tm_map(combined_corpus, PlainTextDocument)

combined_dtm <- DocumentTermMatrix(corpus)
combined_m <- t(as.matrix(combined_dtm))
colnames(combined_m) <- c("Negative tweets", "Positive tweets")


######################################################## NETWORK
hashtags_2011 = read_csv("hashtags_2011.csv")
hashtags_2012 = read_csv("hashtags_2012.csv")
hashtags_2014 = read_csv("hashtags_2014.csv")
hashtags_2016 = read_csv("hashtags_2016.csv")
hashtags_2019 = read_csv("hashtags_2019.csv")
hashtags_2020 = read_csv("hashtags_2020.csv")
hashtags_2021 = read_csv("hashtags_2021.csv")


hashtags_2011 <- hashtags_2011 %>% select(-"...1",-"hashtags", -"split_hash")
hashtags_2012 <- hashtags_2012 %>% select(-"...1")
hashtags_2014 <- hashtags_2014 %>% select(-"...1")
hashtags_2016 <- hashtags_2016 %>% select(-"...1")
hashtags_2019 <- hashtags_2019 %>% select(-"...1")
hashtags_2020 <- hashtags_2020 %>% select(-"...1")
hashtags_2021 <- hashtags_2021 %>% select(-"...1",-"...2")

##### 2011
hash_2011 <- as.matrix(t(as.matrix(hashtags_2011)) %*% as.matrix(hashtags_2011))
hash_network_2011 <- graph.adjacency(hash_2011, mode ="undirected",diag=F)
#V(hash_network_2011)$color <- "#F2AD00"


##### 2012
hash_2012 <- as.matrix(t(as.matrix(hashtags_2012)) %*% as.matrix(hashtags_2012))
hash_network_2012 <- graph.adjacency(hash_2012, mode ="undirected",diag=F)
#V(hash_network_2012)$color <- "#F2AD00"

##### 2014
hash_2014 <- as.matrix(t(as.matrix(hashtags_2014)) %*% as.matrix(hashtags_2014))
hash_network_2014 <- graph.adjacency(hash_2014, mode ="undirected",diag=F)
#V(hash_network_2014)$color <- "#F2AD00"

##### 2016
hash_2016 <- as.matrix(t(as.matrix(hashtags_2016)) %*% as.matrix(hashtags_2016))
hash_network_2016 <- graph.adjacency(hash_2016, mode ="undirected",diag=F)
#V(hash_network_2016)$color <- "#F2AD00"

##### 2019
hash_2019 <- as.matrix(t(as.matrix(hashtags_2019)) %*% as.matrix(hashtags_2019))
hash_network_2019 <- graph.adjacency(hash_2019, mode ="undirected",diag=F)
#V(hash_network_2019)$color <- "#F2AD00"

##### 2020
hash_2020 <- as.matrix(t(as.matrix(hashtags_2020)) %*% as.matrix(hashtags_2020))
hash_network_2020 <- graph.adjacency(hash_2020, mode ="undirected",diag=F)
#V(hash_network_2020)$color <- "#F2AD00"

##### 2021
hash_2021 = as.matrix(t(as.matrix(hashtags_2021)) %*% as.matrix(hashtags_2021))
hash_network_2021 <- graph.adjacency(hash_2021, mode ="undirected", diag = F)
#V(hash_network_2021)$color <- "#F2AD00"

# Delete nodes
#graph_2021 <- delete.vertices((degree_x_2021), degree((degree_x_2021))==0)

#vis_graph_2021 <- toVisNetworkData(graph_2021)


######################################################## ADDITIONAL
year_choose <- tibble::tribble(
  ~year,
  "2011",
  "2012",
  "2014",
  "2016",
  "2019",
  "2020",
  "2021"
)

########################
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#5BBCD6"
  ),
  adminlte_sidebar(
    width = "300px",
    dark_bg = "#daeff5",
    dark_hover_bg = "#00A08A",
    dark_color = "#006356"
  ),
  adminlte_global( 
    content_bg = "#FFF" ,#FFF
   # box_bg = "#f7ebcd", 
    info_box_bg = "#f7ebcd"  #F2AD00
  )
)


######################################################## UI ######################################################## 

# Define UI for application that draws a histogram
ui <- dashboardPage( #dashboardPage
  dashboardHeader(title = "Mexican Twitter Disinformation", titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("book-open")),
      menuItem("Meet the Team", tabName = "aboutus", icon = icon("address-book")),
      menuItem("Map", tabName = "map", icon = icon("map-marker", lib ="glyphicon")),
      menuItem("Text", tabName = "text", icon = icon("text-size", lib ="glyphicon")),
      menuItem("Network", tabName = "network", icon = icon("th")),
      menuItem("Source", tabName = "source", icon = icon("ellipsis-h"))
    )
  ),
  dashboardBody( chooseSliderSkin("Modern"), use_theme(mytheme),
                 setSliderColor(c('#EDAA02','#EDAA02','#EDAA02','#EDAA02','#EDAA02','#EDAA02','#EDAA02','#EDAA02'), c(1, 2, 3, 4, 5, 6, 7, 8, 9)),
                 tags$style(type = "text/css", "html, body {width:100%;height:100%}", ".irs-bar {",
                            "  border-color: #5ABCD6;",
                            "  background-color: #5ABCD6;",
                            "}", ".irs-bar-edge {",
                            "  border-color: #5ABCD6;",
                            "  background-color: transparent;",
                            "}", "layout(font = list(family = “times”)", 
                            "#sidebar {
               background-color: #5ABCD6;
                   }", "layout(font = list(family = “times”)"),
    tabItems( 
      tabItem(tabName = "introduction",
              box(htmlOutput("intro"),
              plotlyOutput("graph_text")%>% 
                withSpinner(color="#082e45"), width = '100%')), #############
      tabItem(tabName = "aboutus",
              uiOutput("linkedin_shrey"),
              uiOutput("linkedin_kata"),
              uiOutput("linkedin_danny"),
              uiOutput("linkedin_andrea"),
              htmlOutput("aboutus")), #############
      tabItem(tabName = "map",
              tabBox(
                title = "Location of 2011-2021 Mexican Disinformation Tweets",
                width = 20,
                tabPanel("Map", fluidRow(box(helpText('You can take a look at the how the number of tweets changed over time in different cities in Mexico'),
                           selectInput('type', label = "Would you like to see it aggregated or not?", 
                                       choices = c('Aggregated', 'Not Aggregated'), 
                                       selected = 'Not Aggregated'),
                           height = '150px'),
                           box(sliderTextInput("time_gis", "Time:",
                                           choices = c(2011, 2012, 2014, 2016, 2019, 2020, 2021), 
                                           grid = TRUE,
                                           animate = animationOptions(interval = 600, loop = TRUE),
                                           selected = 2011),
                               height = '150px')),
              fluidRow(box(leafletOutput("twitter_map", width = "100%", height = 900)%>% 
                             withSpinner(color="#082e45"), width = '1000px'))),
              tabPanel("Explanation", 
                       fluidRow(box(htmlOutput("map_expl"), width = '85%', height = '100%')))
              )), #############
      tabItem(tabName = "text", 
              tabBox(
                title = "Text Analysis",
                width = 18,
                tabPanel("Sentiment Distribution",
                         box(sliderTextInput(inputId = "year_text", label = "Year:", 
                                             choices = year_choose$year,
                                             grid = TRUE, width = '100%'), width = '100%'),
                         plotOutput("hist_text") %>% withSpinner(color="#082e45")),
              tabPanel("Sentiment Wordcloud",
                     box(plotOutput("wordcloud_sent", height = "1000px") %>% 
                           withSpinner(color="#082e45"), height = "1100px", width = "1000px"), style='height: 1100px'),
              tabPanel("Tweet Wordcloud" , style='height: 1000px',
                       fluidRow(box(column(8, selectInput("selection", "Choose a year:",
                                                          choices = years),
                                           actionButton("update", "Update")),
                                    height = '200px'),
                                box(column(12, sliderInput("freq",
                                                           "Minimum Frequency:",
                                                           min = 1,  max = 50, value = 15),
                                           sliderInput("max",
                                                       "Maximum Number of Words:",
                                                       min = 1,  max = 300,  value = 100)),
                                    height = "200px")),
                       fluidRow(plotOutput("plot_wordcloud", height = "800px")%>% withSpinner(color="#082e45"))),
              tabPanel("Top Misinformation Words", 
                       fluidRow(box(selectInput("year_textbar", "Choose a year:",
                                                choices = year_choose$year, selected = 2011),
                                    actionButton("words_update", "Go"), height = '150px'),
                                box(sliderInput("bins",
                                                "Number of words:",
                                                min = 1,
                                                max = 30,
                                                value = 15), height = '150px')),
                       fluidRow(box(plotOutput("wordcount_bargraph", width = 900, height = 800)%>% 
                                      withSpinner(color="#082e45"), width = "1000px"))),
              tabPanel("Explanation",
                       fluidRow(box(htmlOutput("text_expl"), width = '85%', height = '100%')))
              )), #############
      tabItem(tabName = "network", 
              tabBox( height = '1000px',
                title = "Hashtag Network",
                width = '800px',
                tabPanel("Interactive Network",
                         fluidRow(box(sliderTextInput(inputId = "year_interactive", label = "Year:", 
                                             choices = year_choose$year,
                                             grid = TRUE),
                             height = '100px'),
                         box(sliderInput(inputId = "nodes_size", label = "Size of Nodes and Labels:",
                                        min = 40, max = 100, value = 45,ticks = TRUE), 
                             height = '100px')), 
                         fluidRow(box(visNetworkOutput("hashtag_network_interactive", height = '780px') %>% #, width = '1050px'
                               withSpinner(color="#082e45"), width = '100%', height = '100%')
                              )),
                tabPanel("Static Network",
                         fluidRow(box(sliderTextInput(inputId = "year_static", label = "Year:", 
                                             choices = year_choose$year,
                                             grid = TRUE),
                             height = '100px'),
                         box(awesomeRadio(inputId = "nodes",
                                          label = "Nodes:",
                                          choices = c("All Nodes", "Connected Nodes Only"),
                                          inline = TRUE,
                                          checkbox = TRUE),
                             height = '100px')), 
                         fluidRow(box(plotOutput("hashtag_network_static", height = '780px') %>% #width = '1050px', height = '800px'
                           withSpinner(color="#082e45"), width = '100%', height = '100%'))),
                tabPanel("Explanation", 
                         fluidRow(box(htmlOutput("network_expl"), width = '85%', height = '100%'))) #, width = "100%"
              )), #############
      tabItem(tabName = "source",
              fluidRow(box(uiOutput("sources"), width = '85%', height = '100%')))
      )
    ))


######################################################## SERVER ######################################################## 
server <- function(input, output) {
  

  output$intro <- renderUI({
    HTML(paste("<h2> Welcome to our final project for Data Visualization! </h2> </br>

<p>Widespread concerns over Twitter’s capacity to spread misinformation have led to global political and academic attention. Leveraging a dataset from the Twitter Transparency Center, this project zooms into Mexico specifically to explore its Twitter disinformation landscape.</p>
<p>The team made use of several visualization packages in R to inspect Mexican Twitter disinformation from textual, geospatial, and network angles. You can navigate to these analyses by clicking through the tabs to the left of this page. Additionally, you may learn more about the team members on the Meet the Team page and feel free to contact us regarding the project.</p> 
<p>The first of these visualizations is below, where you can see the rapid increase of disinformation tweets in Mexico year over year. The chart allows you to see both cumulative and month-to-month counts of tweets. You may scroll over the chart to identify the specific number of tweets in a given month.</p>
<p>With this as with the other charts on the website, please note that there is data available for 2011-2012, 2014, 2016, and 2019-2021. Data for the years in between were not available.</p>
<p>Please enjoy exploring!</p><br>
<p>Cheers,</p>

<p> Shrey, Kata, Danny, and Andrea</p>
", sep ="<br/>"))
  })
  
  shrey <- a("Shreyans Kothari", href = "https://www.linkedin.com/in/shreyans-kothari/")
  kata <- a("Kata Mezo", href = "https://www.linkedin.com/in/kata-mezo-501627153")
  danny <- a("Danielle Murad", href = "https://www.linkedin.com/in/daniellemuradwaiss/")
  andrea <- a("Andrea Tillotson", href = "https://www.linkedin.com/in/andrea-tillotson-878258139/")
  
  output$linkedin_shrey <- renderUI({
    tagList(HTML(paste("<h1> About the team </h1> <br>")), div(img(src = "about_us.jpeg", style="display: block; margin-left: auto; margin-right: auto;")), HTML(paste("<br> <br>")), shrey, HTML(paste("is pursuing a Masters degree in Data Science at the Columbia University Graduate School of Arts and Sciences. After the conclusion of his undergraduate degree in 2019, Shreyans worked in innovation and strategy in international development. He has worked at a few different organizations, including Save the Children and the United Nations Foundation in Washington DC. Shreyans can be reached at sk4819@columbia.edu.<br><br>")))
  })
  output$linkedin_kata <- renderUI({
    tagList(kata, HTML(paste("is about to graduate from Columbia University’s Masters in Quantitative Methods in the Social Sciences program (Data Science focus). She has two B.A.s, one in Sociology and one in Applied Economics. In 2020, she worked as a grants and incentives consultant at Deloitte in Hungary. After she got admitted to Columbia, she started working as a research assistant at Mailman Public Health Department. Kata can be reached at km3697@columbia.edu<br><br>")))
  })
  output$linkedin_danny <- renderUI({
    tagList(danny, HTML(paste("is a dual national of both Mexico and Canada pursuing a dual Masters degree in International Affairs with a focus on International Security Policy and in Data Science at Columbia University. She works as a Research Assistant for the Cybersecurity Program at SIPA. She began her career with a focus on counter terrorism but have since shifted to cyber security. Prior to her Masters, she worked in various organizations including the UK Parliament, the NATO Defense College in Rome, the United Nations Headquarters in New York and the International Institute for Counter-Terrorism in Israel.<br><br>")))
  })
  output$linkedin_andrea <- renderUI({
    tagList(andrea, HTML(paste("is a soon to be graduate of Columbia University’s Masters in Quantitative Methods in the Social Sciences program (Data Science focus). She received her B.A. in Political Science from the University of Michigan in 2020 before working briefly at the Pew Research Center. Andrea can be reached at a.tillotson@columbia.edu.<br><br>")))
  })
  
  output$aboutus <- renderUI({
    HTML(paste("Please feel free to reach out on LinkedIn or email if you have any comments/questions regarding the Dashboard", 
               sep ="<br/>"))
    })
############################ MAPS
  
  filteredData <- reactive({
    #add rollified thing
    till<- input$time_gis
    gis%>%
      group_by(longitude_gis, latitude_gis) %>%
      filter(if (input$type == "Aggregated") time_gis <= till else time_gis == till) %>%
      summarize(num = sum(n_gis))
  })
  
  output$twitter_map <- renderLeaflet({
    print ("render tiles")
    leaflet(gis, data = filteredData()) %>%
      addTiles () %>%
      setView(-99.134007, 19.380002, zoom = 5) %>%
      clearShapes() %>%
      addCircles(lng = ~longitude_gis, lat = ~latitude_gis,
                 radius = ~num*100, color = wes_pal[5] ) %>%
      addProviderTiles(providers$CartoDB.Positron)
  })
  
  output$map_expl <- renderUI({
    HTML(paste("<h2> GIS Analysis </h2> <br>
                <h4>In this section, we show how the locations of Mexican Disinformation Tweets have changed over the years. The map can help to explore the dataset in space while making the general trends be more visible for the user. The user can choose whether they want to see the aggregated number of tweets until the chosen year  or the exact number of tweets in the chosen year on the map. Please note that our dataset does not include every year from 2011 until 2021. </h4><br>
               <h3> Map: </h3> 
               <p>We saw that over the year the number of tweets has increased dramatically, and our map shows a similar trend as well. From 2016 the misinformation tweets started to appear in more locations compared to 2011, 2012 and 2013, when all the misinformation tweets were concentrated in two areas (Hermosillo and Veracruz). This trend can be attributed to the increasing popularity of social media fenomena. From 2020 to 2021, there is a drastic increase in Mexico city in the number of tweets. This drastic increase can be attributed to the 2021 legislative election in Mexico, and that Mexico city is the bureaucratic capital city as well (Valenzuela, Muñiz & Santos, 2022). This might suggest that over the years misinformation tweets started to be more connected with political related topics.</p> <br>
               <h3> Resource: </h3> 
               <p>Valenzuela, S., Muñiz, C., & Santos, M. (2022). Social Media and Belief in Misinformation in Mexico: A Case of Maximal Panic, Minimal Effects? The International Journal of Press/Politics. https://doi.org/10.1177/19401612221088988: https://journals.sagepub.com/doi/full/10.1177/19401612221088988?casa_token=eBuiK_E2KjwAAAAA%3AGnT93obIxP3yKYsrEFnk2J0DcNP8Ov1uJZs1WL67UvahCHYEVFE3stMR44KRZErYsyVF9FKUD2xAMg</p> <br>
               <br><br><br><br><br><br><br><br><br><br><br><br><br>", sep ="<br/>"))
  })

  
############################ TEXT
  data <- reactive({
    sentiment_all %>%
      filter(year == input$year_text)
  })
  
  output$hist_text <- renderPlot({
    ggplot(data(), aes(sent_total, fill = Sentiment)) +
      geom_bar() +
      scale_fill_manual(values = c(wes_pal[1], wes_pal[3], wes_pal[2])) + #1 3 2
      labs(title = paste("Distribution of sentiment for", input$year_text, "Mexican disinformation tweets"),
           subtitle = paste(nrow(data()), "total disinformation tweets in ", input$year_text),
           x = "Sentiment of disinformation tweets",
           y = "# of tweets") +
      theme_tufte() +
      theme(plot.title = element_text(face = "bold",hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
  })
  
  output$wordcloud_sent <- renderPlot({
    comparison.cloud(combined_m, color = c(wes_pal[1], wes_pal[2]), max.words = 200,
                     scale = c(5, 1),
                     title.size = 3, title.colors = "white",
                     title.bg.colors = c(wes_pal[1], wes_pal[2]))
  })
  
  ######## wordcloud
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot_wordcloud <- renderPlot({
    v <- terms()
    #pal <- wes_palette("Darjeeling2", 8, type = "continuous")
    wordcloud_rep(names(v), v, scale=c(6,0.75),
                  min.freq = input$freq, max.words=input$max,
                  colors=(values = c(wes_pal[1], wes_pal[3], wes_pal[4], wes_pal[5], wes_pal[2] ))) #wes_palette("Darjeeling1")
  })
  
  ####### text barplot
  wordCountYearInput <- eventReactive(input$words_update, {
    switch(input$year_textbar,
           "2011" = WordCount_2011,
           "2012" = WordCount_2012,
           "2014" = WordCount_2014,
           "2016" = WordCount_2016,
           "2019" = WordCount_2019,
           "2020" = WordCount_2020,
           "2021" = WordCount_2021
    )
  })
  
  bin <- reactive({
    req(
      switch(input$bins,
             "1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5,
             "6" = 6, "7" = 7, "8" = 8, "9" = 9, "10" = 10,
             "11" = 11, "12" = 12, "13" = 13, "14" = 14, "15" = 15,
             "16" = 16, "17" = 17, "18" = 18, "19" = 19, "20" = 20,
             "21" = 21, "22" = 22, "23" = 23, "24" = 24, "25" = 25,
             "26" = 26, "27" = 27, "28" = 28, "29" = 29, "30" = 30,))
  })
  output$"wordcount_bargraph" <- renderPlot({
    x <- wordCountYearInput()$words
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    y <- wordCountYearInput()$count
    
    df <- wordCountYearInput()
    df_filter <- df %>% arrange(desc(count)) %>% head(bin()) 
    
    p <- ggplot(df_filter, aes(x = df_filter$count, y = reorder(df_filter$word, df_filter$count)), fill = wes_pal[5]) + geom_bar(stat = "identity", pos = "dodge", fill = wes_pal[5]) + 
      labs(title = paste("Top words used in misinformation tweets for year", input$yrs[1]),
           subtitle = paste(nrow(data()), "total disinformation tweets in ", input$year[1]),
           x = "Word Count", y = NULL) + 
      theme_tufte() +
      theme(plot.title = element_text(face = "bold",hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
    
    p + theme(axis.text = element_text(size = 14)) 
    
  })
  
  output$text_expl <- renderUI({
    HTML(paste("<h4>This section explores the actual text of the Mexican disinformation tweets, including word frequency and tweet sentiment over time. Research has shown that disinformation on social media sites displays a peculiar trend: accounts that spread disinformation often started as pop culture-focused accounts. By visualizing common words and sentiment of all tweets in the Mexican disinformation data set as well as the particular patterns of two high-volume users, we can see that the pattern claimed in previous research in fact does apply to Mexican disinformation tweets.</h4><br>
               <h3> Sentiment Analysis: </h3> 
               <p>In this section, we explore the underlying sentiment of the Mexican disinformation tweets. The visualizations were created using the tm, tidytext, ggplot2, and wordcloud packages. Because the tweets were in Spanish, we also leveraged a comprehensive list of Spanish stop words and a sentiment lexicon from Kaggle. After tokenizing and removing stop words, positive words were scored 1 and negative words were scored -1. The scores were then aggregated to assign each tweet its own combined sentiment score.</p> <br>
               <h4> Sentiment Distribution: </h4> 
               <p>This visualization shows the change in sentiment distribution scores of disinformation tweets from year-to-year over the 2011 to 2021 period (with years 2013, 2015, 2017-2018 missing). To use the visualization, simply drag the year slider to the year you are interested in viewing.</p><br>
               <h4> Sentiment Wordcloud: </h4> 
               <p>This visualization shows the most common words in tweets that were labeled positive or negative. The words themselves are not necessarily positive or negative, but are frequently included in positive or negative tweets. That being said, we can see that many of the words in the positive cloud fall along more positive lines. Words like alianza (alliance), ventaja (advantage), and apoyo (support) do in fact carry a more positive tinge. Words like limitado (limited), perder (to lose), and renuncia (resignation) carry the more negative tinge. However, across both clouds there are also some fairly neutral words: gobierno (government), encuesta (poll), and gente (people).</p> <br>
               <h3> Tweet Analysis: </h3>
               <p> This section begins to explore the context contained in the body of the tweets by mapping the trends and how topics of misinformation campaigns have changed in Mexico over time. The visualizations in this section used the tm, tidytext, ggplot and wordcloud packages. The text was preprocessed by removing symbols and numbers, with the exceptions of accents in Spanish, twitter urls in the text and turning all letters into lower case, tokenizing and removing stopwords and then calculating the term frequency-inverse document frequency (tf/idf), a measure that quantifies the importance of specific words in document. This measure allows us to identify word saliency which enables the visualizations in this section.  </p> <br>
               <h4> Tweet Wordcloud and Top Misinformation Words: </h4> 
               <p>Both the word cloud and the top disinformation words graphs show the evolution of topics in the misinformation campaigns as the years go by. Users are able to toggle through the years, maximum numbers of words and term frequency in the word cloud and year and number of words in the bar graph to identify the most common topics discussed in disinformation tweets each year. </p>
               <p>Both these visualizations clearly illustrate the evolution of the topics being discussed. In 2011 the most common word was “difícildecreer” this is a hashtag used that means “hard to believe”. This makes reference to a TV show by this name, that aired in the Azteca channel, which makes the presence of “aztecaamerica” make sense. In 2012 the common words include “soytudoble” which makes reference to a different TV translated to English as “I am your double”. As such it makes sense that a lot of the words make reference to castings and auditions as they are looking for individuals that look like celebrities. This show continues to be the main topic of conversation in 2014 as well.</p>
               <p> In 2016 we begin to see a shift in topics, while there are still pop culture references, there is also an increase in the political discourse included in the tweets. By 2019 however, tweet content has evolved significantly. The words included are now discussing Mexico, AMLO (short for the elected President Andrés Manuel López Obrador), they refer to violence and the elections. Even so, we continue to see new and old references that continue to reference popular tv shows as well. By  2021, we can see that the tweets have become very political. They discuss the elections, the political parties like Morena and the PAN and so on. </p>
               <p> Through these interactive visualizations, we are able to trace the evolution of content. This analysis shows how accounts likely used these pop culture references of popular tv shows to build up their before shifting to a more political discourse at the same time that elections and the political situation in the country became more tense. </p>
               <br><br><br><br><br><br><br><br><br><br><br><br><br>", sep ="<br/>"))
  })
  
############################ STATIC
  
  output$graph_text <- renderPlotly({
    tweets_ct %>%
      plot_ly(colors = c(wes_pal[1], wes_pal[2], wes_pal[3])) %>%
      add_trace(y = ~cumsum, x = ~order,
                type = "scatter", mode = "lines", name = "Cumulative Tweets",
                hovertemplate = ~paste('</br><b>Month:</b>', MonYr,
                                       '</br><b>Cumulative tweets:</b> ', cumsum),
                legendgroup = 'group1', line = list(color = wes_pal[2])) %>%
      add_trace(y = ~value, x = ~order,
                type = "scatter", mode = "lines", name = "Individual Tweets",
                hovertemplate = ~paste('</br><b>Month:</b>', MonYr,
                                       '</b></br><b>Tweets in month: </b>', value),
                legendgroup = 'group2', line = list(color = wes_pal[1])) %>%
      plotly::layout(title = "<b>Cumulative sum of disinformation tweets in Mexico, 2011-2021</b>",
             xaxis = list(title = "",
                          ticktext = list("Jan. 2011", "Jan. 2012", "Jan. 2013",
                                          "Jan. 2014", "Jan. 2015", "Jan. 2016", "Jan. 2017",
                                          "Jan. 2018", "Jan. 2019", "Jan. 2020", "Jan. 2021"),
                          tickvals = list(1, 13, 25, 37, 49, 61, 73, 85, 97, 109, 121),
                          tickangle=25, showgrid = FALSE),
             yaxis = list(title = "# of disinformation tweets"),
             legend = list(title = list(text = "<b>Legend</b>")),
             font = list(family = "times"),
             plot_bgcolor  = "rgba(0, 0, 0, 0)",
             paper_bgcolor = "transparent") %>%
      add_segments(x = 25, xend = 37, y = 708, yend = 708, line = list(dash = "dash", color = wes_pal[2]),
                   showlegend = FALSE, legendgroup = 'group1') %>%
      add_segments(x = 49, xend = 60, y = 1298, yend = 1298, line = list(dash = "dash", color = wes_pal[2]),
                   showlegend = FALSE, legendgroup = 'group1') %>%
      add_segments(x = 73, xend = 96, y = 1682, yend = 1682, line = list(dash = "dash", color = wes_pal[2]),
                   showlegend = FALSE, legendgroup = 'group1')
  })
  
############################ NETWORK
  
  networkYearInput_interactive <- reactive({
    switch(input$year_interactive,
           "2011" = toVisNetworkData(delete.vertices(hash_network_2011, degree(hash_network_2011)==0)), 
           "2012" = toVisNetworkData(delete.vertices(hash_network_2012, degree(hash_network_2012)==0)),
           "2014" = toVisNetworkData(delete.vertices(hash_network_2014, degree(hash_network_2014)==0)),
           "2016" = toVisNetworkData(delete.vertices(hash_network_2016, degree(hash_network_2016)==0)),
           "2019" = toVisNetworkData(delete.vertices(hash_network_2019, degree(hash_network_2019)==0)),
           "2020" = toVisNetworkData(delete.vertices(hash_network_2020, degree(hash_network_2020)==0)),
           "2021" = toVisNetworkData(delete.vertices(hash_network_2021, degree(hash_network_2021)==0)))
  })
   
  
  output$hashtag_network_interactive <- renderVisNetwork({
    visNetwork(networkYearInput_interactive()$nodes, networkYearInput_interactive()$edges, background = "#5BBCD6",
               main  = list(text = paste(input$year_interactive, " Mexican Twitter disinformation hashtags network"), style =  'font-family:Georgia, Times New Roman, Times, serif;font-weight:bold;font-size:20px;text-align:center;text-color:white'))%>% 
      visOptions(highlightNearest = T)%>%
      visEdges(physics = F, smooth = F, color = "#082e45", shadow = T, width = 2 ) %>% visNetwork::visIgraphLayout("layout.davidson.harel") %>% #layout.davidson.harel
      visNodes(labelHighlightBold = T, level = 0, size = input$nodes_size,  color = list(background ="#F2AD00", highlight = "#F98400") , shadow =T, borderWidthSelected = 2, 
               font = list(size = input$nodes_size,  color =  "white")) %>%
      visInteraction(hover = T, navigationButtons = TRUE) %>%  
      visEvents(blurNode  = "function(e){
          var label_info = this.body.data.nodes.get({
          fields: ['label', 'label_long'],
          filter: function (item) {
          return item.id === e.node
          },
          returnType :'Array'
          });
          this.body.data.nodes.update({id: e.node, label : label_info[0].label_long, label_long : label_info[0].label});
                }")
  })
  
  #observe({
  #  visNetworkProxy("network_proxy_nodes") %>%
   #   visNodes(size = input$nodes_size)
  #})
  
  
  networkYearInput_static <- reactive({
    switch(input$year_static,
           "2011" = hash_network_2011,
           "2012" = hash_network_2012,
           "2014" = hash_network_2014,
           "2016" = hash_network_2016,
           "2019" = hash_network_2019,
           "2020" = hash_network_2020,
           "2021" = hash_network_2021)
  })
    
  nodeSelect <- reactive({
    switch(input$nodes,
           "All Nodes" = networkYearInput_static(),
           "Connected Nodes Only" = delete.vertices((networkYearInput_static()), degree((networkYearInput_static()))==0))
  })
  
  output$hashtag_network_static <- renderPlot({
    rescale_node = function(x,a,b,c,d){c + (x-a)/(b-a)*(d-c)}
    plot(simplify(nodeSelect()), layout= layout.davidson.harel, vertex.size=rescale_node(degree(nodeSelect()), 0, 50, 5, 20), curved = F, #5
         asp = 0, main = paste(input$year_static, " Mexican Twitter disinformation hashtags network"),vertex.label.cex = 0.75, vertex.label.dist = 0.2, 
         vertex.label.color = ifelse((degree(nodeSelect()) >= 15), "#FF0000", "black" ))
  })
  
  output$network_expl <- renderUI({
    HTML(paste("<h2> Network Analysis </h2> <br>
               In this section, we explore the hashtags that were used together via a Mexican Twitter disinformation hashtag network. Twitter hashtags play an important role on the social media website; they allow users to create/find/contribute to communities around topics they are passionate about, or interested in. Hashtags on Twitter are crucial in expanding the audience for any tweet, and thus, directly contribute to disinformation as well. We explore these hashtag networks in two different ways. <br>
               <br>
               <h3> Interactive Networks: </h3>
               <p>These networks were created using the visNetwork library. The user is able to zoom in and out as they desire and move anywhere on the network using the tooltips at the bottom (or just by dragging the network with their cursor). The user can also explore which nodes/hashtags each node is connected to just by clicking on that specific node; this blurs out all alters unless they are directly connected to the ego hashtag. </p>
               <p>At the top, there is a slider which allows the user to visualize the hashtag networks for a few different years. The slider on the right allows the user to control the size of the node and the label.</p>
               <p>The data for this network does not include nodes that only had a connection to themselves. This was a conscious choice I made to make the visualization concise and comprehensible. I have included the option to view those nodes in the static network graphs.</p> <br>
               <h3> Static Networks: </h3>
               <p>The static networks were created using the igraph library in R. Users have the ability to view all nodes in the network, or only the ones that are connected to other nodes. I included the static networks in addition to the interactive ones to give the users an opportunity to explore even those nodes that were not connected to the other nodes. In the static hashtag network graph, the nodes are sized by their degree, i.e., by the number of other nodes they are connected to- bigger nodes have higher degree than smaller nodes. If a node is connected to several other nodes, it holds a central position in the hashtag network; it can be utilized to track misinformation and the topics it embodies, efficiently. The labels for nodes with more than 15 connections are in red to allow users to locate those labels easily, especially when looking at crowded networks.</p>
               <br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>", sep ="<br/>"))
  })
  
  ########################################### DATA SOURCE
  
  source <- a("Twitter Information Operations", href = "https://transparency.twitter.com/en/reports/information-operations.html")
  
  output$sources <- renderUI({
    tagList(HTML(paste("
               <p>The data for this R shiny dashboard was obtained directly from Twitter. Starting in 2018, Twitter started archiving tweets and related meta-data that were expected to be related to \"potential foreign information operations\" (misinformation) on Twitter. The social media platform made these accounts public and searchable so members of the public, governments, and researchers could investigate, learn, and build media literacy capacities for the future in order to tackle spread of misinformation. </p>
               The data includes information/tweets, prohibited by the Twitter Rules, regarding \"platform manipulation\" that are attributed to governtment/state-linked actors.", sep = "<br>")),
            HTML(paste("The data can be found here:")), source)
  })
  
    
}

######################################################## RUN APP ########################################################
shinyApp(ui = ui, server = server)
