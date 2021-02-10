# NLP analysis based on J. R. R. Tolkien's Lord of the Rings Trilogy and The Hobbit
# data was collected from https://github.com/MokoSan/FSharpAdvent/tree/master/Data
# Load libraries
# specify the packages of interest
packages = c("jsonlite",
             "dplyr",
             "stringr",
             "tidyverse",
             "ggcharts",
             "tidytext",
             "naniar",
             "ggplot2",
             "tidyr")

# use this function to check if each package is on the local machine
# if a package is installed, it will be loaded
# if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

## JSON approach
# import json file of all three books and store as a dataframe
raw_books <- fromJSON("./Data/LordOfTheRingsBook.json")

# check for missing data
any(is_na(raw_books)) # returns FALSE

# take a look
str(raw_books) # BookName column is a dataframe

# convert dataframe column to char vector 
dfBooks <- do.call(data.frame, raw_books) %>% 
  rename(BookName = Case) %>%  # rename column
  mutate(ChapterNumber = row_number()) %>%  #  add a cumulative chapter number
  tibble()  # convert to tibble

# take another look
str(dfBooks)
head(dfBooks)

# tokenize (tolkienize???) and store in a tibble df
tidyBooks_stop <- dfBooks %>% 
  unnest_tokens(word, ChapterData)

# take a look
head(tidyBooks_stop)

# count the most used words
tidyBooks_stop %>% 
  count(word, sort = TRUE)

# need to remove stop words
data(stop_words)

# remove stop words and store in a new tibble
tidyBooks <- tidyBooks_stop %>%  
  anti_join(stop_words)

# count the most used words
mostUsed <- tidyBooks %>% 
  count(word, sort = TRUE)

# set save location for the graphs
pathGraphs = "./Graphs"

# column chart of the most used words using the ggchart package
tidyBooks %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 464) %>% 
  mutate(word = reorder(word, n)) %>% 
  bar_chart(word, n) +
    labs(x = "Word",
         y = "Frequency of Use",
         title = "Top 20 Most Used Words in The Lord of The Rings Trilogy") +
    geom_text(aes(label = n), 
                hjust = -.1, 
                vjust = +.4,
                color = "white") +
    theme_nightblue(grid = "XY",
                    axis = "x",
                    ticks = "x")
    # ggsave("Top20LOTRWords.png",
    #      plot = last_plot(),
    #      path = pathGraphs)


# calculate word frequency by book
WordFrequencyByBook <- tidyBooks %>% 
  group_by(BookName, word) %>%
  summarise(n = n())
  # mutate(proportion = n / sum(n)) # percentage of usage


# spread booknames from observations to columns
WordFrequencyByBook %>% 
  spread(BookName, n)



# subset into individual books
FellowshipWordFrequency <- WordFrequencyByBook %>% 
  filter(BookName == "TheFellowshipOfTheRing")

TowersWordFrequency <- WordFrequencyByBook %>% 
  filter(BookName == "TheTwoTowers")

KingWordFrequency <- WordFrequencyByBook %>% 
  filter(BookName == "TheReturnOfTheKing")


# column chart of the most used words using the ggchart package
# FellowshipWordFrequency_barchart <- 
  FellowshipWordFrequency %>% 
    filter(n > 197) %>%
    mutate(word = reorder(word, n)) %>%
    bar_chart(word, n) +
    labs(x = "Word",
         y = "Frequency of Use",
         title = "Top 20 Most Used Words in The Fellowship of the Ring") +
    geom_text(aes(label = n), 
              hjust = -.1, 
              vjust = +.4,
              color = "white") +
    theme_nightblue(grid = "XY",
                    axis = "x",
                    ticks = "x")
# ggsave("Top20FellowshipWords.png",
#      plot = last_plot(),
#      path = pathGraphs)


# column chart of the most used words using the ggchart package
# TowersWordFrequency_barchart <- 
  TowersWordFrequency %>% 
    filter(n > 168) %>%
    mutate(word = reorder(word, n)) %>%
    bar_chart(word, n) +
    labs(x = "Word",
         y = "Frequency of Use",
         title = "Top 20 Most Used Words in The Two Towers") +
    geom_text(aes(label = n), 
              hjust = -.1, 
              vjust = +.4,
              color = "white") +
    theme_nightblue(grid = "XY",
                    axis = "x",
                    ticks = "x")
# ggsave("Top20TowersWords.png",
#      plot = last_plot(),
#      path = pathGraphs)


# column chart of the most used words using the ggchart package
# KingWordFrequency_barchart <- 
  KingWordFrequency %>% 
    filter(n > 150) %>%
    mutate(word = reorder(word, n)) %>%
    bar_chart(word, n) +
    labs(x = "Word",
         y = "Frequency of Use",
         title = "Top 20 Most Used Words in The Return of the King") +
    geom_text(aes(label = n), 
              hjust = -.1, 
              vjust = +.4,
              color = "white") +
    theme_nightblue(grid = "XY",
                    axis = "x",
                    ticks = "x")
# ggsave("Top20KingWords.png",
#      plot = last_plot(),
#      path = pathGraphs)
  

# sentiment analysis by book
LOTR_sentiment <- tidyBooks %>% 
  mutate(linenumber = row_number()) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(BookName, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  select(-negative, -positive)
  # arrange(factor(BookName, levels = c("TheFellowshipOfTheRing",
  #                                     "TheTwoTowers",
  #                                     "TheReturnOfTheKing")))


# manual way of sorting the dataframe by book  
Fellowship <- LOTR_sentiment %>% 
  filter(BookName == "TheFellowshipOfTheRing")

Towers <- LOTR_sentiment %>% 
    filter(BookName == "TheTwoTowers")

King <- LOTR_sentiment %>% 
    filter(BookName == "TheReturnOfTheKing")

LOTR_sentiment <- rbind(Fellowship, Towers, King)


# confirm if arrange function worked as desired
LOTR_sentiment %>% 
  head()

LOTR_sentiment %>% 
  tail()


# adapted from tidytextmining.com--joy sentiment analysis
# license info for nrc lexicon dataset:
# Name: NRC Word-Emotion Association Lexicon 
# URL: http://saifmohammad.com/WebPages/lexicons.html 
# License: License required for commercial use. Please contact Saif M. Mohammad (saif.mohammad@nrc-cnrc.gc.ca). 
# Size: 22.8 MB (cleaned 424 KB) 
# Download mechanism: http 
# Citation info:
#   
#   This dataset was published in Saif M. Mohammad and Peter Turney. (2013), ``Crowdsourcing a Word-Emotion Association Lexicon.'' Computational Intelligence, 29(3): 436-465.
# 
# article{mohammad13,
#   author = {Mohammad, Saif M. and Turney, Peter D.},
#   title = {Crowdsourcing a Word-Emotion Association Lexicon},
#   journal = {Computational Intelligence},
#   volume = {29},
#   number = {3},
#   pages = {436-465},
#   doi = {10.1111/j.1467-8640.2012.00460.x},
#   url = {https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1467-8640.2012.00460.x},
#   eprint = {https://onlinelibrary.wiley.com/doi/pdf/10.1111/j.1467-8640.2012.00460.x},
#   year = {2013}
# 

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidyBooks %>%
  filter(BookName == "TheFellowshipOfTheRing") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

tidyBooks %>%
  filter(BookName == "TheTwoTowers") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

tidyBooks %>%
  filter(BookName == "TheReturnOfTheKing") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)


# column chart of the overall sentiment analysis of each book
# Fellowship
ggplot(Fellowship, aes(index, sentiment, fill = BookName)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Word",
       y = "Frequency of Use",
       title = "The Fellowship of the Ring") +
  theme_nightblue(grid = "XY",
                  axis = "x",
                  ticks = "x")
# ggsave("Fellowship_SentimentAnalysis.png",
#      plot = last_plot(),
#      path = pathGraphs)

# Towers
ggplot(Towers, aes(index, sentiment, fill = BookName)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Word",
       y = "Frequency of Use",
       title = "The Two Towers") +
  theme_nightblue(grid = "XY",
                  axis = "x",
                  ticks = "x")
# ggsave("Towers_SentimentAnalysis.png",
#        plot = last_plot(),
#        path = pathGraphs)

# King
ggplot(King, aes(index, sentiment, fill = BookName)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Word",
       y = "Frequency of Use",
       title = "The Return of the King") +
  theme_nightblue(grid = "XY",
                  axis = "x",
                  ticks = "x")
# ggsave("King_SentimentAnalysis.png",
#        plot = last_plot(),
#        path = pathGraphs)


# replace books to help with ordering
LOTR_sentiment_numbered <- LOTR_sentiment %>% 
  mutate(
    BookName = str_replace_all(
      BookName, c(
        "TheFellowshipOfTheRing" = "1_The Fellowship of the Ring"
        , "TheTwoTowers" = "2_The Two Towers"
        , "TheReturnOfTheKing" = "3_The Return of the King")
      )
    )

head(LOTR_sentiment_numbered)
tail(LOTR_sentiment_numbered)

# facet wrap of all books in the trilogy--ordered incorrectly
ggplot(LOTR_sentiment_numbered, aes(index, sentiment, fill = BookName)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~BookName, ncol = 3, scales = "free_x") +
  labs(title = "Sentiment Analysis of The Lord of the Rings") +
  theme_nightblue(grid = "XY",
                  axis = "x",
                  ticks = "x")
# ggsave("Trilogy_SentimentAnalysis.png",
#        plot = last_plot(),
#        path = pathGraphs)


afinn <- 
