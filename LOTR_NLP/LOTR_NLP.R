# NLP analysis based on J. R. R. Tolkien's Lord of the Rings Trilogy and The Hobbit
# data was collected from https://github.com/MokoSan/FSharpAdvent/tree/master/Data
# Load libraries
# specify the packages of interest
packages = c("jsonlite",
             "dplyr",
             "stringr",
             "tidyverse")

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
head(tidyBooks)

# count the most used words
tidyBooks_stop %>% 
  count(word, sort = TRUE)

# need to remove stop words
data(stop_words)

# remove stop words and store in a new tibble
tidyBooks <- tidyBooks_stop %>%  
  anti_join(stop_words)

# count the most used words
tidyBooks %>% 
  count(word, sort = TRUE)

# column chart of the most used words
tidyBooks %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 400) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) +
    geom_col() +
    xlab("Frequency of Use") +
    coord_flip()








