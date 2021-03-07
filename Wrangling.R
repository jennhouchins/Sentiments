# ASSIGNMENT DESCRIPTION #####################################
# File:         Wrangling.R
# Project:      Sentiments
# Author:       Jennifer Houchins
#
# Purpose:      Independent example of Sentiment Analysis in R
#
# Guiding 
# Questions:    
#
# Description:  

# 1 PROJECT SETUP  ################################
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, dplyr, readr, tidyr, rtweet, writexl,
               readxl, tidytext, textdata, ggplot2, scales,
               ggcats, vader)
pacman::p_load_gh("trinker/lexicon")

# the following chunk deals with setting the working directory and creating
# a data folder if it doesn't already exist
# I just got tired of having to manually create the data folder every time
# I wanted to save a data file and this will ensure that the code will run 
# through with no problems/errors for others who may wish to pull it 
# from Github (e.g., the course instructor)
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path)) # Set working directory to source file location
datapath <- file.path(getwd(), "data")
if (!file.exists(datapath)){
  dir.create(datapath)
  print("The data sub directory has been created in the working directory.")
}

## check to see if the Twitter token is loaded
get_token()

# 2 WRANGLE  ################################

ac_dictionary <- c("#mentalhealth AND #AcademicChatter OR academicchatter",
                   "#mentalhealth AND #AcademicTwitter OR academictwitter",
                   "#mentalhealth AND #PhD OR #phdlife OR #phdchat",
                   '"mental health" AND #AcademicChatter',
                   '"mental health" AND #AcademicTwitter',
                   '"mental health" AND @AcademicChatter',
                   '"mental health" AND "phd students"',
                   "#AcademicMentalHealth OR academicmentalhealth")

# search up to 5000 tweets using the dictionary above but don't get retweets
ac_all_tweets <- search_tweets2(ac_dictionary,
                                n=5000,
                                include_rts = FALSE)

write_xlsx(ac_all_tweets, file.path(datapath,"academicmentalhealth_tweets.xlsx"))

jenn_tweets <- get_timelines("TooSweetGeek", n = 1000)


chatter_text <-
  ac_all_tweets %>%
  filter(lang == "en") %>%
  select(screen_name, created_at, text) 

# Tokenize text

tweet_tokens <- chatter_text %>%
  unnest_tokens(output = word, 
                input = text, 
                token = "tweets") %>% 
  anti_join(stop_words, by = "word")

nrc <- get_sentiments("nrc")
sentiment_nrc <- inner_join(tweet_tokens, nrc, by = "word")
sentiment_nrc

summary_nrc <- sentiment_nrc %>% 
  count(sentiment, sort = TRUE) %>% 
  spread(sentiment, n) %>%
  mutate(lexicon = "nrc") %>%
  relocate(lexicon)


summary_vader <- vader_df(chatter_text$text)

vader_results <- summary_vader %>% 
  mutate(sentiment = ifelse(compound > 0, "positive", ifelse(compound < 0, "negative", "neutral"))) %>% 
  count(sentiment, sort = TRUE) %>% 
  spread(sentiment, n)

