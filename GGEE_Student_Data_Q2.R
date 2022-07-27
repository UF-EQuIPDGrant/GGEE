library(tidyverse)
library(tidytext)
library(dplyr)
library(readr)
library(tidyr)
library(writexl)
library(readxl)
library(textdata)
library(ggplot2)
library(scales)

Q2Sres <- read_excel("Data/Student Interviews_Dulany.xlsx", sheet=2)
Q2SresSel<- select(Q2Sres, Student_Code, quote)

remove_words <-data.frame("word"= c("challenge", "challenging", "lot", "um", "it's","it", "uh","uhh", "ive", "that's"))

Q2Srestidy<-unnest_tokens(Q2SresSel, word, quote)

Q2Sres_remove<- anti_join(Q2Srestidy,remove_words) #remove repeat words

Q2Sresclean<-anti_join(Q2Sres_remove, stop_words)

Q2Sres_counts <- count(Q2Sresclean, word, sort = TRUE)

library(wordcloud2)
wordcloud2(Q2Sres_counts)

afinn <- get_sentiments("afinn")
bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")
loughran <- get_sentiments("loughran")

sentiment_afinn <- inner_join(Q2Sresclean, afinn, by = "word")
sentiment_bing <- inner_join(Q2Sresclean, bing, by = "word")
sentiment_nrc <- inner_join(Q2Sresclean, nrc, by = "word")
sentiment_loughran <- inner_join(Q2Sresclean, loughran, by = "word")

summary_bing <- count(sentiment_bing, sentiment, sort = TRUE)
summary_afinn <- count(sentiment_afinn, value, sort = TRUE)
summary_nrc <- count(sentiment_nrc, sentiment, sort = TRUE)
summary_loughran <- count(sentiment_loughran, sentiment, sort = TRUE)

summary_bing2 <- sentiment_bing %>% 
  group_by(Student_Code) %>% 
  count(sentiment, sort = TRUE) %>% 
  spread(sentiment, n) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(lexicon = "bing") %>%
  relocate(lexicon)

summary_afinn %>%
  ggplot(aes(x="", y=n, fill=value)) +
  geom_bar(width = .6, stat = "identity") +
  labs(title = "Sentiment Values of Teacher Interviews",
       subtitle = "") +
  coord_polar(theta = "y") +
  theme_void()

summary_afinn2 <- sentiment_afinn %>% 
  group_by(Student_Code) %>% 
  filter(value != 0) %>%
  mutate(sentiment = if_else(value < 0, "negative", "positive")) %>% 
  count(sentiment, sort = TRUE) %>% 
  mutate(method = "AFINN")

summary_bing2 <- sentiment_bing %>% 
  group_by(Student_Code) %>% 
  count(sentiment, sort = TRUE) %>% 
  mutate(method = "bing")

summary_nrc2 <- sentiment_nrc %>% 
  group_by(Student_Code) %>% 
  count(sentiment, sort = TRUE) %>% 
  mutate(method = "nrc") 

summary_loughran2 <- sentiment_loughran %>% 
  group_by(Student_Code) %>% 
  count(sentiment, sort = TRUE) %>% 
  mutate(method = "loughran") 

summary_sentiment <- bind_rows(summary_afinn2,
                               summary_bing2,
                               summary_nrc2,
                               summary_loughran2) %>%
  arrange(method, Student_Code) %>%
  relocate(method)

#summary_sentiment

total_counts <- summary_sentiment %>%
  group_by(Student_Code) %>%
  summarise(total = sum(n))

sentiment_counts <- left_join(summary_sentiment, total_counts)

#sentiment_counts

sentiment_percents <- sentiment_counts %>%
  mutate(percent = n/total * 100)

#sentiment_percents

sentiment_percents %>%
  ggplot(aes(x = Student_Code, y = percent, fill=sentiment)) +
  geom_bar(width = .8, stat = "identity") +
  facet_wrap(~method, ncol = 1) +
  coord_flip() +
  labs(title = "GGEE: Sentiment of Student Responses", 
       subtitle = "What has been challenging for you during the camp?",
       x = "Student Codes", 
       y = "Percentage of Words")

summary_nrc2 %>%
  ggplot(aes(x = n, y = sentiment, fill=sentiment)) +
  geom_bar(width = .8, stat = "identity") +
  facet_wrap(~method, ncol = 1) +
  labs(title = "GGEE: Sentiment of Student Responses", 
       subtitle = "What has been challenging for you during the camp?",
       x = "Number of Words", 
       y = "Sentiment")


summary_bing2 %>%
  ggplot(aes(x = n, y = sentiment, fill=sentiment)) +
  geom_bar(width = .8, stat = "identity") +
  facet_wrap(~method, ncol = 1) +
  labs(title = "GGEE: Sentiment of Student Responses", 
       subtitle = "What has been challenging for you during the camp?",
       x = "Number of Words", 
       y = "Sentiment")

summary_afinn %>%
  ggplot(aes(x = value, y = n, fill=value)) +
  geom_bar(width = .8, stat = "identity") +
  labs(title = "GGEE: Sentiment of Student Responses", 
       subtitle = "What has been challenging for you during the camp?",
       x = "Sentiment Value", 
       y = "Number of Words")
