#install.packages("twitteR")
#install.packages("tidyverse")
#install.packages("reshape")

#graph package
#install.packages("ggplot2")

# stringr and dplyr
library(reshape)
library(dplyr)
library(stringr)
library(lubridate)
library(twitteR)
library(ggplot2) 

bad_words = c("bad", "attack", "terrorist", "military", "disaster", "die", "problem", "failing", 
              "fake", "nuclear", "deaths", "dramatic", "hostile", "deadly", "corruption")

good_words = c("good","happy", "thanks", "victory", "great", "honor", "win", "love", "fantastic", 
               "proud", "amazing", "excitement")

# Keys and tokens
consumer_key <- "eOe2JPCErba99kpiUfobwkzhm"
consumer_secret <- "qMg7pFLBdm8Sw88udVdXi2bsAmwk6coYK0nGoVRkoPHTeAc8Mn"
access_token <- "1043294216426803201-dTUsI6F9EloRffgFBMsnfYRwMrxuWw"
access_secret <- "RZCky5q4IkcNYoCvfq4wvcuJZHBmivRGmXDeddmFzNne1"

# oauth twitter
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# donald_trump user
donald_trump = twitteR::getUser('realDonaldTrump')
user_tweet = userTimeline(donald_trump, n=4000, maxID=NULL, sinceID=NULL, includeRts=TRUE,
             excludeReplies=TRUE)


# transform object user_tweet into dataframe
tweets_df = twListToDF(user_tweet)

# Dataframe with column:
# - date
# - count (tweet with bad words)
bad_mood_df = tweets_df %>% 
  select(text, created) %>% 
  filter(str_detect(text, paste(bad_words, collapse = "|"))) %>%
  mutate(date = as.Date(created)) %>%
  group_by(date = floor_date(as.Date(created), "10 days")) %>%
  summarise(count_bad = n())


# Dataframe with column:
# - date
# - count (tweet with good words)
good_mood_df = tweets_df %>% 
  select(text, created) %>% 
  filter(str_detect(text, paste(good_words, collapse = "|"))) %>%
  mutate(date = as.Date(created)) %>%
  group_by(date = floor_date(as.Date(created), "10 days")) %>%
  summarise(count_good = n())

# get max and min date of all Donald Trump's tweets
min_date = min(tweets_df[, "created"])
max_date = max(tweets_df[, "created"])

# Create new DataFrame with column:
# - date
# - count (tweet with good word)
# - count (tweet with bad word)
all_mood_df = data.frame(date = seq(as.Date(min_date),as.Date(max_date), 1)) %>% 
  right_join(bad_mood_df, by = 'date') %>% 
  right_join(good_mood_df, by = 'date')

# Remove all NA row
all_mood_df[is.na(all_mood_df)] = 0


all_mood_df = melt(all_mood_df, id=c("date"))

ggplot(all_mood_df, aes(date, y = value, colour = variable)) + 
  geom_step(aes(y = value)) +
  labs(title = "Mood Donald", y = "count tweets")

ggplot(all_mood_df, aes(date, y = value, colour = variable)) + 
  geom_point(aes(y = value)) +
  labs(title = "Mood Donald", y = "count tweets")

ggplot(all_mood_df, aes(date, y = value, colour = variable)) + 
  geom_line(aes(y = value)) +
  labs(title = "Mood Donald", y = "count tweets")

ggplot(all_mood_df, aes(date, weight = displ, fill = variable)) + 
  geom_bar(aes(weight = value)) +
  labs(title = "Mood Donald", y = "count tweets")


# improvements:
# - more words
# - line points
# - case sensitive
# - create method

