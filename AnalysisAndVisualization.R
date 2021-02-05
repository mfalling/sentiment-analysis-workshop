# Library -----------------------------------------------------------------
library(rtweet)         # For parsing the JSON
library(dplyr)          # For data manipulation
library(textclean)      # For removing contractions
library(stringr)        # For manipulating strings
library(reshape2)       # For melting data; tidyr's pivot_longer() works too.
library(syuzhet)        # For Sentiment Analysis
library(ggplot2)        # For plotting graphs
library(tidytext)       # For unnesting tokens
library(ggmap)          # For geospatial mapping
library(corrplot)       # For correlations
library(RColorBrewer)   # For colors

# Load Data ---------------------------------------------------------------

# Convert the JSON into a parsed data frame.
parsed <- parse_stream("data/GameStop_hr.json")

# Filter to english tweets and keep a subset of columns.
# Note: In other analyses, I might want status ID, user ID, etc.
gamestop <- parsed %>%
  filter(lang == "en") %>%
  lat_lng() %>%
  mutate(time = format(created_at, format = "%H:%M")) %>%
  select(time, text, is_retweet, is_quote, followers_count, lat, lng)

# First Look --------------------------------------------------------------

head(gamestop)

# What are people actually saying?
gamestop %>%
  pull(text) %>%
  head()

# What is the composition of tweets?
composition <- gamestop %>%
  summarise(`Retweets` = sum(is_retweet & !is_quote),
            `Retweeted Quotes` = sum(is_retweet & is_quote),
            `Quote` = sum(!is_retweet & is_quote),
            `Original Tweet` = sum(!is_retweet & !is_quote))

composition

# Reshape the data; use all variables as measure variables.
meltedComp <- melt(composition)
meltedComp

# Plot
ggplot(meltedComp, 
       aes(x = reorder(variable, -value),
           y = value, 
           fill = reorder(variable, -value))) +
  geom_col() +
  labs(title = "Composition of Tweets in the Dataset",
       fill = "Type of Tweet",
       x = "Type of Tweet",
       y = "Number of Tweets")

# Assumption: Retweets echo the original opinion.
# If I had demographic data, this would be a great time to split the dataset.
# I might also be interested in *which* tweets are retweets.

# I'm not interested in retweet/quote status anymore.
gamestop <- gamestop %>%
  select(-is_retweet, -is_quote)

head(gamestop)

# Explore the Text --------------------------------------------------------

# Normalize and Tokenize

# Tokenize the text
tokens <- gamestop %>% 
  select(time, text) %>%
  mutate(text = str_replace_all(text, "http(s?)://(.*)[.][a-z]+", "")) %>%
  mutate(text = replace_contraction(text)) %>%
  unnest_tokens("word", "text") %>%
  anti_join(get_stopwords())

tokens

# Fourth most popular token: pic.twitter.com/OAmkrqhZHm
# Many of these words won't be in our dictionaries (gamestop, robinhood, etc).

nrc_sent <- tokens %>%
  select(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment) %>%
  arrange(desc(n))

nrc_sent

nrc_polarity <- nrc_sent %>%
  filter(sentiment == "negative" | sentiment == "positive") %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(word = reorder(word, n))

# Plot NRC polarity
ggplot(nrc_polarity, aes(x = word, y = n, fill = sentiment)) +
  geom_col() +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiments in One Hour of Gamestop Tweets",
       x = NULL,
       y = "Number of Words") +
  coord_flip()


# Time-Series Sentiment Analysis ------------------------------------------

# Inspiration: 
# rforjournalists.com/2019/12/23/how-to-perform-sentiment-analysis-on-tweets/

# We don't need to remove stopwords for the Syuzhet dictionaries.
# Filter the words to the afinn sentiment dictionary and get frequency by time
dict <- get_sentiments("afinn")
afinn_sent <- tokens %>%
  inner_join(dict, by = "word") %>%
  group_by(time) %>%
  summarise(sentiment = mean(value))

afinn_sent

#plot
ggplot(afinn_sent, aes(x = time, y = sentiment)) + 
  geom_line(group = 1) + 
  geom_point() +
  labs(title = "Twitter Gamestop Sentiment over an hour") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text(size = 15))

# Do the high and low points make sense? Is this the best dictionary?
yourTimePoint <- "20:16"
gamestop %>%
  filter(time == yourTimePoint) %>%
  pull(text) %>%
  head()

# Geographical Sentiment Analysis -----------------------------------------

# Filter the dataset to geocoordinate-enabled tweets
geoSent <- gamestop %>%
  filter(!is.na(lat) & !is.na(lng)) %>%
  select(-time) %>%
  mutate(afinn = get_sentiment(text, "afinn"))

# What's the most positive and negative tweet?
geoSent %>%
  filter(afinn == min(afinn) | afinn == max(afinn)) %>%
  pull(text)

# Plot.
ggplot() + 
  geom_polygon(data = map_data("world"), 
               aes(x = long, y = lat, group = group),
               color="black", fill="white") +
  coord_fixed(1.3) +
  geom_point(data = geoSent, 
             aes(x=lng, y=lat, 
                 color = as.factor(afinn),
                 size = followers_count)) +
  scale_color_brewer(palette = "RdYlGn") +
  labs(title = "Small Sample of Gamestop Sentiments on Twitter",
       color = "Sentiment Score") +
  theme_void()


# Eastern most part of USA: West Quoddy Head, Maine
geoSentUSA <- geoSent %>%
  filter(lng < -68.669037)

ggplot() + 
  geom_polygon(data = map_data("state"), 
               aes(x = long, y = lat, group = group),
               color="black", fill="white") +
  coord_fixed(1.3) +
  geom_point(data = geoSentUSA, 
             aes(x=lng, y=lat, 
                 color = as.factor(afinn),
                 size = followers_count)) +
  scale_color_brewer(palette = "RdYlGn") +
  labs(title = "Small Sample of Gamestop Sentiments on Twitter",
       color = "Sentiment Score") +
  theme_void()


# Looking at Emotion ------------------------------------------------------

# Get the sentiment scores
sentimentScores <- gamestop %>%
  pull(text) %>%
  get_nrc_sentiment(.)

head(sentimentScores)

# High Positive.
gamestop[847, ] %>%
  pull(text)

# High Negative.
gamestop[5310, ] %>%
  pull(text)

# High Fear.
gamestop[3702, ] %>%
  pull(text)

# High Trust.
gamestop[2820, ] %>%
  pull(text)

# Emotion Proportions -----------------------------------------------------

emotionProportion <- sentimentScores %>%
  select(!!1:8) %>%
  prop.table() %>%
  colSums() %>%
  data.frame("proportion" = .) %>%
  tibble::rownames_to_column("sentiment") 

# Color-Blind Friendly Palette
cbPalette <- c("#CC79A7", "#E69F00", "#D55E00", "#009E73",
               "#bab34b", "#0072B2", "#56B4E9", "#999999")

ggplot(emotionProportion, aes(x = reorder(sentiment, -proportion),
                              y = proportion,
                              fill =  reorder(sentiment, -proportion))) +
  geom_col() +
  labs(title = "Emotion in GameStop Twitter Conversation (1 hour)",
       x = "Sentiment",
       y = "Proportion of Dataset",
       fill = "Emotion") +
  scale_fill_manual(values = cbPalette) +
  scale_y_continuous(labels = function(x){paste0(x*100, "%")}) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Is this just representative of the dictionary?
nrc_dict <- get_sentiment_dictionary("nrc")
nrc_dict %>%
  group_by(sentiment) %>%
  tally() %>%
  filter(sentiment != "negative" & sentiment != "positive") %>%
  arrange(desc(n))

# If our bar graph just mimicked the NRC dictionary, we would expect
# Fear and Anger to the highest scoring emotions.

# Sentiment Grid ----------------------------------------------------------

# How would you clean this up?

# Capture emotion and polarities
emotion <- nrc_sent %>%
  filter(!sentiment %in% c("positive", "negative"))
emotion

polarity <- nrc_sent %>%
  filter(sentiment %in% c("positive", "negative"))

noPolarity <- anti_join(emotion, polarity, by = "word") %>%
  mutate(sentiment = "neither") %>%
  group_by(word) %>%
  distinct() %>%
  as.data.frame()
polarity <- rbind(polarity, noPolarity)
polarity

# Rename sentiment column
colnames(emotion)[2] <- "emotion"
colnames(polarity)[2] <- "polarity"

emotion
polarity

# Update wordFreq_nrc to contain emotion and polarity labels.
wordFreq <- inner_join(emotion, polarity,
                       by = c("word", "n"))
wordFreq

topwords <- wordFreq %>%
  group_by(polarity) %>%
  top_n(n = 3, wt = n) %>%
  arrange(desc(n))
topwords

ggplot(wordFreq, aes(x = reorder(emotion, -n),
                     y = n,
                     col = reorder(emotion, -n),
                     fill = reorder(emotion, -n))) +
  geom_point() +
  facet_grid(~ polarity) +
  labs(title = "Emotional Words in Gamestop-Related Tweets",
       y = "Frequency of Words",
       x = "Emotions (facted by polarity)",
       fill = "Emotion",
       col = "Emotion") +
  scale_colour_manual(values = cbPalette) +
  scale_fill_manual(values = cbPalette) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggrepel::geom_text_repel(data = topwords, aes(label = word), size = 6)

# Manual Investigation ----------------------------------------------------

gamestop %>%
  filter(grepl("taught", text)) %>%
  distinct(text) %>%
  pull()

# Correlations ------------------------------------------------------------

M <-cor(sentimentScores[, 1:8])
corrplot(M, 
         type = "upper", 
         order = "hclust",
         col = brewer.pal(n=8, name = "RdYlBu"))

# Significant relationship between tweet length and sentiment scores.
sentimentScores$tweetLength <- length(unlist(strsplit(gamestop$text, "\\W+")))
regPolarity <- lm(formula = tweetLength ~ positive + negative, data = sentimentScores)
regEmotion <- lm(data = sentimentScores, formula = tweetLength ~ anger + anticipation +
                   fear + joy + sadness + surprise + trust)
summary(regPolarity)
summary(regEmotion)

# No significant relationship between followers count and sentiment scores.
sentimentScores$followers_count <- gamestop$followers_count
regPolarity2 <- lm(formula = followers_count ~ positive + negative, data = sentimentScores)
regEmotion2 <- lm(data = sentimentScores, formula = followers_count ~ anger + anticipation +
                    fear + joy + sadness + surprise + trust)
summary(regPolarity2)
summary(regEmotion2)
