# Library -----------------------------------------------------------------
library(syuzhet)        # For Sentiment Analysis
library(dplyr)          # For data manipulation
library(textclean)      # For removing contractions
library(stringr)        # For removing substrings
library(tidytext)       # For unnesting tokens

# An Example --------------------------------------------------------------

text <- "Quite a while ago I bought 70 stocks in Gamestop for like $5 each as I was first getting into stocks.  Today I sold them for $328 each. $22,000+ !! Won't have to worry about insulin for a long while. ...and billionaires want to make this illegal..."
example <- as.data.frame(text, stringsAsFactors = FALSE)

# Three different lexicon dictinaries. 
# Each uses a different scale to understand sentiment.
get_sentiment(text, "bing")
get_sentiment(text, "afinn")
get_sentiment(text, "nrc")

# Why does this tweet have a negative sentiment?

# Normalization -----------------------------------------------------------

# Make lowercase and replace contractions, punctuation, and numbers.
clean <- example %>%
  mutate(text = tolower(text)) %>%
  mutate(text = replace_contraction(text)) %>%
  mutate(text = str_remove_all(text, "[[:digit:]]")) %>%
  mutate(text = str_remove_all(text, "[[:punct:]]"))

clean

# Some of this normalization is done by unnest_tokens and isn't necessary.
# Explore the function by using View(unnest_tokens) to understand what it does.

# How many words (tokens) in this tweet?
length <- length(unlist(strsplit(clean$text, "\\W+")))
length

# What percentage of the tweet is positive or negative?
(get_sentiment(clean$text, "bing")/length)*100
(get_sentiment(clean$text, "afinn")/length)*100
(get_sentiment(clean$text, "nrc")/length)*100

# Tokenization ------------------------------------------------------------

# Tokenization and Frequency Count
tokens <- clean %>% 
  unnest_tokens("word", "text") %>%
  group_by(word) %>%
  tally(name = "freq") %>%
  arrange(desc(freq))

head(tokens)

# More Normalization ------------------------------------------------------

# Removing stopwords from tokens can be easier than removing them from strings.
tokens <- tokens %>%
  anti_join(get_stopwords(), by = "word")

head(tokens)

# Explore Sentiment Dictionaries ------------------------------------------

# How are these dictionaries different from each other?

# Join tokens with dictionary
tokens %>%
  select(word) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment) 

# Join tokens with dictionary
tokens %>%
  select(word) %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, value) 

# Join tokens with dictionary
tokens %>%
  select(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(sentiment == "positive" | sentiment == "negative") %>%
  count(word, sentiment) 


# Emotion -----------------------------------------------------------------

# When not filtering NRC...
get_nrc_sentiment(text)
