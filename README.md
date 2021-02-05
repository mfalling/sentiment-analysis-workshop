# Sentiment Analysis Workshop

Files related to the Sentiment Analysis Workshop on Feb 5, 2020 for USF's Big Data Analytics Lab.

##  BasicSentimentAnalysis.R
This script explores the sentiment of a single tweet. It uses the syuzhet R Package to find sentiment according to the Bing, AFINN, and NRC lexicon dictionaries. It normalizes and tokenizes the text using the tidyverse syntax. It compares the dictionaries and introduces emotion from the NRC lexicon.

## AnalysisAndVisualization.R
This script uses the GameStop_hr.json file for sentiment analysis. It visualizes the following:
* Proportions & Frequencies
  * Tweet type (original, retweet, quotes, and quoted retweets)
  * Average Emotion
  * Positive/negative words
  * Emotion faceted by polarity
* Time-Series
* Geographic
  * Global
  * US
* Correlations 
  * Between emotions (correlation matrix)
  * Between emotions and variables (significance testing)

