# Sentiment Analysis Workshop

Files related to the Sentiment Analysis Workshop on Feb 5, 2020 for USF's Big Data Analytics Lab.

This contains two R Scripts:
* BasicSentimentAnalysis.R
* AnalysisAndVisualization.R

##  BasicSentimentAnalysis.R
This script explores the sentiment of a single tweet. It uses the syuzhet R Package to find sentiment according to the Bing, AFINN, and NRC lexicon dictionaries. It normalizes and tokenizes the text using the tidyverse syntax. It compares the dictionaries and introduces emotion from the NRC lexicon.

## AnalysisAndVisualization.R
This script uses the GameStop_hr.json file for sentiment analysis. It visualizes the following:
* Proportion the tweet type (original, retweet, quotes, and quoted retweets)
* Proportion of emotion in the dataset
* Frequency of positive/negative words
* Time-Series Data: How sentiment changes over a 1hr period
* Geographic Data: Where sentiment is happening, with a focus on influential people (followers_count)
* Breakdown of emotions in each polarity (positive/negative)
* Correlations between emotions
* Correlations between emotions and variables
