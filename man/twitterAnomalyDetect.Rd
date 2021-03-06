% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/twitterTextMining.R
\name{twitterAnomalyDetect}
\alias{twitterAnomalyDetect}
\title{Anomaly Detection of Tweets on certain Topic}
\usage{
twitterAnomalyDetect(tweets, lastDay = Sys.Date(), title = "AUD/USD",
  alpha = 0.5, fontsize = 10)
}
\arguments{
\item{tweets}{a list of corpus and data.frame of tweets retreived from \code{tweet_corpus_chunck()} or \code{tweet_corpus{}}}

\item{lastDay}{last date of the analysis}

\item{title}{title to be added to the results plots}

\item{alpha}{opacity 0.5 default}

\item{fontsize}{font size}
}
\description{
When do people tweet? Anomaly Detection of Tweets on certain Topic.
}
\details{
Please use \code{RQuantAPI} for twitter data retrieval
}
\examples{
setupTwitterConn()
tweets <- tweet_corpus_chunck(search = "aud+usd", n = 50000, chunck_size = 7, total_size = 90, until = Sys.Date())
saveRDS(tweets, '~/analytics/Workspace/Tweets.RDS')
tweets = readRDS('~/analytics/Workspace/Tweets.RDS')
anomalies <- twitterAnomalyDetect(tweets, lastDay = Sys.Date(), title = "AUD/USD")
anomalies$tweetsDistribution
anomalies$anomalies

}
