library(sentimentAnalysis)

setupTwitterConn()
tweets <- tweet_corpus_chunck(search = "aud+usd", n = 100, chunck_size = 7, total_size = 7, until = Sys.Date())
saveRDS(tweets, '../Tweets.RDS')
tweets = readRDS('../Tweets.RDS')
anomalies <- twitterAnomalyDetect(tweets, lastDay = Sys.Date(), title = "AUD/USD")
anomalies$tweetsDistribution
anomalies$anomalies
