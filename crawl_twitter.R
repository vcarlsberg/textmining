library(twitteR)
library(tm)

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
tweet_bude = userTimeline("@budesumiyati")

