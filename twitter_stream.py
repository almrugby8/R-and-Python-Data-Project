# -*- coding: utf-8 -*-
"""
Created on Wed Apr 20 15:41:15 2016

@author: CarolineL
"""

#Import the necessary methods from tweepy library
from tweepy.streaming import StreamListener
from tweepy import OAuthHandler
from tweepy import Stream

#Variables that contains the user credentials to access Twitter API 
access_token = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
access_token_secret = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
consumer_key = "XXXXXXXXXXXXXXXXXXXXXX"
consumer_secret = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"


#This is a basic listener that just prints received tweets to stdout.
class StdOutListener(StreamListener):

    def on_data(self, data):
        #print data
        with open('users/caroline/twitproj/fetched_tweets.txt','a') as tf:
            tf.write(data)
        return True

    def on_error(self, status):
        print status
        
if __name__ == '__main__':

    #This handles Twitter authetification and the connection to Twitter Streaming API
    l = StdOutListener()
    auth = OAuthHandler(consumer_key, consumer_secret)
    auth.set_access_token(access_token, access_token_secret)
    stream = Stream(auth, l)

    #This line filter Twitter Streams to capture data by the keywords: 'zika'
    stream.filter(track=['zika'])

import json
import pandas as pd

tweets_data_path = 'fetched_tweets.txt'

#reading data into an array
tweets_data = []
tweets_file = open(tweets_data_path, "r")
for line in tweets_file:
    try:
        tweet = json.loads(line)
        tweets_data.append(tweet)
    except:
        continue

#converting into a dataframe
tweets = pd.DataFrame()

#selecting variables of interest out
tweets['creation time'] = map(lambda tweet: tweet.get('created_at', None), tweets_data)
tweets['timestamp'] = map(lambda tweet: tweet.get('timestamp_ms', None), tweets_data)
tweets['user id'] = map(lambda tweet: tweet.get('user',{}).get('id') if tweet.get('user', None) != None else None, tweets_data)
tweets['text'] = map(lambda tweet: tweet.get('text', None), tweets_data)
tweets['lang'] = map(lambda tweet: tweet.get('lang', None), tweets_data)
tweets['country'] = map(lambda tweet: tweet.get('place',{}).get('country') if tweet.get('place', None) != None else None, tweets_data)
tweets['city'] = map(lambda tweet: tweet.get('place',{}).get('name') if tweet.get('place', None) != None else None, tweets_data)


#writing to a csv using a codec which can read the data correctly
tweets.to_csv(path_or_buf="/Users/AaronMauner/Google Drive/UCD Spring '16/R and Python/Data Project/tweetsparsed.csv",na_rep="NA",encoding='utf-8')
