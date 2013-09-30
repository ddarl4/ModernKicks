# scratch code to understand bear/python-twitter module

import twitter, os, time, sys
from random import choice
from config import Config

# config file
CONFIG = '/Users/hogstrom/Documents/code/configs/bot2.cfg'
keyDict = {}
with open(CONFIG, 'r') as f:
    for line in f:
        line = line.replace('\n','')
        splt = line.split(' ')
        keyDict[splt[0]] = splt[1]

api = twitter.Api(consumer_key=keyDict['consumer_key'],
    consumer_secret=keyDict['consumer_secret'],
    access_token_key=keyDict['access_token_key'],
    access_token_secret=keyDict['access_token_secret'])

status = api.PostUpdate('python-twitter update from the wolf kitchen')
print status.text

# print home timeline
tmLine = api.GetHomeTimeline()
print [x for x in tmLine]

#search for phrase 
searchRes = api.GetSearch(term='gene expression',count=3)
oneStatus = searchRes[0]
print oneStatus.text

