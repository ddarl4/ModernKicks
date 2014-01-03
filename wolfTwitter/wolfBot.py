'''
Class to retrieve, manage, and post twitter content 

This tool specializes in Craigslist searches and re-tweeting 
content containing a specific phrase

'''

import twitter, os, time, sys
import mechanize
import cookielib
import pandas as pd
from BeautifulSoup import BeautifulSoup
import craigslist_scraper.cl_scraper as csc

class howl_tweet(object):
    '''
    Class to retrieve, manage, and post twitter content 

    Parameters
    ----------
    cred_path : str
        path to twitter credential file
    '''
    def __init__(self, cred_path):
        '''
        Initialize a new instance twitter managment
        
        '''
        # set output directories
        self.cred_path = cred_path
        self.api_connect()

    def api_connect(self):
        '''
        connect to the twitter api Bear's twitter module
        '''
        keyDict = {}
        with open(self.cred_path, 'r') as f:
            for line in f:
                line = line.replace('\n','')
                splt = line.split(' ')
                keyDict[splt[0]] = splt[1]
        api = twitter.Api(consumer_key=keyDict['consumer_key'],
            consumer_secret=keyDict['consumer_secret'],
            access_token_key=keyDict['access_token_key'],
            access_token_secret=keyDict['access_token_secret'])
        self.api = api

    def scrape_craigslist_links(self):
        '''
        Use the craigslist_scraper module to search CL and get urls 
        '''
        cities = ['boston','nyc','sf','philadelphia','portland','fortcollins']
        listingFrame = pd.DataFrame()
        for city in cities:
            cs = csc.cl_scraper(location=city,query='"dog not included"')
            cs.run_cl_query()
            cs.extract_listings(title_strict=False)
            listingFrame = pd.concat([listingFrame,cs.craigslist_results],axis=0)
        self.listingFrame = listingFrame


    def post_craigslist_links(self,n_posts=5):
        '''
        post links from craigslist search 
        '''
        for link in self.listingFrame['url'][:n_posts]:
            post = 'dog not included ' + link
            print post 
            status = self.api.PostUpdate(post)
            time.sleep(2)
    
    def retweet_search_results(self,search_phrase='dog not included',n_posts=5):
        '''
        post links from craigslist search 
        '''
        searchRes = self.api.GetSearch(term=search_phrase,count=n_posts)
        for res in searchRes:
            print res.text
            status = self.api.PostRetweet(res.id)
            time.sleep(10)
