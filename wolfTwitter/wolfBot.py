'''
Class to retrieve, manage, and post twitter content 

This tool specializes in Craigslist searches and re-tweeting 
content containing a specific phrase

'''

import twitter, os, time, sys
from random import choice
from config import Config
import mechanize
import cookielib
from BeautifulSoup import BeautifulSoup

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
    
    def get_craigslist_links(self):
        '''
        search items for sale on craigslist containing
        a specific phrase 
        '''
        # Set paramaters for Browser object 
        br = mechanize.Browser()
        # Cookie Jar
        cj = cookielib.LWPCookieJar()
        br.set_cookiejar(cj)
        # Browser options
        br.set_handle_equiv(True)
        br.set_handle_gzip(True)
        br.set_handle_redirect(True)
        br.set_handle_referer(True)
        br.set_handle_robots(False)
        # Follows refresh 0 but not hangs on refresh > 0
        br.set_handle_refresh(mechanize._http.HTTPRefreshProcessor(), max_time=1)
        #open craigslist 
        url = 'http://dallas.craigslist.org/'
        br.open(url)
        br.select_form(nr=0)
        br.form['query'] = '"dog not included"' #see form.controls
        br.submit()
        html = br.response().read()
        soup = BeautifulSoup(html)
        titleTag = soup.html.head.title
        #make a list of the CL findings
        body1 = soup.html.body.contents[3]
        body2 = body1.contents[5]
        links2 = body2.findAll('a')
        linkList = []
        for link in links2:
            if len(link.attrs) > 1:
                tup1 = link.attrs[1]
                if (tup1[0] == 'class') and (tup1[1] == 'i'):
                    tup2 = link.attrs[0]
                    resLink = tup2[1]
                    if resLink[:4] == 'http':
                        linkList.append(resLink)
                    else:
                        linkList.append(url + resLink)
        self.linkList = linkList

    def post_craigslist_links(self,n_posts=5):
        '''
        post links from craigslist search 
        '''
        for ilink,link in enumerate(self.linkList[:n_posts]):
            post = 'dog not included - post ' + str(ilink+1) + ' ' + link
            print post 
            status = self.api.PostUpdate(post)
            time.sleep(10)
    
    def retweet_search_results(self,search_phrase='dog not included',n_posts=5):
        '''
        post links from craigslist search 
        '''
        searchRes = self.api.GetSearch(term=search_phrase,count=n_posts)
        for res in searchRes:
            print res.text
            status = self.api.PostRetweet(res.id)
            time.sleep(10)
