# scratch code to understand bear/python-twitter module

import twitter, os, time, sys
from random import choice
from config import Config
import mechanize
import cookielib
from BeautifulSoup import BeautifulSoup

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
searchRes = api.GetSearch(term='dog not included',count=4)
for res in searchRes:
    print res.text
oneStatus = searchRes[0]
print oneStatus.text

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
url = 'http://sfbay.craigslist.org/'
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

# make dog not included post:
for ilink,link in enumerate(linkList[:5]):
    post = 'dog not included - post ' + str(ilink+1) + ' ' + link
    print post 
    status = api.PostUpdate(post)
    time.sleep(10)

### next search twttr for the phrase 'dog not included' or 'dogs not included'
# also '#dognotincluded'
# make bank of re-tweets 'absolutly breath taking puppy you have there'
# 'that dog is stunning'

### add ebay search

## re-tweet 5 most recent dog not included posts
searchRes = api.GetSearch(term='dog not included',count=5)
for res in searchRes:
    print res.text
    status = api.PostRetweet(res.id)
    time.sleep(10)



