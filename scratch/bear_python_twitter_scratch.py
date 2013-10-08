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
searchRes = api.GetSearch(term='dog not included',count=20)
for res in searchRes:
    print res.text
oneStatus = searchRes[0]
print oneStatus.text

#open craigslist 
url = 'http://nyc.craigslist.org'
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
for ilink,link in enumerate(linkList[:10]):
    post = 'dog not included - post ' + str(ilink+1) + ' ' + link
    print post 
    status = api.PostUpdate(post)
    time.sleep(10)

### next search twttr for the phrase 'dog not included' or 'dogs not included'

