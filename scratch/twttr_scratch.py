


# added SimpleTwitterBot with
# git submodule add https://github.com/yasulab/SimpleTwitterBot.git

# PYTHONPATH=~/one/location:$PYTHONPATH
# PYTHONPATH=~/second/location:$PYTHONPATH
# export PYTHONPATH

# PYTHONPATH=/Users/hogstrom/Documents/code/pestle:$PYTHONPATH
# PYTHONPATH=/Users/hogstrom/Documents/code/jailbird:$PYTHONPATH
# PYTHONPATH=/Users/hogstrom/Documents/code/ModernKicks:$PYTHONPATH
# export PYTHONPATH



import twitter 
import twitter, os, time, sys
from random import choice
from config import Config

# log files
LATESTFILE = 'bot_latest.txt'
LOGFILE = 'bot_log.txt'

# config file
CONFIG = '/Users/hogstrom/Documents/code/configs/bot2.cfg'
keyDict = {}
with open(CONFIG, 'r') as f:
    for line in f:
        line = line.replace('\n','')
        splt = line.split(' ')
        keyDict[splt[0]] = splt[1]

api = twitter.api(consumer_key=keyDict['consumer_key'],
    consumer_secret=keyDict['consumer_secret'],
    access_token_key=keyDict['access_token_key'],
    access_token_secret=keyDict['access_token_secret'])


from twitter import *

OAUTH_TOKEN=keyDict['access_token_key']
OAUTH_SECRET=keyDict['access_token_secret']
CONSUMER_KEY=keyDict['consumer_key']
CONSUMER_SECRET=keyDict['consumer_secret']

o1 = OAuth(OAUTH_TOKEN, OAUTH_SECRET, CONSUMER_KEY, CONSUMER_SECRET)
t = Twitter(auth=o1)
# see "Authentication" section below for tokens and keys
t = Twitter(
            auth=OAuth(OAUTH_TOKEN, OAUTH_SECRET,
                       CONSUMER_KEY, CONSUMER_SECRET)
           )

# Get your "home" timeline
t.statuses.home_timeline()

twttr = twitter.Twitter(auth=twitter.OAuth(OAUTH_TOKEN, OAUTH_SECRET,
                       CONSUMER_KEY, CONSUMER_SECRET))

# Get a particular friend's timeline
t.statuses.friends_timeline(id="billybob")

# Also supported (but totally weird)
t.statuses.friends_timeline.billybob()

# to pass in GET/POST parameters, such as `count`
t.statuses.home_timeline(count=5)

# to pass in the GET/POST parameter `id` you need to use `_id`
t.statuses.oembed(_id=1234567890)

# Update your status
t.statuses.update(
    status="Using @sixohsix's sweet Python Twitter Tools.")

# Send a direct message
t.direct_messages.new(
    user="billybob",
    text="I think yer swell!")

# Get the members of tamtar's list "Things That Are Rad"
t._("tamtar")._("things-that-are-rad").members()

# Note how the magic `_` method can be used to insert data
# into the middle of a call. You can also use replacement:
t.user.list.members(user="tamtar", list="things-that-are-rad")

# An *optional* `_timeout` parameter can also be used for API
# calls which take much more time than normal or twitter stops
# responding for some reasone
t.users.lookup(screen_name=','.join(A_LIST_OF_100_SCREEN_NAMES), _timeout=1)

# Overriding Method: GET/POST
# you should not need to use this method as this library properly
# detects whether GET or POST should be used, Nevertheless
# to force a particular method, use `_method`
t.statuses.oembed(_id=1234567890, _method='GET')



MY_TWITTER_CREDS = os.path.expanduser('~/.my_app_credentials')
if not os.path.exists(MY_TWITTER_CREDS):
    oauth_dance("My App Name", CONSUMER_KEY, CONSUMER_SECRET,
                MY_TWITTER_CREDS)

oauth_token, oauth_secret = read_token_file(MY_TWITTER_CREDS)

twitter = Twitter(auth=OAuth(
    oauth_token, oauth_secret, CONSUMER_KEY, CONSUMER_SECRET))
t2 = tw.Twitter(domain='api.twitter.com', api_version='1.1', auth=OAuth(oauth_token, oauth_secret, CONSUMER_KEY, CONSUMER_SECRET))
# Now work with Twitter
twitter.statuses.update('Hello, world!')