'script to utilize the wolfBot twitter class'

import wolfTwitter.wolfBot as wolfBot

#reload(wolfBot)
credFile = '/Users/hogstrom/Documents/code/configs/bot2.cfg'
wb = wolfBot.howl_tweet(credFile)
# wb.get_craigslist_links()
wb.scrape_craigslist_links()
wb.retweet_search_results(search_phrase='dogs not included',n_posts=5)
wb.post_craigslist_links(n_posts=2)
