'script to utilize the wolfBot twitter class'

import wolfTwitter.wolfBot as wolfBot

#reload(wolfBot)
credFile = '/Users/hogstrom/Documents/code/configs/bot2.cfg'
wb = wolfBot.howl_tweet(credFile)
wb.verify_credentials()

wb.scrape_craigslist_links()
wb.retweet_search_results(search_phrase='#doglover',n_posts=2)
wb.post_craigslist_links(n_posts=2)
wb.favorite_search_results(search_phrase='#doglover',n_posts=10)