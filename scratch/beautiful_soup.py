import mechanize
import cookielib
from BeautifulSoup import BeautifulSoup
import urllib2
# import html2text

# Browser
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

# User-Agent (this is cheating, ok?)
# br.addheaders = [('User-agent', 'Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.1) Gecko/2008071615 Fedora/3.0.1-1.fc9 Firefox/3.0.1')]

# The site we will navigate into, handling it's session
br.open('http://www.medicinescomplete.com/mc/martindale/current/')

# Select the first (index zero) form
br.select_form(nr=0)

# config file
CONFIG = '/Users/hogstrom/Documents/code/bot3.cfg'
keyDict = {}
with open(CONFIG, 'r') as f:
    for line in f:
        line = line.replace('\n','')
        splt = line.split(' ')
        keyDict[splt[0]] = splt[1]

# User credentials
br.form['UserEmail'] = keyDict['usrnm']
br.form['UserPass'] = keyDict['passwrd'] 

# Login
br.submit()
html = br.response().read()
soup = BeautifulSoup(html)
head = soup.contents[1].contents[0]

br.follow_link(text='Drugs and Ancillary Substances')
br.follow_link(text='By therapeutic use')

html = br.response().read()
soup = BeautifulSoup(html)
titleTag = soup.html.head.title

# all_msg_links = [l for l in br.links()]
# for lnk in all_msg_links:
#     print lnk.text

body1 = soup.html.body.contents[2]
LinkList = body1.findAll('a')
for lnk in LinkList:
    if lnk.has_key('title'):
        classTitle = lnk['title']
        # hrefVal = lnk['href']
        # br.open(hrefVal)
        br.follow_link(text=str(classTitle))
        html = br.response().read()
        classSoup = BeautifulSoup(html)




['title']

#open craigslist 
url = 'http://boston.craigslist.org'
br.open(url)
br.select_form(nr=0)
br.form['query'] = '"dog not included"' #see form.controls
br.submit()
html = br.response().read()
soup = BeautifulSoup(html)
titleTag = soup.html.head.title


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




forms = mechanize.ParseFile(url)

search  = ['tour','58']
c = urllib2.urlopen(url)
page = c.read()
soup = BeautifulSoup(page)
links =  soup('a')

for link in links:
   for item in search:
     if (string.find(str(link.string).lower(),item) != -1):
         print '  > FOUND SEARCH WORD : ' + item
         webbrowser.open(str(link['href']))


