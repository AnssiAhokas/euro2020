import requests
import time
import random
import pandas as pd
from bs4 import BeautifulSoup

## Create a proxy scraper
proxy_page = requests.get('https://free-proxy-list.net/', headers={'User-Agent':'Mozilla/5.0'})
proxy_soup = BeautifulSoup(proxy_page.text,"lxml")

## Create a proxy cache
proxies = []

for item in proxy_soup.select("#proxylisttable tbody tr"):
    proxy = ':'.join([item.text for item in item.select("td")[:2]])
    proxies.append(proxy)

## Now we have the latest proxy list which we are going to use in further scraping

## Generate all pages we are going to scrape

first_url = "https://eu-football.info/_matches.php?"
dummy_url = "https://eu-football.info/_matches.php?page="
pages = [first_url]

for i_url in range(2, 200):
    complete_url = dummy_url + str(i_url)
    pages.append(complete_url)

## Create a total data cahce
all_matches_data = pd.DataFrame([])

## Start tehe loop from here

for i_page in range(0, len(pages)):

    ## Specific page
    url = pages[i_page]

    ## Print the url for monitoring the loop
    print(url)

    ## Start the proxy loop here
    for i_proxy in range(0, len(proxies)):

        ## Define the specific proxy to be used
        proxy = proxies[i_proxy]

        ## Print the proxy for monitoring
        print(proxy)

        ## Start the try loop here
        try:
            page = requests.get(url, proxies = {"http": proxy, "https": proxy}, timeout = 5)
            soup = BeautifulSoup(page.content, "html.parser")

            ## All the matches in that page
            matches = soup.find_all("div", {"class": ["d7 z6 p5 b12 m6 m11 z3 lin", "d3 z6 p5 b12 m6 m11 z3 lin"]})

            ## Create a data cache
                ## This data cache contains all the matches from the single page
                ## This data is will be appended to all_matches_data cache
            matches_data = pd.DataFrame([])

            ## Start the second loop here
            for i_match in range(0, len(matches)):

                ## Number for the match in the list
                match = matches[i_match]

                ## The true number of the match
                match_number = match.find("div", {"class": "e b12 z6 p3 z2 w51"}).text

                ## Participants
                match_team1 = match.find("div", {"class": "m20"}).text.split(" vs ")[0]
                match_team2 = match.find("div", {"class": "m20"}).text.split(" vs ")[1]

                ## Goals
                match_team1_goals = match.find("div", {"class": "e15 b12 p5 d50"}).text.split("-")[0]
                match_team2_goals = match.find("div", {"class": "e15 b12 p5 d50"}).text.split("-")[1]

                ## Time of the match
                match_time = match.find("div", {"class": "e r7 z6"}).text[0:11]

                ## Nature of the match (not necessary needed)
                match_nature = match.find("div", {"class": "e z6 r7"}).text

                ## Match data
                match_data = {"match_number": [match_number],
                              "match_team1": [match_team1],
                              "match_team2": [match_team2],
                              "match_team1_goals": [match_team1_goals],
                              "match_team2_goals": [match_team2_goals],
                              "match_time": [match_time],
                              "match_nature": [match_nature]}
                match_df = pd.DataFrame(match_data)

                ## Append the data to first data cache
                matches_data = matches_data.append(match_df)

            all_matches_data = all_matches_data.append(matches_data)

            ## Print if the data was scraped succesfully (for monitoring the loop)
            print("data scraped succesfully...")

            ## Let the loop sleep for x seconds before new request
                ## This could help us not to get blocked
                ## Random sleep time applied
            time.sleep(random.randint(5, 10))

            ## Break the loop so if the proxy works, it wont loop trough all the proxies
                ## Breakin will stop the loop for this page and move to next one
            break
        except:

            ## Print the possible error with the proxy for monitoring
            print("error with proxy...")

            ## If the proxy doesn't work, pass will move to next proxy in the list
            pass

## Write the latest CSV (2 CSVs are written, one with the timestamp and one without)
all_matches_data.to_csv("C:\\Users\\anssi\\Desktop\\Ohjelmointi\\R_Projektit\\euro2020\\data\\matches.csv")
##all_matches_data.to_csv("C:\\Users\\anssi\\Desktop\\Ohjelmointi\\R_Projektit\\euro2020\\data\\matches.csv")



