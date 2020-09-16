---
title: "Live Session 4"
author: "Ben Goodwin"
date: "9/14/2020"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}

#####Libraries#######
#####################
library(dplyr)      
library(tidyr)      
library(GGally)
library(ggplot2)
library(stringr)
library(twitteR)
library(tidyverse)
library(tidytext)
#####################
```


##Question 0

```{r}
#####################
#   Question 0, R   #
#####################
#Live Session 4 For Live Session Web Scraping Code



library(XML) #xml_Parse
library(dplyr)
library(tidyr)
library(stringi)
library(rvest) #html_table, html_node
library(ggplot2)
library(RCurl) #getURL

#Basics of Scraping XML

# Method 1: XML

data <-getURL("https://www.w3schools.com/xml/simple.xml")
doc <- xmlParse(data)
names <- xpathSApply(doc,"//name",xmlValue)
price <- xpathSApply(doc,"//price",xmlValue)
description <- xpathSApply(doc,"//description",xmlValue)
bfasts = data.frame(names,price,description)
bfasts
bfasts$description
length(grep("covered",bfasts$description))
grepl("covered",bfasts$description)
sum(grepl("covered",bfasts$description))
which(grepl("covered",bfasts$description))


# Method 2: rvest

hp<-read_html("https://www.w3schools.com/xml/simple.xml")
hp_nameR <- html_nodes(hp,"name")
hp_priceR <- html_nodes(hp,"price")
hp_descR <- html_nodes(hp,"description")
hp_nameR
hp_name = stri_sub(hp_nameR,7,-8)
hp_name
hp_price = stri_sub(hp_priceR,8,-9)
hp_price
hp_desc = stri_sub(hp_descR,14,-15)
hp_desc
bfast = data.frame(hp_name,hp_price,hp_desc)
grep("toast", bfast$hp_desc)
grepl("toast",bfast$hp_desc)

sum(grepl("toast",bfast$hp_desc))

```

```{r}
#####################
#   Question 0, R   #
#####################


#################################################################################################
#You have been hired by a restaurateur to some research on Sushi restaurants in Baltimore.      #
#	You have come across data on the web contained in the following XML file.   	                #
#	Data: https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml                  #
                                                                                                #
#	Scrape the XML page for name, zipcode and city council district.  (Use either the XML or rvest# package.)                                                                                       #
#	Make a dataframe with just those columns.                                                     #
#	Are there any Sushi restaurants in Baltimore? (Where the dataset is from.)                    #
#	If so, can you estimate how many?                                                             #
#	Filter the dataframe for just downtown restaurants (Council District 11).                     #
#	Are there any Sushi restaurants downtown?  # research the “grep” function                     #
#	If so, estimate how many “Sushi” restaurants are in Downtown                                  #
#	Make a barplot of the estimated number of restaurants (Sushi or otherwise) in each council.   #
#################################################################################################


data2 <- read_html("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml")  
data2_nameR <- html_nodes(data2,"name")
data2_zipcode <- html_nodes(data2, "zipcode")
data2_counilDistrict <- html_nodes(data2, "councildistrict")

data2_nameR
data2_nameR = stri_sub(data2_nameR,7,-8)
data2_nameR
data2_zipcode = stri_sub(data2_zipcode,8,-9)
data2_zipcode
data2_counilDistrict = stri_sub(data2_counilDistrict,14,-15)
data2_counilDistrict

#put info into df
baltDat = data.frame(data2_nameR,data2_zipcode,data2_counilDistrict)


#Any sushi restaurants?
sushiInfo <-  baltDat %>%filter_all(any_vars(grepl("SUSHI",.)))

sushiInfo

#District 11 

dist11Res <- baltDat%>% filter_all((any_vars(grepl("11",.))))

dist11Res

#Which sushi in District 11?

sushiInfoDist11 <- sushiInfo %>% filter_all((any_vars(grepl("11",.))))

sushiInfoDist11


```

```{r}
#Barplot for restaurant count

counts <- read.csv("counts.csv")
View(counts)


p<-ggplot(data=counts, aes(x=District, y=Count)) +
  geom_bar(stat="identity",fill="steelblue",)+theme_minimal()+
  xlab("District Number") + ylab("District Total Restaurant Count")+geom_text(aes(label=Count), vjust=1.6, color="white", size=3.5)
p

```

```{r}
#####################
#   Question 2, R   #
#####################

#twitteR

api_key = "LppEOUYKCTdaAXrEH3Nrh40oz"
api_secret ="SUYZKto8Cpubto745TRZQ9CvmwLuttQMNU50ivpBVWk95KBdAk"
access_token ="221884810-J92SaubEFoHcTMUKk2r6BPLaPcTndHDdOqqqAHh6"
access_secret="N0TDhqqwxElWLfc1n4mw023MlAo5pK6orA8L5RsprkR7M"
setup_twitter_oauth(api_key,api_secret,access_token,access_secret)


#get some tweets
tweets=searchTwitter("New Brunswick",n=10,lang="en")

tweets

#locations
trend=availableTrendLocations()
#View(trend)

#trends for Quebec, closest region to NB
getTrends(3534)


fn_twitter <- searchTwitter("#nbelection2020",n=1000,lang="en")

fn_twitter_df <- twListToDF(fn_twitter) # Convert to data frame


tweet_words <- fn_twitter_df %>% select(id, text) %>% unnest_tokens(word,text)

tweet_words %>% count(word,sort=T) %>% slice(1:20) %>% 
  ggplot(aes(x = reorder(word, 
    n, function(n) -n), y = n)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60, 
    hjust = 1)) + xlab("")

# Create a list of stop words: a list of words that are not worth including

my_stop_words <- stop_words %>% select(-lexicon) %>% 
  bind_rows(data.frame(word = c("the", "rt", "to", "in","https","t.co","a","for","and","is","of","you","if","on","at","up","all")))

tweet_words_interesting <- tweet_words %>% anti_join(my_stop_words)

tweet_words_interesting %>% group_by(word) %>% tally(sort=TRUE) %>% slice(1:25) %>% ggplot(aes(x = reorder(word, 
    n, function(n) -n), y = n)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60, 
    hjust = 1)) + xlab("")



```


