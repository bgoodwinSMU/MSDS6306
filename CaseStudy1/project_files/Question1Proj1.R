########################################################
#How many breweries are present in each state?
########################################################

########################################################
#Libraries (to give us some built in functionality)
library(usmap)
library(ggplot2)
########################################################

########################################################
#read in brewery data
breweryDat <- read.csv("breweries.csv")
#Ensure structure of data is compliant 
head(breweryDat)
########################################################

#########################################################
#Use Dplyr to group breweries by state
brewByState <- breweryDat %>% group_by(State) %>% count()
#########################################################

#########################################################
#Add breweries by state to state information dataframe
statepop$brewByState <- brewByState$n
#########################################################

#########################################################
#Fix mismatched state brewery count to state info df
statepop[1,5] <- 3
statepop[2,5] <- 7
statepop[3,5] <- 11
statepop[4,5] <- 2
statepop[8,5] <- 2
statepop[9,5] <- 1
statepop[14,5] <- 18
statepop[15,5] <- 22
statepop[16,5] <- 5
statepop[20,5] <- 9
statepop[22,5] <- 23
statepop[25,5] <- 2
statepop[26,5] <- 9
statepop[28,5] <- 5
statepop[29,5] <- 2
statepop[30,5] <- 3
statepop[32,5] <- 4
statepop[34,5] <- 19
statepop[33,5] <- 16
statepop[35,5] <- 1
statepop[45,5] <- 4
statepop[46,5] <- 10
statepop[47,5] <- 16
statepop[49,5] <- 1
statepop[50,5] <- 20
#Check data 
#View(statepop)
#View(brewByState)
#########################################################

#########################################################
#Call plot functions to plot state brewery count on USmap
plot_usmap(data = statepop, values = "brewByState", color = "red") + 
  scale_fill_continuous(name = "brewbyState", label = scales::comma) + 
  theme(legend.position = "right")+labs(title = "Total Brewery Count Per State")
#########################################################

#########################################################
#Call plot functions to plot state brewery count on USmap
plot_usmap(data = statepop, values = "brewByState", color = "red") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "brewByState", label = scales::comma
  ) + theme(legend.position = "right")+labs(title = "Total Brewery Count Per State")
#########################################################

