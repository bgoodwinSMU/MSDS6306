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
nationBrewPlot <- plot_usmap(data = statepop, values = "brewByState",labels=TRUE, color = "grey73") + scale_fill_continuous(low = "purple", high = "green", name = "Brewery Count", label = scales::comma) + theme(legend.position = "bottom")+labs(title = "Total Brewery Count Per State")

#display plot
nationBrewPlot
#########################################################


#########################################################
#Break down by region, NE first
NEplot <- plot_usmap(data=statepop, values = "brewByState",labels = TRUE,include = .new_england,color = "grey73") + scale_fill_continuous(low = "purple", high = "green", name = "Brewery Count", label = scales::comma)+ theme(legend.position = "bottom")+labs(title = "Total Brewery Count Per State")
NEplot
#########################################################

#########################################################
#Break down by region, Mid Atlantic second
MAplot <- plot_usmap(data=statepop, values = "brewByState",labels = TRUE,include = .mid_atlantic,color = "grey73") + scale_fill_continuous(low = "purple", high = "green", name = "Brewery Count", label = scales::comma)+ theme(legend.position = "bottom")+labs(title = "Total Brewery Count Per State")
MAplot
#########################################################


#########################################################
#Break down by region, East North Central third
ENCplot <- plot_usmap(data=statepop, values = "brewByState",labels = TRUE,include = .east_north_central,color = "grey73") + scale_fill_continuous(low = "purple", high = "green", name = "Brewery Count", label = scales::comma)+ theme(legend.position = "bottom")+labs(title = "Total Brewery Count Per State")
ENCplot
#########################################################

#########################################################
#Break down by region, West North Central fourth
WNCplot <- plot_usmap(data=statepop, values = "brewByState",labels = TRUE,include = .west_north_central,color = "grey73") + scale_fill_continuous(low = "purple", high = "green", name = "Brewery Count", label = scales::comma)+ theme(legend.position = "bottom")+labs(title = "Total Brewery Count Per State")
WNCplot
#########################################################

#########################################################
#Break down by region, South Atlantic fifth
SAplot <- plot_usmap(data=statepop, values = "brewByState",labels = TRUE,include = .south_atlantic,color = "grey73") + scale_fill_continuous(low = "purple", high = "green", name = "Brewery Count", label = scales::comma)+ theme(legend.position = "bottom")+labs(title = "Total Brewery Count Per State")
SAplot
#########################################################

#########################################################
#Break down by region, East South Central sixth
ESCplot <- plot_usmap(data=statepop, values = "brewByState",labels = TRUE,include = .east_south_central,color = "grey73") + scale_fill_continuous(low = "purple", high = "green", name = "Brewery Count", label = scales::comma)+ theme(legend.position = "bottom")+labs(title = "Total Brewery Count Per State")
ESCplot
#########################################################

#########################################################
#Break down by region, West South Central seventh
WSCplot <- plot_usmap(data=statepop, values = "brewByState",labels = TRUE,include = .west_south_central,color = "grey73") + scale_fill_continuous(low = "purple", high = "green", name = "Brewery Count", label = scales::comma)+ theme(legend.position = "bottom")+labs(title = "Total Brewery Count Per State")
WSCplot
#########################################################

#########################################################
#Break down by region, Mountain eighth 
Mplot <- plot_usmap(data=statepop, values = "brewByState",labels = TRUE,include = .mountain,color = "grey73") + scale_fill_continuous(low = "purple", high = "green", name = "Brewery Count", label = scales::comma)+ theme(legend.position = "bottom")+labs(title = "Total Brewery Count Per State")
Mplot
#########################################################

#########################################################
#Break down by region, Pacific ninth 
Pplot <- plot_usmap(data=statepop, values = "brewByState",labels = TRUE,include = .pacific,color = "grey73") + scale_fill_continuous(low = "purple", high = "green", name = "Brewery Count", label = scales::comma)+ theme(legend.position = "bottom")+labs(title = "Total Brewery Count Per State")
Pplot
#########################################################


