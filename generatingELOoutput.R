#####################################################################################
### LATEST VERSION OF QUIDDITCH ELO - 12/29/2016 ####################################
#####################################################################################

# Alright dudes, here's what I've got for the latest ELO ranking system

# First off, provisional matches are now working
  # I'm generating a detailed write-up, but here's a quick overview:
  # For your first X number of games, teams accumulate points based on opposing team's ELO and the game score
  # This point value is flexible, and we can make provisional teams gain more or less points based on whether 
    # their opponent is also provisional or is an established team.
  # After playing X number of games, the total point value accrued is averaged
  # Games between provisional and standard teams don't change the standard team's ELO
  # Based off http://gameknot.com/help-answer.pl?question=29

# Second, data output is now structured as a list (array of arrays, not sure how common this format is in other languages)
# Below, I've pulled out the elements of each list in 3 sections:
  # elo.table.201X - ELO values of a team over the course of a season
  # team.table.201X - Number of games a team plays in a season, when they started, when they progressed beyond provisional status
  # gamehistory.table.201X - All games played within a season, type of game, win expectancy, and ELO change

# For optimizing the formula, here's what can be changed:
  # initelo - initial elo value of new teams
  # carryover - what fraction of elo points carry over from season to season
  # K - k-factor, the base level of elo change associated with each game played
  # winvar - win variance, determines win expectancy function an elo change
    # the higher the win variance value, the more variance teams show in performance about their ELO value
    # a higher winvar says that upsets are more common than a lower winvar, for example
  # placematches - number of games teams need to play before becoming standard
  # placementshift - value of winning/losing games against other provisional teams when a team has provisional status
  # standardshift - value of wining/losing games against established (standard) teams when a team has provisional status

# MAKE SURE TO RUN OTHER SCRIPTS (elocalc_func.R and placement_func.R) BEFOREHAND

# Set working directory:
games <- read.csv("Games_6_9_2017.csv", stringsAsFactors=F, header=T)

#install.packages("reshape2") # Need to install before producing datasets for graphing
library(reshape2)
library(ggplot2)

# Assigning winning team
games$winner <- as.numeric(games$teamA_score - games$teamB_score > 0)

# Loading in necessary functions
source(file = "elocalc_func.R")
source(file = "placement_func.R")

### Generating ELO data ##############################################################

# 2014 Data
output <- elocalc_func(initelo = 1500,
             gamesdata = games[games$season == 2014,],
             carryover = .7,
             K= 60,
             winvar = 400,
             placematches = 5,
             placementshift = 200,
             standardshift = 200)

elo.table.2014 <- output[1]
team.table.2014 <- output[2]
gamehistory.table.2014 <- output[3]

# 2015 Data
output <- elocalc_func(initelo = 1500,
                       gamesdata = games[games$season == 2015,],
                       carryover = .7,
                       oldelotable = data.frame(elo.table.2014)[,1:5],
                       K= 60,
                       winvar = 400,
                       placematches = 5,
                       placementshift = 200,
                       standardshift = 200)

elo.table.2015 <- output[1]
team.table.2015 <- output[2]
gamehistory.table.2015 <- output[3]

# 2016 data
output <- elocalc_func(initelo = 1500,
                       gamesdata = games[games$season == 2016,],
                       carryover = .7,
                       oldelotable = data.frame(elo.table.2015)[,1:5],
                       K= 60,
                       winvar = 400,
                       placematches = 5,
                       placementshift = 200,
                       standardshift = 200)

elo.table.2016 <- output[1]
team.table.2016 <- output[2]
gamehistory.table.2016 <- output[3]

# 2017 data
output <- elocalc_func(initelo = 1500,
                       gamesdata = games[games$season == 2017,],
                       carryover = .7,
                       oldelotable = data.frame(elo.table.2016)[,1:5],
                       K= 60,
                       winvar = 400,
                       placematches = 5,
                       placementshift = 200,
                       standardshift = 200)
games[is.na(games$qpd),]

elo.table.2017 <- output[1]
team.table.2017 <- output[2]
gamehistory.table.2017 <- output[3]

View(as.data.frame(elo.table.2017)[,(1:4)])

#### Writing files ########################################################################

# For example, here's the 2016 ELO table
outputfile <- data.frame(gamehistory.table.2017)
write.csv(outputfile, file="games_table_2017.csv")

#### Graphing ELO over time ###############################################################

# You can run these code chunks separately to produce rough line graphs of ELO change within a season

# 2014 Season: 

elochange.data.2014 <- melt(data.frame(elo.table.2014)[,-c(2,3,4)], variable.name = "Date")
colnames(elochange.data.2014)[3] <- 'ELO'
elochange.data.2014$Date <- sub("X", "", elochange.data.2014$Date)
elochange.data.2014$Date <- as.Date(elochange.data.2014$Date, format="%m.%d.%Y")

p1 <- ggplot(aes(x=Date, y=ELO, color=team.name), data=elochange.data.2014)
p1 + geom_line() + theme(legend.position = "none") + ggtitle("ELO Change 2014")


# 2015 Season:

elochange.data.2015 <- melt(data.frame(elo.table.2015)[,-c(2,3,4)], variable.name = "Date")
colnames(elochange.data.2015)[3] <- 'ELO'
elochange.data.2015$Date <- sub("X", "", elochange.data.2015$Date)
elochange.data.2015$Date <- as.Date(elochange.data.2015$Date, format="%m.%d.%Y")

p1 <- ggplot(aes(x=Date, y=ELO, color=team.name), data=elochange.data.2015)
p1 + geom_line() + theme(legend.position = "none") + ggtitle("ELO Change 2015")


# 2016 Season:

elochange.data.2016 <- melt(data.frame(elo.table.2016)[,-c(2:5)], variable.name = "Date")
colnames(elochange.data.2016)[3] <- 'ELO'
elochange.data.2016$Date <- sub("X", "", elochange.data.2016$Date)
elochange.data.2016$Date <- as.Date(elochange.data.2016$Date, format="%m.%d.%Y")

p1 <- ggplot(aes(x=Date, y=ELO, color=team.name), data=elochange.data.2016)
p1 + geom_line() + theme(legend.position = "none") + ggtitle("ELO Change 2016")

# 2017 Season:

elochange.data.2017 <- melt(data.frame(elo.table.2017)[,-c(2:5)], variable.name = "Date")
colnames(elochange.data.2017)[3] <- 'ELO'
elochange.data.2017$Date <- sub("X", "", elochange.data.2017$Date)
elochange.data.2017$Date <- as.Date(elochange.data.2017$Date, format="%m.%d.%Y")

p1 <- ggplot(aes(x=Date, y=ELO, color=team.name), data=elochange.data.2017)
p1 + geom_line() + theme(legend.position = "none") + ggtitle("ELO Change 2017")




