
# Reading in datasets
elo.table <- read.csv("ELO_Output_2017.csv", header = T, stringsAsFactors = F)
teams <- read.csv("USQTeams.csv", stringsAsFactors = F, header = T)
gamehistory <- read.csv("games_table_2017.csv")

# Packages
library(ggplot2)
library(ggthemes)
library(dplyr)

# Generating List of All Teams
wc.elo <- data.frame(team.name = elo.table$team.name,
                     elo = elo.table$X3.26.2017)
wc.elo <- merge(wc.elo, teams, by = "team.name")


# Plotting fit of We function to game data
gamehistory$elodiff <- gamehistory$teamA.elo - gamehistory$teamB.elo
logit_plot <- ggplot(aes(x = elodiff,
                         y = winner),
                         data = gamehistory[gamehistory$game.type == 1,])
textdf <- data.frame( x = c(-800,800),
                      y = c(.95, .05),
                      labels = c("Winner", "Loser"))

logit_plot + 
  geom_point() + 
  theme_fivethirtyeight() +
  stat_function(fun = function(x){1 / (10 ^ (-x/400) + 1)},
                col = "red",
                lwd = 1.5) +
  labs(title = "Win Expectancy", x = "Opponent's Difference Elo Rating", y = "Odds of Winning") + 
  geom_text(aes(x = x,
                y = y,
                label = labels),
            data = textdf,
            alpha = .3,
            size = 10)

we.func <- function(x,y){
    elodiff = x - y
    odds = 1 / (10 ^ (-elodiff/400) + 1)
    return(odds)
  }

custom.dist <- function(my.list, my.function) {
  n <- length(my.list)
  mat <- matrix(0, ncol = n, nrow = n)
  colnames(mat) <- rownames(mat) <- names(my.list)
  for(i in 1:nrow(mat)) {
    for(j in 1:ncol(mat)) {
      mat[i,j] <- my.function(my.list[i],my.list[j])
    }}
  return(as.dist(mat))
}

elo.vec <- wc.elo$elo
names(elo.vec) = as.character(wc.elo$team.name)
team_odds <- custom.dist(elo.vec, we.func)

# Team ranking in r.o. 32 tournament
c("Lone Star Quidditch Club",
  "Texas Cavalry",
  "Mizzou Quidditch",
  "Texas State University - San Marcos",
  "Texas Quidditch",
  "Rochester United",
  "Los Angeles Gambits",
  "Quidditch Club Boston",
  "The Lost Boys",
  "RPI Quidditch",
  "Florida's Finest",
  "Bowling Green State University",
  "District of Columbia Quidditch Club",
  "Texas A&M Quidditch",
  "Kansas Quidditch",
  "Maryland Quidditch",
  "Bosnyan Bearsharks",
  "Arizona State university",
  "The Warriors",
  "Penn State University Nittany Lions",
  "Ball State Cardinals",
  "University of Rochester Thestrals",
  "The Silver Phoenix",
  "Oklahoma State University",
  "Cal Quidditch",
  "Gulf Coast Gumbeaux",
  "Rutgers University Quidditch",
  "Ohio State",
  "University of California Los Angeles",
  "University of Miami",
  "Central Michigan Quidditch",
  "Lake Erie Elite")




