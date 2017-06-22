# Reading in datasets
elo.table <- read.csv("ELO_Output_2017.csv", header = T, stringsAsFactors = F)
teams <- read.csv("USQTeams.csv", stringsAsFactors = F, header = T)

# Packages
library(ggplot2)
library(ggthemes)
library(dplyr)

# Generating List of All Teams
wc.elo <- data.frame(team.name = elo.table$team.name,
                     elo = elo.table$X3.26.2017)
wc.elo <- merge(wc.elo, teams, by = "team.name")

# Creating list of teams that played in WC
gametable <- data.frame(gamehistory.table.2017)
gametable$teamA <- as.character(gametable$teamA)
gametable$teamB <- as.character(gametable$teamB)
gametable$date <- as.Date(as.character(gametable$date), format = "%m/%d/%Y")
WCTeams <- unique(c(gametable[gametable$date > "2017-03-26",3], gametable[gametable$date > "2017-03-26",4]))

# Plotting
p1 <- ggplot(aes(x = elo, 
                 fill = factor(type, levels = c("College", "Adult Community"))
                 ), 
             data = wc.elo)

p1 + 
  geom_histogram(alpha = .4, breaks = seq(1000, 2300, by = 50), position = "identity", color = "black") + 
  theme_fivethirtyeight() +
  scale_fill_discrete(labels = c("College", "Community"), c("firebrick", "deepskyblue")) + 
  labs( title = "College vs. Community Elo Rating", y = "# of Teams", x = "Elo Rating") + 
  guides(fill=guide_legend(title= "Team Type")) + 
  geom_vline(xintercept = c(mean(wc.elo$elo[wc.elo$type == "Adult Community"]),
                            mean(wc.elo$elo[wc.elo$type == "College"])),
             size = 1) + 
  geom_text(aes(x = x + 25,
                y = y,
                label = label,
                fill = NULL),
  data = data.frame(x = c(mean(wc.elo$elo[wc.elo$type == "Adult Community"]),
                         mean(wc.elo$elo[wc.elo$type == "College"])),
                    y = c(8,8),
                    label = c("Average Community", "Average College")),
  angle = 90)

# Plotting part 2 
p2 <- ggplot(aes(x = elo, 
                 fill = factor(type, levels = c("College", "Adult Community"))))

p2 + 
  
  geom_histogram(breaks = seq(1000, 2300, by = 50), position = "identity", color = "black", alpha = .05,
                 data = wc.elo) + 
  
  geom_histogram(data = wc.elo[wc.elo$team.name %in% WCTeams,], breaks = seq(1000, 2300, by = 50),
                 col = "black", position = "identity", alpha = .5) + 
  
  theme_fivethirtyeight() +
  
  scale_fill_discrete(labels = c("College", "Community"), c("firebrick", "deepskyblue")) + 
  
  labs( title = "College vs. Community Elo Rating", y = "# of Teams", x = "Elo Rating") + 
  
  guides(fill=guide_legend(title= "Team Type")) + 
  
  geom_vline(xintercept = c(mean(wc.elo$elo[wc.elo$type == "Adult Community" & 
                                              wc.elo$team.name %in% WCTeams]),
                            mean(wc.elo$elo[wc.elo$type == "College" &
                                              wc.elo$team.name %in% WCTeams])),
             size = 1) + 
  
  geom_text(aes(x = x + 25,
                y = y,
                label = label,
                fill = NULL),
            data = data.frame(x = c(mean(wc.elo$elo[wc.elo$type == "Adult Community" & 
                                                      wc.elo$team.name %in% WCTeams]),
                                    mean(wc.elo$elo[wc.elo$type == "College" &
                                                      wc.elo$team.name %in% WCTeams])),
                              y = c(8,8),
                              label = c("Average Community", "Average College")),
            angle = 90)

t.test(elo ~ type, data = wc.elo)
t.test(elo ~ type, data = wc.elo[wc.elo$team.name %in% WCTeams,])

allteams <- wc.elo %>%
    group_by(type, region) %>%
    summarise(teams = length(type))
qualifiers <- wc.elo[wc.elo$team.name %in% WCTeams,] %>%
    group_by(type, region) %>%
    summarise(qual_teams = length(type))

wc.qualification <- merge(data.frame(allteams), data.frame(qualifiers), all = T)
wc.qualification[is.na(wc.qualification)] <- 0
wc.qualification$prop = wc.qualification$qual_teams / wc.qualification$teams

p3 <- ggplot(aes(x = factor(region),
                 fill = factor(type),
                 y= prop),
             data = wc.qualification)
p3 + 
  geom_hline(yintercept =  sum(wc.qualification$qual_teams) / sum(wc.qualification$teams)) + 
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_text(aes(label = wc.qualification$teams,
                y = prop + .02), 
            position = position_dodge(width = 1)) +
  labs( title = "World Cup Qualification", y = "Proportion of Teams Qualified", x = "Region") +
  guides(fill=guide_legend(title= "Team Type"))


  
allteams.noregion <- wc.elo %>%
  group_by(type) %>%
  summarise(teams = length(type))
qualifiers.noregion <- wc.elo[wc.elo$team.name %in% WCTeams,] %>%
  group_by(type) %>%
  summarise(qual_teams = length(type))

allteams.noregion
qualifiers.noregion
prop.test(n = c(45,99), x = c(21,39))

# 46% of comm teams qualified
# 39% of college teams qualified
