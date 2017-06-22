#' ELO Calculations function
#'
#' Function used in ELO calculations to generate placement match data
#' @param gamedsata Dataset containing scores of individual games
#' @param carryover Carryover from one year to next
#' @param K K-coefficient
#' @param oldelotable Table containing prior ELO values
#' @param winvar Variance in win estimation function
#' @param placematches Number of placement matches before team becomes standard
#' @param placementshift ELO shift associated with provisional games
#' @param standardshift ELO shift associated with standard games
#' @keywords Quidditch ELO Ratings
#' @author Evan Batzer

elocalc_func <- function(initelo, # Initial ELO Values of teams not previously recorded
                    gamesdata, # Dataset containing scores of individual games
                    carryover,  # Carryover from one year to next (estimated to be .5)
                    K, # K-coefficient
                    oldelotable, # Table containing prior ELO values
                    winvar, # Variance in win estimation function
                    placematches,
                    placementshift,
                    standardshift){

  # Changing column names of old elo table to be consistent, or generates empty dataset if not supplied

  if(missing(oldelotable)){
    oldelotable <- data.frame(team.name = NA, elo = NA)
  }else{
    colnames(oldelotable) <- c("team.name", "team.age", "elo", "elo.seasonstart", "team.provelo")
  }

  # Establishes what teams are in each game dataset
  teams <- unique(c(gamesdata$teamA_name, gamesdata$teamB_name))

  # Teams which are found in the current dataset and the old data table
  elo.seasonstart <- c()

  # Assigning team age
  team.age <- c()

  # Assigning team type
  team.type <- c()
  
  # Assigning team provisional elo
  team.provelo <- c()
  

  for( i in 1:length(teams)){
    if(length(which(oldelotable$team.name == teams[i])) == 0){
      elo.seasonstart[i] <- initelo
      team.age[i] <- unique(gamesdata$season)
      team.type[i] <- "provisional"
      team.provelo[i] <- 0
    }else if( length(which(oldelotable$team.name == teams[i])) == 1){
      elo.seasonstart[i] <- oldelotable$elo[which(oldelotable$team.name == teams[i])] -
        (oldelotable$elo[which(oldelotable$team.name == teams[i])] - initelo) * (1-carryover)
      team.age[i] <- oldelotable$team.age[which(oldelotable$team.name == teams[i])]
      team.type[i] <- "standard"
      team.provelo[i] <- oldelotable$team.provelo[which(oldelotable$team.name == teams[i])]
    }else if (length(which(oldelotable$team.name == teams[i])) > 1){
      stop(paste("Error: Team names are not unique\nCheck Team:", teams[i]))
    }
  }

  # Generating table of elo values
  elotable <- data.frame(team.name = teams,
                         team.age = team.age,
                         elo = elo.seasonstart,
                         elo.seasonstart = elo.seasonstart,
                         team.provelo = team.provelo)


  # Generating team info table
  teamhistory <- data.frame(team.name = teams,
                            team.age = team.age,
                            team.type = factor(team.type, levels=c("provisional", "standard")),
                            matches.played = rep(0, length(teams)))

  # Generating unique match history table
  matchhistory <- data.frame(date=gamesdata$date,
                             season = gamesdata$season,
                             teamA = gamesdata$teamA_name,
                             teamB = gamesdata$teamB_name,
                             winner = gamesdata$winner,
                             match.number = gamesdata$matchno,
                             teamA.elo = rep(0, length(gamesdata$date)),
                             teamB.elo = rep(0, length(gamesdata$date)),
                             win.expectancy = rep(0, length(gamesdata$date)),
                             elo.change = rep(0, length(gamesdata$date)),
                             game.type = rep(0, length(gamesdata$date)))

  # Calculating ELO value for each timestep
  # This should be updated.
  # Instead of calculating ELO change for each games each timestep,
  # we should calculate cumulative change in ELO for the event

  for( timestep in unique(gamesdata$date)){

    gamesdata.time <- gamesdata[which(gamesdata$date == timestep),]

    # Refreshes provisional status of teams
    for(i in 1:nrow(teamhistory)){
      if(teamhistory$matches.played[i] > placematches & teamhistory$team.type[i] == "provisional"){
        teamhistory$team.type[i] <- "standard"
        elotable$elo[which(elotable$team.name == teamhistory$team.name[i])] <-  
          elotable$team.provelo[which(elotable$team.name == teamhistory$team.name[i])] / teamhistory$matches.played[i]
      }
    }

    # ELO calculations
    for( gameindex in gamesdata.time$matchno){

      if( teamhistory$team.type[teamhistory$team.name ==
                                gamesdata.time$teamA_name[which(
                                  gamesdata.time$matchno == gameindex)]] == "standard" &
          teamhistory$team.type[teamhistory$team.name ==
                                gamesdata.time$teamB_name[which(
                                  gamesdata.time$matchno == gameindex)]] == "standard"){

        matchhistory$game.type[matchhistory$match.number == gameindex] <- 1

        a.oldrating <- elotable$elo[elotable$team.name ==
                                      gamesdata.time$teamA_name[which(
                                        gamesdata.time$matchno == gameindex)]]

        matchhistory$teamA.elo[matchhistory$match.number == gameindex] <- a.oldrating


        b.oldrating <- elotable$elo[elotable$team.name ==
                                      gamesdata.time$teamB_name[which(
                                        gamesdata.time$matchno == gameindex)]]

        matchhistory$teamB.elo[matchhistory$match.number == gameindex] <- b.oldrating


        dr <- a.oldrating - b.oldrating # Difference in rating

        We <- 1 / (10 ^ (-dr/winvar) + 1) # Win expectancy
        # For team B, win expectancy is just the inverse of this

        matchhistory$win.expectancy[matchhistory$match.number == gameindex] <- We

        # Calculating quaffle point differential
        quaffledifferential <- gamesdata.time$qpd[which(gamesdata.time$matchno == gameindex)]

        # Multiplies K-coefficient by victory margin
        marginmultiplier <- c()

        if(quaffledifferential <= 30){
          # Teams that win by quaffle differential 30 or less are assumed to have played a competitive game
          marginmultiplier <- 1

        }else{
          # Anything over (blowouts) improve standings by the 1 + log of remaining QPD.
          marginmultiplier <- 1 + log10(abs(quaffledifferential) - 30 + 1)

        }

        # K coefficent also varies with tournament, with 2 ^ match type
        # Match type 0 = matches played outside of major tournaments
        # Match type 1 = matches played during regional championships
        # Match type 2 = matches played during world cup

        if(gamesdata$matchtype[which(gamesdata.time$matchno == gameindex)] == 0){
          matchmultiplier <- 1
        }else{
          matchmultiplier <- 2.5
        }

        # Elo change
        matchhistory$elo.change[matchhistory$match.number == gameindex] <-
          (K * matchmultiplier * marginmultiplier) *
          (gamesdata.time$winner[which(
            gamesdata.time$matchno == gameindex)] - We)

        # Team A elo rating change
        a.newrating <- a.oldrating + (K * matchmultiplier * marginmultiplier) *
          (gamesdata.time$winner[which(
            gamesdata.time$matchno == gameindex)] - We)

        elotable$elo[elotable$team.name == gamesdata.time$teamA_name[which(
          gamesdata.time$matchno == gameindex)]] = a.newrating

        # Team B elo rating change
        b.newrating <- b.oldrating + (K * matchmultiplier * marginmultiplier) *
          ((1-gamesdata.time$winner[which(
            gamesdata.time$matchno == gameindex)]) - (1-We))

        elotable$elo[elotable$team.name == gamesdata.time$teamB_name[which(
          gamesdata.time$matchno == gameindex)]] = b.newrating

        # Updating total match numbers
        teams.playing <- c(gamesdata.time$teamA_name[which(
          gamesdata.time$matchno == gameindex)],
          gamesdata.time$teamB_name[which(
            gamesdata.time$matchno == gameindex)])

        teamhistory$matches.played[teamhistory$team.name %in% teams.playing] <-
          teamhistory$matches.played[teamhistory$team.name %in% teams.playing] + 1


      }
      
      if(teamhistory$team.type[teamhistory$team.name ==
                               gamesdata.time$teamA_name[which(
                                 gamesdata.time$matchno == gameindex)]] == "provisional" |
         teamhistory$team.type[teamhistory$team.name ==
                               gamesdata.time$teamB_name[which(
                                 gamesdata.time$matchno == gameindex)]] == "provisional"){


        matchhistory$game.type[matchhistory$match.number == gameindex] <- 0


        # Elo change
        matchhistory$elo.change[matchhistory$match.number == gameindex] <- 0

        # Assigning team provisionality

        if(teamhistory$team.type[teamhistory$team.name ==
                                 gamesdata.time$teamA_name[which(
                                   gamesdata.time$matchno == gameindex)]] == "standard"){
          a.status <- 1
        }else{
          a.status <- 0
        }

        if(teamhistory$team.type[teamhistory$team.name ==
                                 gamesdata.time$teamB_name[which(
                                   gamesdata.time$matchno == gameindex)]] == "standard"){
          b.status <- 1
        }else{
          b.status <- 0
        }

        # Assigning ELO change

        a.oldrating <- elotable$elo[elotable$team.name ==
                                      gamesdata.time$teamA_name[which(
                                        gamesdata.time$matchno == gameindex)]]

        matchhistory$teamA.elo[matchhistory$match.number == gameindex] <- a.oldrating


        b.oldrating <- elotable$elo[elotable$team.name ==
                                      gamesdata.time$teamB_name[which(
                                        gamesdata.time$matchno == gameindex)]]

        matchhistory$teamB.elo[matchhistory$match.number == gameindex] <- b.oldrating


        # Using placement match function

        a.placematches <- teamhistory$matches.played[teamhistory$team.name ==
                                                       gamesdata.time$teamA_name[which(
                                                         gamesdata.time$matchno == gameindex)]]
        b.placematches <- teamhistory$matches.played[teamhistory$team.name ==
                                                       gamesdata.time$teamB_name[which(
                                                         gamesdata.time$matchno == gameindex)]]
        
        a.provelo <- elotable$team.provelo[elotable$team.name ==
                                                       gamesdata.time$teamA_name[which(
                                                         gamesdata.time$matchno == gameindex)]]
        b.provelo <- elotable$team.provelo[elotable$team.name ==
                                                       gamesdata.time$teamB_name[which(
                                                         gamesdata.time$matchno == gameindex)]]
        

        elooutcomes <- placement_func(gamedata = gamesdata.time[gamesdata.time$matchno == gameindex,],
                                      a.provelo = a.provelo,
                                      b.provelo = b.provelo,
                                      a.rating = a.oldrating,
                                      b.rating = b.oldrating,
                                      a.status = a.status,
                                      b.status = b.status,
                                      a.placematches = a.placematches,
                                      b.placematches = b.placematches,
                                      placementshift = placementshift,
                                      standardshift = standardshift)
        
        a.newrating <- elooutcomes[1]
        a.newprovelo <- elooutcomes[2]
        b.newrating <- elooutcomes[3]
        b.newprovelo <- elooutcomes[4]
        
        elotable$elo[elotable$team.name == gamesdata.time$teamA_name[which(
          gamesdata.time$matchno == gameindex)]] = a.newrating
        
        elotable$elo[elotable$team.name == gamesdata.time$teamB_name[which(
          gamesdata.time$matchno == gameindex)]] = b.newrating
        
        elotable$team.provelo[elotable$team.name == gamesdata.time$teamA_name[which(
          gamesdata.time$matchno == gameindex)]] = a.newprovelo

        elotable$team.provelo[elotable$team.name == gamesdata.time$teamB_name[which(
          gamesdata.time$matchno == gameindex)]] = b.newprovelo


        # Updating total match numbers
        teams.playing <- c(gamesdata.time$teamA_name[which(
          gamesdata.time$matchno == gameindex)],
          gamesdata.time$teamB_name[which(
            gamesdata.time$matchno == gameindex)])

        teamhistory$matches.played[teamhistory$team.name %in% teams.playing] <-
          teamhistory$matches.played[teamhistory$team.name %in% teams.playing] + 1

      }
    }

    # New ELO vector, used to calculate elo change over timesteps
    newelovec <- elotable$elo
    elotable <- cbind(elotable, newelovec)
    colnames(elotable)[ncol(elotable)] <- as.character(timestep)

  }

  return(list(elotable, teamhistory, matchhistory))

}
