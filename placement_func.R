#' Placement matches function
#'
#' Function used in ELO calculations to generate placement match data
#' @param gamedata Data on individual quidditch game, including quaffle point differential
#' @param rating Team original ELO rating
#' @param status Team provisional (1) or standard (0) status
#' @param placementshift ELO shift associated with provisional games
#' @param standardshift ELO shift associated with standard games
#' @keywords Quidditch ELO Ratings
#' @author Evan Batzer

placement_func <- function(gamedata, # Data on individual game, including winning team and point differential
                           a.status, # Team A status (1/0 as provisional/standard)
                           b.status, # Team B status (1/0 as provisional/standard)
                           a.rating,
                           b.rating, 
                           a.provelo, # Team A total provisional ELO accumulated
                           b.provelo, # Team B total provisional ELO accumulated
                           a.placematches, # Number of placement matches played by team A
                           b.placematches, # Number of placement matches played by team B
                           placementshift, # Shift in ELO associated with provisional games
                           standardshift){ # Shift in ELO associated with standard games
  
  print("Processing Placement Matches:")
  
  a.newrating <- c()
  b.newrating <- c()
  a.newprovelo <- c()
  b.newprovelo <- c()
  
  print(paste(a.rating, b.rating))
  print(paste(a.status, b.status))
  

  # Converts winner values of 0 to -1 for use in calculations
  if(gamedata$winner == 0){
    gamedata$winner <- -1
  }

  # If both teams are provisional, change ELO of both teams by placement shift value
  if(a.status == 0 & b.status == 0){
    
    # Team A rating change
    a.newprovelo <- a.provelo + ((a.rating + b.rating) / 2) + (placementshift * # Placement shift value
                                                           gamedata$winner * # Winner value
                                                           log10(abs(gamedata$qpd + 1)/10)) # Log of quaffle point differential
    
    a.newrating = a.newprovelo / (a.placematches + 1)

    # Team B rating change
    b.newprovelo <- b.provelo + ((a.rating + b.rating) / 2) - (placementshift * # Placement shift value
                                                            gamedata$winner * # Winner value
                                                            log10(abs(gamedata$qpd + 1)/10)) # Log of quaffle point differential
    b.newrating = b.newprovelo / (b.placematches + 1)
    
    
  }

  # If only team A is provisional, change the ELO of that team by the standard shift value.
  if(a.status == 0 & b.status == 1){
    
    # Team A rating change
    a.newprovelo <- a.provelo + ((a.rating + b.rating) / 2) + (placementshift * # Placement shift value
                                                            gamedata$winner * # Winner value
                                                            log10(abs(gamedata$qpd + 1)/10)) # Log of quaffle point differential
    a.newrating = a.newprovelo / (a.placematches + 1)
      
    # Team B rating change (no change)

    b.newprovelo = b.provelo
    b.newrating = b.rating
  }

  # If only team B is provisional, change the ELO of that team by the standard shift value
  
  if(a.status == 1 & b.status == 0){
    
    # Team A rating change (no change)
    
    a.newprovelo = a.provelo
    a.newrating = a.rating
    
    # Team B rating change
    b.newprovelo <- b.provelo + ((a.rating + b.rating) / 2) - (placementshift * # Placement shift value
                                                            gamedata$winner * # Winner value
                                                            log10(abs(gamedata$qpd + 1)/10)) # Log of quaffle point differential  
    b.newrating = b.newprovelo / (b.placematches + 1)
    
  }
  
  print(paste(a.newrating, a.newprovelo, b.newrating, b.newprovelo))
  return(c(a.newrating, a.newprovelo, b.newrating, b.newprovelo))
  
}
