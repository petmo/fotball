
########################################
# Key to results data:
#
# Div = League Division
# Date = Match Date (dd/mm/yy)
# HomeTeam = Home Team
# AwayTeam = Away Team
# FTHG and HG = Full Time Home Team Goals
# FTAG and AG = Full Time Away Team Goals
# FTR and Res = Full Time Result (H=Home Win, D=Draw, A=Away Win)
# HTHG = Half Time Home Team Goals
# HTAG = Half Time Away Team Goals
# HTR = Half Time Result (H=Home Win, D=Draw, A=Away Win)
#
# Match Statistics (where available)
# Attendance = Crowd Attendance
# Referee = Match Referee
# HS = Home Team Shots
# AS = Away Team Shots
# HST = Home Team Shots on Target
# AST = Away Team Shots on Target
# HHW = Home Team Hit Woodwork
# AHW = Away Team Hit Woodwork
# HC = Home Team Corners
# AC = Away Team Corners
# HF = Home Team Fouls Committed
# AF = Away Team Fouls Committed
# HFKC = Home Team Free Kicks Conceded
# AFKC = Away Team Free Kicks Conceded
# HO = Home Team Offsides
# AO = Away Team Offsides
# HY = Home Team Yellow Cards
# AY = Away Team Yellow Cards
# HR = Home Team Red Cards
# AR = Away Team Red Cards
# HBP = Home Team Bookings Points (10 = yellow, 25 = red)
# ABP = Away Team Bookings Points (10 = yellow, 25 = red)
#
########################################


require(data.table)
require(stringi)

dt <- data.table(read.csv('data/D1.csv'))
all.stats <- colnames(dt) # List of all available stats

convert.and.sort.date <- function(dt) {
  ## Convert Date column to Date datatype and sort by date 
  
  dt$Date <- as.Date(dt$Date,"%d/%m/%y")
  dt <- dt[order(Date),]
  
  return(dt)
}
get.teams <- function(dt) {
  ## Return vector of team names
  return(levels(dt$HomeTeam))
}


get.home.stat <- function(dt,stat,team,date,k) {
  ## Return vector of k last stat for home games before date
  
  if (!(stat %in% colnames(dt))) {
    sprintf('Stat %s not in feature names', stat)
    return(NULL)
  }
  stat.vec <- tail(dt[dt$Date < date,][HomeTeam == team,..stat],k)
  return(as.numeric(unlist(stat.vec)))
}


get.away.stat <- function(dt,stat,team,date,k) {
  ## Return vector of k last stat for away games before date
  
  if (!(stat %in% colnames(dt))) {
    sprintf('Stat %s not in feature names', stat)
    return(NULL)
  }
  stat.vec <- tail(dt[dt$Date < date,][AwayTeam == team,..stat],k)
  return(as.numeric(unlist(stat.vec)))
}


get.full.stat <- function(dt,stat,team,date,k) {
  ## Return vector of k last stat for home and away games
  ## Note: Home/Away stats will be treated equally
 
  if (!(stat %in% colnames(dt))) {
    cat(sprintf('Stat %s not in feature names\n', stat))
    return(NULL)
  }
  
  home.stat <- stat
  away.stat <- stat
  
  if (stat == 'FTHG' | stat == 'FTAG' | stat == 'HTHG' | stat == 'HTAG') {
    stri_sub(home.stat,from=c(3),len=1) <- 'H'
    stri_sub(away.stat,from=c(3),len=1) <- 'A'
  } else {
    # Any other stat (must start with H or A)
    stri_sub(home.stat,from=c(1),len=1) <- 'H'
    stri_sub(away.stat,from=c(1),len=1) <- 'A'
  }
  
  
  matches <- tail(dt[dt$Date < date,][HomeTeam == team | AwayTeam == team,],k)
  
  # Need to do some tricks to maintain date order
  stat.vec <- rbindlist(list(matches[HomeTeam == team,c('Date',home.stat),with=FALSE],
                             matches[AwayTeam == team,c('Date',away.stat),with=FALSE]))
  stat.vec <- stat.vec[order(Date)]
  
  # Second column will be stat of interest
  return(stat.vec[[2]])
}


get.prev.HG <- function(dt,team,date,k) {
  ## Return vector of goals for in last k home games before date (format "2018-04-08")
  
  goals <- tail(dt[dt$Date < date,][HomeTeam == team,FTHG],k)
  return(goals)
}


get.prev.AG <- function(dt,team,date,k) {
  ## Return vector of goals for in last k away games before date (format "2018-04-08")
  
  goals <- tail(dt[dt$Date < date,][AwayTeam == team,FTAG],k)
  return(goals)
}


get.prev.goals <- function(dt,team,date,k) {
  ## Return vector of goals for last k games before date
  
  matches <- tail(dt[dt$Date < date,][HomeTeam == team | AwayTeam == team,],k)
  goals <- c(matches[HomeTeam == team, FTHG], matches[AwayTeam == team, FTAG])
  
  return(goals)
}




test.date <- "2017-01-01"
team <- teams[2]
