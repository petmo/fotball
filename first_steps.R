
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

#dt <- data.table(read.csv('data/BL1_BL2_2010-2017.csv'))
#all.stats <- colnames(dt) # List of all available stats


convert.and.sort.date <- function(dt) {
  ## Convert Date column to Date datatype and sort by date 
  
  dt$Date <- as.Date(dt$Date,"%d/%m/%y")
  dt <- dt[order(dt$Date),]
  
  return(dt)
}

trim.whitespace.factors <- function(dt) {
  ## Trims trailing whitespace of factors causing double-levels
  
  dt[] = lapply(dt, function(x) if (is.factor(x)) factor(sub(" +$", "", x)) else x)
  return(dt)
}

compute.mean.var <- function(dt) {
  # Store mean and variance for future transformations

  mean.var.dt <- data.frame(mean = numeric(0),var = numeric(0))  
  
  for (col in colnames(dt)) {
    if (is.numeric(dt[[col]])) {
    mean.var.dt[col,] = c(mean(dt[[col]]), sd(dt[[col]]))
    }
  }
  
  return(mean.var.dt)
}


zero.mean.variance <- function(dt, unit.var = 1) {
  # Set mean to zero and, if unit.var =1, var to 1 (of numeric columns)
  
  for (col in colnames(dt)) {
    if (is.numeric(dt[[col]])) {
      dt[[col]] <- dt[[col]] - mean(dt[[col]])
      
      if (unit.var == 1) {
        dt[[col]] <- dt[[col]]/sd(dt[[col]])
      }
    }
  }
  return(dt)
}

exp.discount <- function(dt, alpha = 0.0025) {
  ## Exponentially discount first column in dt based on second column as time

  return(unlist(dt[,1] * exp(dt[,2] * alpha)))
}


get.teams <- function(dt) {
  ## Return vector of team names
  return(levels(dt$HomeTeam))
}


get.home.stat <- function(dt,stat,team,date,k) {
  ## Return vector of k last stat for home games before date
  
  if (!(stat %in% colnames(dt))) {
    cat(sprintf('Stat %s not in feature names', stat))
    return(NULL)
  }

  #if (for.against == 0) {
  #  # Indicating we're looking for stats AGAINST team (which is hometeam)
  #  # Replace letter in stat with A
  #  if (stat == 'FTHG' | stat == 'FTAG' | stat == 'HTHG' | stat == 'HTAG') {
  #    # These stats have H or A in position 3
  #    stri_sub(stat,from=c(3),len=1) <- 'A'
  #  } else {
  #    # Any other stat of interest starts with H or A
  #    stri_sub(stat,from=c(1),len=1) <- 'A'
  #  }
  #}

  stat.vec <- tail(dt[dt$Date < date,][HomeTeam == team,c(stat,'Date'),with=FALSE],k)
  stat.vec[,'time'] <- as.integer(stat.vec$Date - date)
  stat.vec[,Date := NULL]
    
  if (nrow(stat.vec) < k) {
    return(data.frame(matrix(NA,k,2)))
  } else {
    return(stat.vec)
  }
}

get.away.stat <- function(dt,stat,team,date,k) {
  ## Return vector of k last stat for away games before date
  
  if (!(stat %in% colnames(dt))) {
    cat(sprintf('Stat %s not in feature names', stat))
    return(NULL)
  }
  
  #if (for.against == 0) {
  #  # Indicating we're looking for stats AGAINST team (which is hometeam)
  #  # Replace letter in stat with A
  #  if (stat == 'FTHG' | stat == 'FTAG' | stat == 'HTHG' | stat == 'HTAG') {
  #    # These stats have H or A in position 3
  #    stri_sub(stat,from=c(3),len=1) <- 'A'
  #  } else {
  #    # Any other stat of interest starts with H or A
  #    stri_sub(stat,from=c(1),len=1) <- 'A'
  #  }
  #}
  
  stat.vec <- tail(dt[dt$Date < date,][AwayTeam == team,c(stat,'Date'),with=FALSE],k)
  stat.vec[,'time'] <- as.integer(stat.vec$Date - date)
  stat.vec[,Date := NULL]
  
  if (nrow(stat.vec) < k) {
    return(data.frame(matrix(NA,k,2)))
  } else {
    return(stat.vec)
  }
}


get.WLD <- function(dt,WLD,team,date) {
  # WLD = {'W','L','D'}
  
  # NOT DONE. Need to be a bit smart here.
  
  matches <- tail(dt[dt$Date < date,][HomeTeam == team | AwayTeam == team,],k)
  
  
  
  
}


get.full.stats <- function(match.dt,stat,team,date,k,for.against) {

  ## Return vector of k last stat for home and away games
  ## Stat should be home version of stat
  
  ## for.against indicates if its stats for (1) or against/conceded (0)
  if (for.against == 1) {
    a <- 'H'
    b <- 'A'
  } else {
    a <- 'A'
    b <- 'H'
  }
 
  if (!(stat %in% colnames(match.dt))) {
    cat(sprintf('Stat %s not in feature names\n', stat))
    return(NULL)
  }

  home.stat <- stat
  away.stat <- stat
  
  # Replace letter in stat to give correct correspondance
  if (stat == 'FTHG' | stat == 'FTAG' | stat == 'HTHG' | stat == 'HTAG') {
    # These stats have H or A in position 3
    stri_sub(home.stat,from=c(3),len=1) <- a
    stri_sub(away.stat,from=c(3),len=1) <- b
  } else {
    # Any other stat of interest starts with H or A
    stri_sub(home.stat,from=c(1),len=1) <- a
    stri_sub(away.stat,from=c(1),len=1) <- b
  }
  
  matches <- tail(match.dt[match.dt$Date < date,][HomeTeam == team | AwayTeam == team,],k)
  
  # Need to do some tricks to maintain date order
  stat.vec <- rbindlist(list(matches[HomeTeam == team,c('Date',home.stat),with=FALSE],
                             matches[AwayTeam == team,c('Date',away.stat),with=FALSE]))
  stat.vec <- stat.vec[order(Date)][[2]]
  
  if (length(stat.vec) < k) {
    return(as.numeric(rep(NA,k)))
  } else {
    return(stat.vec)
  }
}











get.matchup.home.stat <- function(dt,stat,hometeam,awayteam,date,k) {
  
  ## Not done 
  
  return(get.home.stat(dt[HomeTeam == hometeam & AwayTeam == awayteam,],stat,hometeam,date,k))
}


exp.smoothing <- function(x,alpha) {
  ## Exponential smoothing for vector x (assumes recent first sorting)
  ## Smoothes
  
  s = rep(0,length(x))
  s[1] = x[1]
  
  if (length(x) > 1) {
    for (i in 2:length(x)) {
      s[i] = alpha*x[i] + (1-alpha)*s[i-1]
    }
  }
  return(s)
}



### Below functions are useless, just use get.stat functions

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





#test.date <- "2017-01-01"
#team <- teams[2]





