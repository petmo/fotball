require(data.table)

dt <- data.table(read.csv('data/D1.csv'))



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
