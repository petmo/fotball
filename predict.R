
### NEW prediction test

fixtures <- data.table(read.csv('data/future/fixtures.csv'))

fixtures <- fixtures[fixtures$Div %in% dt.pr$Div]
fixtures <- fixtures[,c('Div','Date','HomeTeam','AwayTeam'),with=FALSE]

fixtures <- convert.and.sort.date(fixtures)
fixtures <- trim.whitespace.factors(fixtures)


dt.predict <- data.table(matrix(0,nrow = nrow(fixtures),ncol = length(all.names)))
colnames(dt.predict) = all.names

for (row in 1:nrow(fixtures)) {
  
  date <- fixtures[row]$Date
  hometeam <- fixtures[row]$HomeTeam
  awayteam <- fixtures[row]$AwayTeam
  
  
  for (i in 1:(length(data.columns))) {
    #Slow maybe because of warnings, idk why tho
    
    stat <- data.columns[i]
    
    stat.names.hf <- paste(stat,'hf.names',sep='')
    stat.names.ha <- paste(stat,'ha.names',sep='')
    stat.names.af <- paste(stat,'af.names',sep='')
    stat.names.aa <- paste(stat,'aa.names',sep='')
    
    feature.hf <- eval(parse(text=stat.names.hf))   #Home-team, for
    feature.ha <- eval(parse(text=stat.names.ha))  #Home-team, against
    
    feature.af <- eval(parse(text=stat.names.af)) #Away-team, for
    feature.aa <- eval(parse(text=stat.names.aa))   #Away-team, against
    
    
    dt.predict[row,(feature.hf) := as.list(rev(exp.discount(get.home.stat(dt, stat, hometeam,
                                                                          date, k))))]
    dt.predict[row,(feature.ha) := as.list(rev(exp.discount(get.away.stat(dt, stat, hometeam, 
                                                                          date, k))))]
    
    dt.predict[row,(feature.af) := as.list(rev(exp.discount(get.home.stat(dt, stat, awayteam,
                                                                          date, k))))]
    dt.predict[row,(feature.aa) := as.list(rev(exp.discount(get.away.stat(dt, stat, awayteam, 
                                                                          date, k))))]
  }
}


predictions <- predict(xg,xgb.DMatrix(data = as.matrix(dt.predict[,-c('Y','Div')])),xg$best_iteration)
fixtures[,'predictions' := predictions]
fixtures

impl.odds.adj <- lapply(fixtures$predictions,get.accuracy2, pred.vals = pred.vals, Y = test$Y)

fixtures[,'implied odds_adj' := 1/unlist(impl.odds.adj)]


#https://www.scoreboard.com/en/soccer/france/ligue-2/
fixtures[,'actual corners' := rev(c(NA,9,13,9,6,10,9,10,15,9,13,15))]




