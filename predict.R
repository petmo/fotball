require(plyr)

### NEW prediction test

# Prepare the prediciton data
fixtures <- data.table(read.csv('data/future/fixtures_2704.csv'))

fixtures <- fixtures[fixtures$Div %in% dt.pr$Div]
fixtures <- fixtures[,c('Div','Date','HomeTeam','AwayTeam'),with=FALSE]

fixtures <- convert.and.sort.date(fixtures)
fixtures <- trim.whitespace.factors(fixtures)


#hometeams <- c('Aston Villa','Barnsley','Burton','Hull','Norwich',"Nott'm Forest",'QPR')
#awayteams <- c('Derby','Brentford','Bolton','Cardiff','Leeds','Bristol City','Birmingham')
#div = rep('E1',length(hometeams))
#fixtures <- data.table(HomeTeam = hometeams, AwayTeam = awayteams, Div = div, Date = date)

dt.predict <- data.table(matrix(0,nrow = nrow()))

# Convert prediction data to our model
dt.predict <- data.table(matrix(0,nrow = nrow(fixtures),ncol = length(all.names)))
colnames(dt.predict) = all.names

dt.predict$Div <- as.factor(dt.predict$Div)
levels(dt.predict$Div) <- levels(dt$Div)

for (row in 1:nrow(fixtures)) {
  print(row)
  
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
  
  dt.predict[row, 'Div'] = fixtures[row]$Div
  dt.predict[row, 'Date'] = date
}


dt.predict <- create.one.hot(dt.predict)

dpred <- as.matrix(dt.predict[,-c('Div','Date')])

#Load our models
#xg.over <- xgb.load('models/xg_MAT_over95_064.model')
#xg.under <- xgb.load('models/xg_MAT_under95_061.model')

xg.u10 <- xgb.load('models/xg_MAP_U105_058.model')
xg.o10 <- xgb.load('models/xg_MAP_O105_052.model')
xg.ou10 <- xgb.load('models/xg_AUC_OU105_057.model')

pred.u10 <- predict(xg.u10, dpred, ntreelimit = 500) 
pred.o10 <- predict(xg.o10, dpred, ntreelimit = 500)
pred.ou10 <- predict(xg.ou10, dpred)
pred.uo10 <- 1-pred.ou10


xg.u9 <- xgb.load('models/xg_MAP_U95_049.model')
xg.o9 <- xgb.load('models/xg_MAP_O95_064.model')
xg.ou9 <- xgb.load('models/xg_AUC_OU95_057.model')

pred.u9 <- predict(xg.u9, dpred, ntreelimit = 900) 
pred.o9 <- predict(xg.o9, dpred, ntreelimit = 1000)
pred.ou9 <- predict(xg.ou9, dpred, ntreelimit = 1250)
pred.uo9 <- 1-pred.ou9


fixtures[,'10.o' := pred.o10]
fixtures[,'10.ou' := pred.ou10]
fixtures[,'10.u' := pred.u10]
fixtures[,'10.uo' := pred.uo10]

fixtures[,'9.o' := pred.o9]
fixtures[,'9.ou' := pred.ou9]
fixtures[,'9.u' := pred.u9]
fixtures[,'9.uo' := pred.uo9]


fixtures[,-c('Date')]

find.fixtures <- function(fx,lim) {
  # Finds fixtures based on 
  
  # Find closest l to lim to compute precision
  l.vals <- c(0.5,0.55,0.6,0.65)
  l <- l.vals[which.min(abs(l.vals-lim))]
  
  # Implied odds lookup
  prec.df = data.frame('l' = c(0.5,0.55,0.6,0.65), 
                    '10.o' = c(0.5548246, 0.5980392, 0.5980392, 0.5980392),
                    '10.u' = c(0.5529379, 0.6433475, 0.6969697, 0.6969697),
                    '9.o' = c(0.5918117, 0.6112790, 0.6326120, 0.6624041),
                    '9.u' = c(0.5492611, 0.6129032,0.6129032,0.6129032))

  fx.lim <- data.table(data.frame())
  i = 3 #stupid count number used to get odds from prec.df 
  for (col in seq(5,11,by=2)) {
    boolvec <- as.vector(mapply(function(a,b,l){(a > l & b > l)}, fx[,col,with=F], fx[,(col+1),with=F],l = l))
    
    if (sum(boolvec) > 0) {
      odds <- 1/prec.df[prec.df$l == l, col - i]
      
      fx.lim <- rbind.fill(fx.lim, cbind(fx[boolvec,c(1,3,4,col,col+1),with=F], odds))
    }
    i = i +1
  }
   
  return(fx.lim)
}


get.odds <- function(l,colnr) {
  l.vec <- c(0.5,0.55,0.6,0.65)
  l.adj <- l.vec[sum(l >= l.vec)]
  
  prec.df = data.frame('l' = l.vec, 
                       '10.o' = c(0.5548246, 0.5980392, 0.5980392, 0.5980392),
                       '10.u' = c(0.5529379, 0.6433475, 0.6969697, 0.6969697),
                       '9.o' = c(0.5918117, 0.6112790, 0.6326120, 0.6624041),
                       '9.u' = c(0.5492611, 0.6129032,0.6129032,0.6129032))   
  return(1/prec.df[prec.df$l == l.adj,colnr])
}

find.fixtures.odds <- function(fx,odds) {
  # Finds fixtures returning odds smaller than 'odds'
  
  lim <- 1/odds
  l.vals <- c(0.5,0.55,0.6,0.65)
  
  # Closest lims for odds for different cases
  lim.vec = c(l.vals[which((c(0.5548246, 0.5980392, 0.5980392, 0.5980392) > lim))[1]],
               l.vals[which((c(0.5529379, 0.6433475, 0.6969697, 0.6969697) > lim))[1]],
               l.vals[which((c(0.5918117, 0.6112790, 0.6326120, 0.6624041) > lim))[1]],
               l.vals[which((c(0.5492611, 0.6129032, 0.6129032, 0.6129032) > lim))[1]])
  lim.vec[is.na(lim.vec)] = 1
    
  # Implied odds lookup
  prec.df = data.frame('l' = c(0.5,0.55,0.6,0.65), 
                       '10.o' = c(0.5548246, 0.5980392, 0.5980392, 0.5980392),
                       '10.u' = c(0.5529379, 0.6433475, 0.6969697, 0.6969697),
                       '9.o' = c(0.5918117, 0.6112790, 0.6326120, 0.6624041),
                       '9.u' = c(0.5492611, 0.6129032,0.6129032,0.6129032))           


  fx.lim <- data.table(data.frame())
  i = 3 #stupid count number used to get odds from prec.df 
  for (col in seq(5,11,by=2)) {
    boolvec <- as.vector(mapply(function(a,b,l){(a > l & b > l)}, fx[,col,with=F], fx[,(col+1),with=F],l = lim.vec[col-(i+1)]))
    
    if (sum(boolvec) > 0) {
      odds <- get.odds(lim.vec[col-(i+1)],col-i)
      
      fx.lim <- rbind.fill(fx.lim, cbind(fx[boolvec,c(1,3,4,col,col+1),with=F], odds))
    }
    i = i +1
  }
  
  return(fx.lim)
}

odds1.6 <- find.fixtures.odds(fixtures,1.6)

write.csv(odds1.6,'data/predict/predict_2804_odds16.csv')
write.csv(odds1.65.o,'data/predict/predict_2804_odds165.csv')



