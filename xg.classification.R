require(xgboost)
require(data.table)

dt.pr <- data.table(read.csv('data/processed/data_5_corners_meanvar.csv'))

mean.var <- read.csv('data/processed/meanvar_5_corners_meanvar.csv',row.names = 1,header = TRUE)
dt.pr[,X := NULL]

#dt.pr <- data.table(read.csv('data/processed/data_5_corners.csv'))
#dt.pr[,Y := as.integer(dt.pr[,Y > class.limit])]

create.one.hot <- function(dt.pr) {
  
  one.hot.mat <- as.matrix(model.matrix(~Div+0,dt.pr))
  dt.pr <- cbind(dt.pr,as.matrix(model.matrix(~Div+0,dt.pr)))
  
  return(dt.pr)
}


recover.identity <- function(dt.pr) {
  # Recover identity for corner stat
  
  home.corners <- (dt.pr$Y_1 * mean.var['HC','var']) + mean.var['HC','mean']
  away.corners <- (dt.pr$Y_2 * mean.var['AC','var']) + mean.var['AC','mean']
  
  dt.pr[,'corners' := (home.corners + away.corners)]
}


dt.pr <- recover.identity(dt.pr)
dt.pr <- create.one.hot(dt.pr)

class.limit <- 9.5
dt.pr[,Y := as.integer(dt.pr[,corners > class.limit])]

# Adjust sample size (check with other computer for best function)
#sample.idx <- sample(nrow(dt.pr[dt.pr[,Y2 == 0],]),nrow(dt.pr[dt.pr[,Y2==1]]))
#dt.true <- dt.pr[sample.idx,]
#dt.pr <- rbind(dt.true,dt.pr[dt.pr[,Y2 == 1]])




p = 0.8 #Train/test proportion

smp_size <- floor(p * nrow(dt.pr))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(dt.pr)), size = smp_size)

train <- dt.pr[train_ind, ]
test <- dt.pr[-train_ind, ]

# New (meanvar) data
dtrain <- xgb.DMatrix(data = as.matrix(train[,-c('Y','Y_1','Y_2','corners','Div','Date')]), label= as.matrix(train[,Y]))
dtest <- xgb.DMatrix(data = as.matrix(test[,-c('Y','Y_1','Y_2','corners','Div','Date')]), label=as.matrix(test[,Y]))

# Old data
#dtrain <- xgb.DMatrix(data = as.matrix(train[,-c('Y','Div','div')]), label= as.matrix(train[,Y]))
#dtrain <- xgb.DMatrix(data = as.matrix(test[,-c('Y','Div','div')]), label= as.matrix(test[,Y]))

watchlist <- list(test=dtest,train=dtrain)    



### COARSE SEARCH
runs <- 1000
grid.df <- cbind(data.frame('best.auc' = 0),tune.grid)

for (i in 81:runs) {
  print('RUN:')
  print(i)
  
  tune.grid <- expand.grid(eta = 10^runif(1, -3, -1),
                           gamma = runif(1, 0.0, 0.5),
                           max_depth = round(runif(1,3,10)),
                           min_child_weight = runif(1, 1, 5),
                           subsample = runif(1, 0.5, 1),
                           colsample_bytree = runif(1, 0.5, 1),
                           lambda = runif(1, 1, 1.2),
                           alpha = 10^runif(1,0-4,-2))
  xg = NULL
  
  xg <- xgb.train(params = tune.grid,
                  data = dtrain,
                  nrounds = 4000,
                  objective = "binary:logistic",
                  eval_metric = 'auc',
                  maximize = TRUE,
                  early_stopping_rounds = 40,
                  watchlist = watchlist,
                  print_every_n = 1)  
  
  best.auc <- max(xg$evaluation_log$test_auc)
  
  print('auc:')
  print(best.auc)
  
  
  grid.df[i,] <- c(best.auc, tune.grid)
  
}


min(grid.df$best.rmse)



##alt:
#best.auc        eta      gamma max_depth min_child_weight
#78 0.560696 0.01260506 0.05350153         7         1.618353
#subsample colsample_bytree   lambda       alpha
#78 0.5901354        0.9967475 1.144454 0.003549758

### ORDINARY TRAIN
tune.grid <- expand.grid(eta = c(0.002),#c(0.001587127),
                         gamma = c(0.4366591 ),
                         max_depth = c(10),#c(10),
                         min_child_weight = c(2.117832),
                         subsample = c(0.7052139),
                         colsample_bytree = c(0.9965859),
                         lambda = c(1.085726),
                         alpha = c(0.0001228216))

tune.grid <- expand.grid(eta = c(0.01),
                         gamma = c(0.5),
                         max_depth = c(10),
                         min_child_weight = c(3.0),
                         subsample = c(0.7),
                         colsample_bytree = c(0.6),
                         lambda = c(1.5),
                         alpha = c(0.0001))

xg <- xgb.train(params = tune.grid,
                data = dtrain,
                nrounds = 4000,
                objective = "binary:logistic",
                eval_metric = 'auc',
                maximize = TRUE,
                #eval_metric = 'error@0.5',
                early_stopping_rounds = 40,
                watchlist = watchlist,
                print_every_n = 1)  


## ANALYSIS

importance <- xgb.importance(model = xg, feature_names = colnames(train[,-c('Y','Y_1','Y_2','C','Div','Date')]))
xgb.plot.importance(importance,top_n = 10)
# HSTaa5 is weighted heaviest. This is problematic. xgboost doesnt care about scaling,
# so time discounting does nothing. 
# suggested fixes: different model?, use a discounted sum instead of multiple columns for each parameter


# TESTING ::
ntest = 25

test.idx <- sample(dim(test)[1],ntest)
pred.vals <- predict(xg,xgb.DMatrix(data = as.matrix(test[test.idx,-c('Y','-Y2','Div')])))#,ntreelimit = 2500)#xg$best_iteration)
#actual.vals <- test[test.idx,'Y']

for (i in 1:ntest) {
  cat(sprintf('pred: %f, actual: %d\n',pred.vals[i],test[test.idx[i],Y]))
}



get.accuracy2 <- function(pred.vals, Y, model.prob,incr = 0.05) {
  
  i = 1
  acc.vec = c()
  p.vec = seq(0,(1-incr),by=incr)
  for (p in p.vec) {
    if (p < 0.5) {
      acc = sum(p < pred.vals[Y==0] & pred.vals[Y==0] < p+incr)/ sum(p < pred.vals & pred.vals < p+incr)
    }
    else{
      acc = sum(p < pred.vals[Y==1] & pred.vals[Y==1] < p+incr)/ sum(p < pred.vals & pred.vals < p+incr)
    }
    if (acc == 'NaN'){
      acc.vec[i] = 0
    } else{
      acc.vec[i] = acc
    } 
    i = i+1
  }
  
  pos = sum(model.prob > p.vec)
  mean.acc = (acc.vec[pos] + acc.vec[pos+1])/2
  
  return(mean.acc)
}


get.accuracy <- function(pred.vals, Y, p) {
  # Get expected accuracy of model output p given pred.vals
  # Note: this works as an (empricial) odds converter
  cutoff = 0.5
  
  if (p < cutoff) {
    acc = sum(pred.vals[Y == 0] < p)/sum(pred.vals < p)
  } else {
    acc = sum(pred.vals[Y == 1] > p)/sum(pred.vals > p)
  }
  return(acc)
}

pred.vals <- predict(xg,dtest)#,ntreelimit = 996)
acc = c()
acc2 = c()
p.vec = seq(0.01,1,by=0.001)


for (i in 1:length(p.vec)) {
  p = p.vec[i]
    
  acc[i] = get.accuracy(pred.vals, test$Y, p)
  acc2[i] = get.accuracy2(pred.vals, test$Y, p,incr=0.025)
  i = i +1
}

plot(p.vec,acc,'l',xlim = c(0.25,0.75),ylim=c(0.35, 1),col='red')
lines(p.vec,acc2,'l',xlim = c(0.25,0.75),ylim=c(0.35, 1),col='green')


### NEW prediction test

date <- as.Date("2018-04-22")

hometeams <- as.factor(c('Derby',"Nott'm Forest",'Fulham','Aston Villa','Barnsley','Burton'))
awayteams <- as.factor(c('Cardiff','Barnsley','Sunderland','Derby','Brentford','Bolton'))
division = 'E1'

predict.matches <- data.table(HomeTeam = hometeams, AwayTeam = awayteams, Date = as.Date(date))


predict.matches[,(levels(dt.pr$Div)) := list(rep(0.0,length(levels(dt.pr$Div))))]
predict.matches[,division] = rep(1,nrow(predict.matches))

dt.predict <- data.table(matrix(0,nrow = nrow(predict.matches),ncol = length(all.names)))
colnames(dt.predict) = all.names

for (row in 1:nrow(predict.matches)) {
  
  date <- predict.matches[row]$Date
  hometeam <- predict.matches[row]$HomeTeam
  awayteam <- predict.matches[row]$AwayTeam
  
  
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
predict.matches <- data.table(predict.matches)
predict.matches[,'predictions' := predictions]
predict.matches






###Predict test


hometeam <- 'Hertha Berlin'
awayteam <- 'Augsburg'
date <- as.Date("2018-04-22")



hometeam <- "Schalke 04"
awayteam <- "M'gladbach"
#0.5538096


hometeam <- 'Wolfsburg'
awayteam <- 'Hamburg'
#0.245

hometeam <- 'Leverkusen'
awayteam <- "Stuttgart" 
#0.2589625

hometeam <-  "Bayern Munich"
awayteam <- "Ein Frankfurt"
#0.08177751

hometeam <- 'Hoffenheim'
awayteam <- 'Hannover'
#0.4220825

hometeam <- 'Mainz'
awayteam <- "RB Leipzig"
#0.3448883

hometeam <- "Werder Bremen" 
awayteam <- "Dortmund"
#0.6676579




dt.pr <- data.table(matrix(0,nrow = 1,ncol = length(all.names)))
colnames(dt.pr) = all.names

for (i in 1:(length(data.columns)-1)) {
  home.stat <- data.columns.home[i]
  
  stat <- data.columns[i]
  stat.names.f <- paste(stat,'f.names',sep='') 
  stat.names.a <- paste(stat,'a.names',sep='')
  feature.f <- eval(parse(text=stat.names.f)) 
  feature.a <- eval(parse(text=stat.names.a))
  
  dt.pr[row,(feature.f) := as.list(exp.smoothing(rev(get.full.stats(dt,home.stat, hometeam,
                                                                    date,k,for.against = 1)), alpha))]
  dt.pr[row,(feature.a) := as.list(exp.smoothing(rev(get.full.stats(dt,home.stat, hometeam,
                                                                    date,k,for.against = 0)), alpha))]
  
  i = i + 1
  
  stat <- data.columns[i]
  stat.names.f <- paste(stat,'f.names',sep='') 
  stat.names.a <- paste(stat,'a.names',sep='')
  feature.f <- eval(parse(text=stat.names.f)) 
  feature.a <- eval(parse(text=stat.names.a))
  
  dt.pr[row,(feature.f) := as.list(exp.smoothing(rev(get.full.stats(dt,home.stat, awayteam,
                                                                    date,k,for.against = 1)), alpha))]
  dt.pr[row,(feature.a) := as.list(exp.smoothing(rev(get.full.stats(dt,home.stat, awayteam,
                                                                    date,k,for.against = 0)), alpha))]
  
}


prediction <- predict(xg,xgb.DMatrix(data = as.matrix(dt.pr[,-c('Y','div')])))
prediction






#### TEST REGRESSION ACCURACY
dtest <- xgb.DMatrix(data = as.matrix(test[,-c('Y','Y_1','Y_2','C','Div','Date')]), label=as.matrix(test[,Y]))

pred.vals <- predict(xg,dtest,ntreelimit = xg$best_iteration)



