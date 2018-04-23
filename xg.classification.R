require(xgboost)
require(data.table)

dt.pr <- data.table(read.csv('data/processed/data_5_corners.csv'))



class.limit = 14

dt.pr[,Y2 := as.integer(dt.pr[,Y > class.limit])]

sample.idx <- sample(nrow(dt.pr[dt.pr[,Y2 == 0],]),nrow(dt.pr[dt.pr[,Y2==1]]))

dt.true <- dt.pr[sample.idx,]

dt.pr <- rbind(dt.true,dt.pr[dt.pr[,Y2 == 1]])

p = 0.75 #Train/test proportion

smp_size <- floor(p * nrow(dt.pr))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(dt.pr)), size = smp_size)

train <- dt.pr[train_ind, ]
test <- dt.pr[-train_ind, ]


dtrain <- xgb.DMatrix(data = as.matrix(train[,-c('Y','Y2','Div')]), label= as.matrix(train[,Y2]))
dtest <- xgb.DMatrix(data = as.matrix(test[,-c('Y','Y2','Div')]), label=as.matrix(test[,Y2]))

watchlist <- list(test=dtest,train=dtrain)    



tune.grid <- expand.grid(eta = c(0.01),
                         gamma = c(0.1),
                         max_depth = c(4),
                         min_child_weight = c(1.0),
                         subsample = c(0.8),
                         colsample_bytree = c(0.6),
                         lambda = c(1.2),
                         alpha = c(0.0001))

xg <- xgb.train(params = tune.grid,
                data = dtrain,
                nrounds = 2500,
                objective = "binary:logistic",
                eval_metric = 'auc',
                maximize = TRUE,
                early_stopping_rounds = 40,
                watchlist = watchlist,
                print_every_n = 1)  




# TESTING ::
ntest = 25

test.idx <- sample(dim(test)[1],ntest)
pred.vals <- predict(xg,xgb.DMatrix(data = as.matrix(test[test.idx,-c('Y','-Y2','Div')])))#,ntreelimit = 2500)#xg$best_iteration)
#actual.vals <- test[test.idx,'Y']

for (i in 1:ntest) {
  cat(sprintf('pred: %f, actual: %d\n',pred.vals[i],test[test.idx[i],Y]))
}




### Predict test


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


