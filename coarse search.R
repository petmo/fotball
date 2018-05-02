

### Get the datasets and function by running xg.proper, up until training starts


### COARSE SEARCH
runs <- 1000
grid.df <- cbind(data.frame('best.map' = 0),tune.grid)

for (i in 1:runs) {
  print('RUN:')
  print(i)
  
  tune.grid <- expand.grid(eta = 10^runif(1, -3, -1),
                           gamma = runif(1, 0.0, 0.5),
                           max_depth = round(runif(1,4,12)),
                           min_child_weight = runif(1, 1, 5),
                           subsample = runif(1, 0.5, 1),
                           colsample_bytree = runif(1, 0.5, 1),
                           lambda = runif(1, 1, 1.2),
                           alpha = 10^runif(1,0-4,-2))
  xg = NULL
  
  xg <- train.xgb.classifier(train,test,hyperparams = tune.grid, method = 'auc')
  
  best.map <- max(xg$evaluation_log$test_auc)
  
  print('map:')
  print(best.map)
  
  grid.df[i,] <- c(best.map, tune.grid)
  
}


