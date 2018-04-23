require(xgboost)


dt.pr <- data.table(read.csv('data/processed/data_pr1_corners.csv'))


p = 0.75 #Train/test proportion

smp_size <- floor(p * nrow(dt.pr))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(dt.pr)), size = smp_size)

train <- dt.pr[train_ind, ]
test <- dt.pr[-train_ind, ]



dtrain <- xgb.DMatrix(data = as.matrix(train[,-'Y']), label= as.matrix(train[,Y]))
dtest <- xgb.DMatrix(data = as.matrix(test[,-'Y']), label=as.matrix(test[,Y]))


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
                nrounds = 1000,
                objective = "reg:linear",
                eval_metric = 'rmse',
                early_stopping_rounds = 40,
                watchlist = watchlist,
                print_every_n = 1)  


# TESTING ::
ntest = 25

test.idx <- sample(dim(test)[1],ntest)
pred.vals <- predict(xg,xgb.DMatrix(data = as.matrix(test[test.idx,-'Y'])),ntreelimit = 800)#xg$best_iteration)

rmse = 0
for (i in 1:length(test.idx)) {
  rmse = rmse + sqrt((pred.vals[i] - test[test.idx[i],Y])^2)
  cat(sprintf('pred: %f, actual: %d\n',pred.vals[i],test[test.idx[i],Y]))
}
rmse = rmse/ntest
rmse;




### Do random parameter search (takes a long time)



### COARSE SEARCH
runs <- 1000
grid.df <- cbind(data.frame('best.rmse' = 1000),tune.grid)

for (i in 1:runs) {
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
                  nrounds = 1000,
                  objective = "reg:linear",
                  eval_metric = 'rmse',
                  early_stopping_rounds = 40,
                  watchlist = watchlist,
                  print_every_n = 1)  
  
  best.rmse <- min(xg$evaluation_log$test_rmse)
  
  print('rmse:')
  print(best.rmse)
  
  
  grid.df[i,] <- c(best.rmse, tune.grid)
  
}


min(grid.df$best.rmse)
### For conrers::


#best.rmse             eta            gamma 
#1.7194140000     0.0185450500     0.2404875790 
#max_depth   min_child_weight        subsample 
#9.0000000000     3.4792815251     0.9823138125 
#colsample_bytree     lambda            alpha 
#0.5007197297     1.0902941165     0.0006845954 

tune.grid <- expand.grid(eta = c(0.0185450500),
                         gamma = c(0.2404875790),
                         max_depth = c(9),
                         min_child_weight = c(3.4792815251),
                         subsample = c(0.9823138125),
                         colsample_bytree = c(0.5007197297),
                         lambda = c(1.0902941165),
                         alpha = c(0.0006845954))

#or:

tune.grid <- expand.grid(eta = c(0.04218549),
                         gamma = c(0.1295857),
                         max_depth = c(5),
                         min_child_weight = c(4.841194),
                         subsample = c(0.5411639),
                         colsample_bytree = c(0.9747233),
                         lambda = c(1.131214),
                         alpha = c(0.0059633653))



#### FOR GOALS::
## Optimal:
## rmse 1.687107,
# eta         gamma             max_depth min_child_weight
# 0.01318533  0.09783051        8         4.083859
# subsample   colsample_bytree  lambda    alpha
# 0.6269748   0.9692856         1.070889  0.0001572666
tune.grid <- expand.grid(eta = c(0.001318533),
                         gamma = c(0.09783051),
                         max_depth = c(8),
                         min_child_weight = c(4.083859),
                         subsample = c(0.6269748),
                         colsample_bytree = c(0.9692856),
                         lambda = c(1.070889),
                         alpha = c(0.0001572666))



