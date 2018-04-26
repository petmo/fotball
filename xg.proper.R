require(xgboost)
require(data.table)




### Load data

# Load most recent dataset, as well as mean/variance data for all columns needed to recover identity
dt.pr <- data.table(read.csv('data/processed/full_pr_2504.csv'))
mean.var <- read.csv('data/processed/meanvar_full_pr_2504.csv',row.names = 1,header = TRUE)

# Delete X-column appearing in dt.pr because of row.names
dt.pr[,X := NULL]




### Define some help functions 
# (Currently specialized for corners and current data format)

create.one.hot <- function(dt.src) {
  # Creates one-hot encoding for the 'Div' column
  one.hot.mat <- as.matrix(model.matrix(~Div+0,dt.src))
  dt.pr <- cbind(dt.src,as.matrix(model.matrix(~Div+0,dt.src)))
  
  return(dt.src)
}

recover.identity <- function(dt.src) {
  # Recover actual number of corners by transforming and adding Y_1 and Y_2
  home.corners <- (dt.src$Y_1 * mean.var['HC','var']) + mean.var['HC','mean']
  away.corners <- (dt.src$Y_2 * mean.var['AC','var']) + mean.var['AC','mean']
  dt.src[,'C' := (home.corners + away.corners)] 
  # C indicating corners
  
  return(dt.src)
}

create.binary.target <- function(dt.src, class.limit){
  # Convert corner column to new binary row 
  dt.src[,Y := as.integer(dt.src[,C > class.limit])]
  return(dt.src)
}

create.train.test <- function(dt.src, p) {
  #Creates train and test sets of ratio p
  smp_size <- floor(p * nrow(dt.src))
  train_ind <- sample(seq_len(nrow(dt.src)), size = smp_size)
  
  train <- dt.src[train_ind, ]
  test <- dt.src[-train_ind, ]
  return(list(train=train,test=test))
}




### Do the processing 

class.limit <- 9.5 #over/under 9.5 corners

dt.pr <- recover.identity(dt.pr)
dt.pr <- create.one.hot(dt.pr)
dt.pr <- create.binary.target(dt.pr, class.limit)

train.test.list <- create.train.test(dt.pr,0.8)

train.valid <- train.test.list$train
test <- train.test.list$test

#Test will be used as unseen data
#Further partition train.valid into train and valid sets

train.valid.list <- create.train.test(train.valid,0.8)
train <- train.valid.list$train
valid <- train.valid.list$test




### Create functions for different types of training

train.xgb.classifier <- function(dt.train, dt.valid, hyperparams) {
  ### Train model as a classifier using binary column Y as target

  # Exclude these parameters from training
  exclude.list <- c('Y','Y_1','Y_2','C','Div','Date')
  
  # Convert data to xgb format (data is train parameters, label is target column)
  dtrain <- xgb.DMatrix(data = as.matrix(dt.train[,-exclude.list,with=FALSE]), label= as.matrix(dt.train[,Y]))
  dvalid <- xgb.DMatrix(data = as.matrix(dt.valid[,-exclude.list,with=FALSE]), label= as.matrix(dt.valid[,Y]))
  
  #'auc', 'error' or 'map'
  method = 'auc'
  
  # Train model
  xg <- xgb.train(params = hyperparams,
                  data = dtrain,
                  nrounds = 4000,
                  objective = "binary:logistic",
                  eval_metric = method,
                  maximize = TRUE,
                  early_stopping_rounds = 40,
                  watchlist = list(test=dvalid,train=dtrain))  
  
  return(xg)
}

train.xgb.regression <- function(dt.train, dt.valid, hyperparams) {
  ### Train model as a classifier using numeric column C as target
  
  # Exclude these parameters from training
  exclude.list <- c('Y','Y_1','Y_2','C','Div','Date')
  
  # Convert data to xgb format (data is train parameters, label is target column)
  dtrain <- xgb.DMatrix(data = as.matrix(dt.train[,-exclude.list,with=FALSE]), label= as.matrix(dt.train[,C]))
  dvalid <- xgb.DMatrix(data = as.matrix(dt.valid[,-exclude.list,with=FALSE]), label= as.matrix(dt.valid[,C]))
  
  #Train model
  xg <- xgb.train(params = hyperparams,
                  data = dtrain,
                  nrounds = 4000,
                  objective = "reg:linear",
                  eval_metric = 'rmse',
                  early_stopping_rounds = 40,
                  watchlist = list(test=dvalid,train=dtrain))  
  
  return(xg)
}




### Define hyperparams and train model object xg

# Hyperparams. should be tweaked for optimal result
tune.grid <- expand.grid(eta = c(0.002),#c(0.001587127),
                         gamma = c(0.4366591 ),
                         max_depth = c(10),#c(10),
                         min_child_weight = c(2.117832),
                         subsample = c(0.7052139),
                         colsample_bytree = c(0.9965859),
                         lambda = c(1.085726),
                         alpha = c(0.0001228216))

xg <- train.xgb.classifier(train,test,tune.grid)




### Define some test functions 
## *NOTE: below only for binary classifier

get.precision <- function(pred.vals, Y, p) {
  # Get expected precision of model output p given pred.vals
  cutoff = 0.5
  
  if (p < cutoff) {
    acc = sum(pred.vals[Y == 0] < p)/sum(pred.vals < p)
  } else {
    acc = sum(pred.vals[Y == 1] > p)/sum(pred.vals > p)
  }
  return(acc)
}

get.precision.steps <- function(pred.vals, Y, model.prob,incr = 0.05) {
  # Get expected precision of model output p within some increment around p
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
  
  # pos is the index of model.prob in the step discretization
  pos = sum(model.prob > p.vec)
  mean.acc = (acc.vec[pos] + acc.vec[pos+1])/2
  
  return(mean.acc)
}




### Predict values and compute precision for plotting
dtest <- xgb.DMatrix(data = as.matrix(test[,-c('Y','Y_1','Y_2','C','Div','Date')]), label=as.matrix(test[,Y]))

pred.vals <- predict(xg,dtest,ntreelimit = xg$best_iteration)#359)
prec = c()
prec.step = c()
p.vec = seq(0.01,1,by=0.001)


for (i in 1:length(p.vec)) {
  p = p.vec[i]
  
  prec[i] = get.precision(pred.vals, test$Y, p)
  prec.step[i] = get.precision.steps(pred.vals, test$Y, p,incr=0.01)
  i = i +1
}

plot(p.vec,prec,'l',xlim = c(0.5,0.75),ylim=c(0.35, 1),col='red')
lines(p.vec,prec.step,'l',xlim = c(0.5,0.75),ylim=c(0.35, 1),col='green')
grid()


### To come: prediction script for new matches
