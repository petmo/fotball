require(xgboost)
require(data.table)


set.seed(123)

### Load data

# Load most recent dataset, as well as mean/variance data for all columns needed to recover identity
dt.pr <- data.table(read.csv('data/processed/full_pr_3004.csv'))
mean.var <- read.csv('data/processed/meanvar_full_pr_3004.csv',row.names = 1,header = TRUE)

# Delete X-column appearing in dt.pr because of row.names
dt.pr[,X := NULL]




### Define some help functions 
# (Currently specialized for corners and current data format)

create.one.hot <- function(dt.src) {
  # Creates one-hot encoding for the 'Div' column
  one.hot.mat <- as.matrix(model.matrix(~Div+0,dt.src))
  dt.src <- cbind(dt.src,as.matrix(model.matrix(~Div+0,dt.src)))
  
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

create.binary.target <- function(dt.src, class.limit, ou = 'over'){
  # Convert corner column to new binary row 
  
  if (ou == 'over') {
    dt.src[,Y := as.integer(dt.src[,C > class.limit])] 
  } else { #ou = 'under'
    dt.src[,Y := as.integer(dt.src[,C < class.limit])] 
  }
  
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

create.equal.target <- function(dt.src) {
  # Creates equal data partition around binary target variable Y
  # Note: returns only indices for the partition
  
  sum.pos <- sum(dt.src$Y)
  sum.neg <- sum(!dt.src$Y)
  
  if (sum.pos > sum.neg) {
    
    idx <- sample(sum.pos,sum.neg)
    resample <- which(dt.src$Y == 1)[idx]
    
    return(c(resample,which(dt.src$Y == 0)))
  } else if (sum.pos <= sum.neg) {
    
    idx <- sample(sum.neg,sum.pos)
    resample <- which(dt.src$Y == 0)[idx]
    
    return(c(resample,which(dt.src$Y == 1)))
  }
}

remove.fifth.match <- function(dt.src) {
  #Deletes fifth day colunmns to reduce parameter space
  dt.red <- copy(dt.src)
  
  for (col in seq(5,160,by=5)) {
    del.col <- colnames(dt.src)[col]
    dt.red[,(del.col) := NULL]
  }
  return(dt.red)
}

sum.day.columns <- function(dt.src) {
  dt.red <- copy(dt.src[,161:182])
  
  for (col in seq(1,155,by=5)) {
    name = colnames(dt.src)[col]
    dt.red[,(name) := rowSums(dt.src[,col:(col+4)])]
  }
  
  return(dt.red)
}

### Do the processing 

class.limit <- 9.5 #over/under X corners

dt.pr <- recover.identity(dt.pr)
dt.pr <- create.one.hot(dt.pr)
dt.pr <- create.binary.target(dt.pr, class.limit, ou = 'over')
dt.pr <- remove.fifth.match(dt.pr)

dt.eq <- copy(dt.pr[create.equal.target(dt.pr)])
#dt.mini <- sum.day.columns(dt.eq)

train.test.list <- create.train.test(dt.eq,0.7)

train.valid <- train.test.list$train
test <- train.test.list$test

# Define the test set, which will be used as unseen data
dtest <- xgb.DMatrix(data = as.matrix(test[,-c('Y','Y_1','Y_2','C','Div','Date')]), label=as.matrix(test[,Y]))


#Further partition train.valid into train and valid sets

train.valid.list <- create.train.test(train.valid,0.8)
train <- train.valid.list$train
valid <- train.valid.list$test




### Create functions for different types of training

train.xgb.classifier <- function(dt.train, dt.valid, hyperparams, method = 'auc') {
  ### Train model as a classifier using binary column Y as target

  # Different sets of hyperparams
  if (hyperparams == 0) {
    # odd behaviour for this one
    hyperparams <- expand.grid(eta = c(0.006435492),
                             gamma = c(0.39018358),
                             max_depth = c(4),
                             min_child_weight = c(1.108874),
                             subsample = c(0.551886),
                             colsample_bytree = c(0.9996487),
                             lambda = c(1.181020),
                             alpha = c(0.0013763099))
  } else if (hyperparams == 1) {
    #test map = 0.543694 (over9.5)
    hyperparams <- expand.grid(eta = c(0.002),#c(0.001587127),
                             gamma = c(0.4366591),
                             max_depth = c(10),
                             min_child_weight = c(2.117832),
                             subsample = c(0.7052139),
                             colsample_bytree = c(0.9965859),
                             lambda = c(1.085726),
                             alpha = c(0.0001228216))
  } else if (hyperparams == 2) {
    #test map = 0.597688 (over 9.5)
    hyperparams <- expand.grid(eta = c(0.001289811),
                               gamma = c(0.1159276),
                               max_depth = c(3),
                               min_child_weight = c(3.237605),
                               subsample = c(0.8351582),
                               colsample_bytree = c(0.5986002),
                               lambda = c(1.049509),
                               alpha = c(0.000610730))
  } else if (hyperparams == 3) {
    #test map = 0.542727 (over9)
    hyperparams <- expand.grid(eta = c(0.016492298),
                               gamma = c(0.2536451),
                               max_depth = c(6),
                               min_child_weight = c(4.407466),
                               subsample = c(0.7891656),
                               colsample_bytree = c(0.9869861),
                               lambda = c(1.098573),
                               alpha = c(0.003344467))
  } else if (hyperparams == 4) {
    #test map = 0.563816 (over9)
    hyperparams <- expand.grid(eta = c(0.002456801),
                               gamma = c(0.1906276),
                               max_depth = c(4),
                               min_child_weight = c(1.974675),
                               subsample = c(0.8888778),
                               colsample_bytree = c(0.7311592),
                               lambda = c(1.110872),
                               alpha = c(0.006297184))
  } else if (hyperparams == 5) {
    #test map = 0.557345 (over9)
    hyperparams <- expand.grid(eta = c(0.021315668),
                               gamma = c(0.02163579),
                               max_depth = c(9),
                               min_child_weight = c(3.344209),
                               subsample = c(0.8678486),
                               colsample_bytree = c(0.9956850),
                               lambda = c(1.115719),
                               alpha = c(0.0068222679))
  } else if (hyperparams == 6) {
    #test map = 0.554661 (over9), use ntree around 900
    hyperparams <- expand.grid(eta = c(0.002378752),
                               gamma = c(0.03768980),
                               max_depth = c(8),
                               min_child_weight = c(3.499548),
                               subsample = c(0.8028222),
                               colsample_bytree = c(0.9373219),
                               lambda = c(1.120904),
                               alpha = c(0.0002876332))
  } else if (hyperparams == 7) {
    #test auc = 0.568277 (over9), use ntree around 900
    hyperparams <- expand.grid(eta = c(0.004193594),
                               gamma = c(0.20976651),
                               max_depth = c(12),
                               min_child_weight = c(1.953116),
                               subsample = c(0.8178786),
                               colsample_bytree = c(0.9801170),
                               lambda = c(1.175815),
                               alpha = c(0.001286925))
  }
  
   

  
  # Exclude these parameters from training
  exclude.list <- c('Y','Y_1','Y_2','C','Div','Date')
  
  # Convert data to xgb format (data is train parameters, label is target column)
  dtrain <- xgb.DMatrix(data = as.matrix(dt.train[,-exclude.list,with=FALSE]), label= as.matrix(dt.train[,Y]))
  dvalid <- xgb.DMatrix(data = as.matrix(dt.valid[,-exclude.list,with=FALSE]), label= as.matrix(dt.valid[,Y]))
  
  
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
                  eval_metric = 'mae',
                  early_stopping_rounds = 40,
                  watchlist = list(test=dvalid,train=dtrain))  
  
  return(xg)
}




### Define hyperparams and train model object xg


# Example grid
tune.grid <- expand.grid(eta = c(0.006435492),#c(0.001587127),
                         gamma = c(0.39018358),
                         max_depth = c(4),#c(10),
                         min_child_weight = c(1.108874),
                         subsample = c(0.551886),
                         colsample_bytree = c(0.9996487),
                         lambda = c(1.181020),
                         alpha = c(0.0013763099))


#hyperparams = 1 seems best, maybe = 6

xg <- train.xgb.classifier(train,valid,hyperparams = 1, method = 'auc')
#xg <- train.xgb.regression(train,valid,tune.grid)




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
