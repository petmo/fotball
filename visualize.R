require(ggplot2)

### Using functions defined in xg.proper.R
## Should probably do a source file for these



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

get.precision.count.steps <- function(pred.vals, Y, incr = 0.01) {
  # Get expected precision of model output p within some increment around p
  i = 1
  acc.vec = c()
  count.vec = c()
  
  p.vec = seq(0,(1-incr),by=incr)
  for (p in p.vec) {
    if (p < 0.5) {
      count <- sum(p < pred.vals & pred.vals < p+incr) 
      acc = sum(p < pred.vals[Y==0] & pred.vals[Y==0] < p+incr)/ count
    }
    else{ #p > 0.5
      count <- sum(p < pred.vals & pred.vals < p+incr)
      acc = sum(p < pred.vals[Y==1] & pred.vals[Y==1] < p+incr)/ count
    }
    if (count == 0){
      acc.vec[i] = 0
      count = 0
    } else{
      acc.vec[i] = acc
    } 
    count.vec[i] = count
    i = i+1
  }
  
  return(list(prec = acc.vec, count = count.vec, p = p.vec))
}


get.prec.continous <- function(pred.vals, Y, incr = 0.001) {
  p.vec = seq(0.01,1,by=incr)
  i = 1
  prec.vec <- c()
  
  for (p in p.vec) {
    prec.vec[i] <- get.precision(pred.vals, Y, p)
    i = i+1
  }
  return(list(prec = prec.vec, p = p.vec)) 
}



### Compute vectors to plot

### Predict values and compute precision for plotting
dtest <- xgb.DMatrix(data = as.matrix(test[,-c('Y','Y_1','Y_2','C','Div','Date')]), label=as.matrix(test[,Y]))

# Number of trees to include
#treelim = xg$best_iteration
#treelim = which.max(xg$evaluation_log$test_map)
treelim = 1350

pred.vals <- predict(xg,dtest, ntreelimit = treelim)
prec.count.steps <- get.precision.count.steps(pred.vals, test$Y)
prec.cont <- get.prec.continous(pred.vals,test$Y)

# Have to scale by s to get double y-axis
s <- max(prec.count.steps$count)

cutoff <- data.frame( x = c(-Inf, Inf), y = s*0.55, cutoff = factor(s*0.55) )

ggplot () +
  geom_col( aes(x = prec.count.steps$p, y = prec.count.steps$count)) + 
  geom_line(aes(x = prec.cont$p , y = s*prec.cont$prec), color = 'green') +
  geom_step(aes(x = prec.count.steps$p, y = s*prec.count.steps$prec), color = 'red') +
  geom_line(aes(x, y), cutoff,linetype = 'dotted') +
  scale_y_continuous(sec.axis = sec_axis(~./s,name='precision',breaks=seq(0,1,by=0.05))) +
  scale_x_continuous(name = 'model output',breaks=seq(0,1,by=0.05), limits = c(min(pred.vals),max(pred.vals))) +
  ylab('frequency')
  
  
# IMPORTANCE PLOT

importance <- xgb.importance(model = xg, feature_names = colnames(train[,-c('Y','Y_1','Y_2','C','Div','Date')]))
xgb.plot.importance(importance,top_n = 10)
# HSTaa5 is weighted heaviest. This is problematic. xgboost doesnt care about scaling,
# so time discounting does nothing. 
# suggested fixes: different model?, use a discounted sum instead of multiple columns for each parameter




