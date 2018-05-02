require('kernlab')

dtest <- xgb.DMatrix(data = as.matrix(test[,-c('Y','Y_1','Y_2','C','Div','Date')]), label=as.matrix(test[,Y]))


svm.data <- data.frame(o.p = over.pred, u.p = (1-under.pred), Y = test$Y)

svp = ksvm(Y~., data = svm.data, type = "C-svc", C = 50, kernel = "rbfdot")


test.data <- data.frame(o.p = pred_over, u.p = pred_under)
svm.pred = predict(svp,test.data)

test.data[,'pred'] = svm.pred
test.data
#test.data[,'actual'] = test$Y[idx]

predict(svp,test.data)


### PRECISION GIVEN LIMIT l
#l = 0.15
prec.over <- sum((over.pred[test$Y == 1] - (1-under.pred[test$Y == 1]) > l))/sum((over.pred - (1-under.pred) > l))

prec.under <- sum((under.pred[test$Y == 0] - (1-over.pred[test$Y == 0]) > l))/sum((under.pred - (1-over.pred) > l))

# shit doesnt work



prec.over <- sum((pred.vals[test$Y == 1]) > 0.5)/sum(pred.vals > 0.5)
prec.over.model <- sum((over.pred[test$Y == 1]) > 0.5)/sum(over.pred > 0.5)

prec.under <- sum((pred.vals[test$Y == 0]) < 0.5)/sum(pred.vals < 0.5)



### COMBINED PRECISION


lim = 0.55
comb.prec <- sum(pred.vals[test$Y == 1] > lim & over.pred[test$Y == 1] > lim)/
              sum(pred.vals > lim & over.pred > lim)
comb.prec;          


comb.vec <- c()
lim.vec <- seq(0.4,0.8,by=0.025)
for (i in 1:length(lim.vec)) {
  lim = lim.vec[i]
  comb.vec[i] <- sum(pred.vals[test$Y == 1] > lim & over.pred[test$Y == 1] > lim)/sum(pred.vals > lim & over.pred > lim)
}


               

### OVER 10.5
#464
pred.vals <- predict(xg,dtest)#, ntreelimit = 900)#xg$best_iteration)#

comb.vec <- c()
lim.vec <- seq(0.5,0.65,by=0.05)
for (i in 1:length(lim.vec)) {
  lim = lim.vec[i]
  comb.vec[i] <- sum(pred.vals[test$Y == 0] > lim)/sum(pred.vals > lim)
}




lim = 0.55
prec <- sum(pred.vals[test$Y == 1] > lim)/sum(pred.vals > lim)
prec


pred.o <- predict(xg.o,dtest, ntreelimit = 500)#xg$best_iteration)#
pred.u <- predict(xg.u,dtest, ntreelimit = 500)#xg$best_iteration)# 


comb.vec <- c()
lim.vec <- seq(0.5,0.65,by=0.05)
for (i in 1:length(lim.vec)) {
  lim = lim.vec[i]
  comb.vec[i] <- sum(pred.u9[test$Y == 1] > lim & pred.uo9[test$Y == 1] > lim)/sum(pred.u9 > lim & pred.uo9 > lim)
}


pred.uo <- 1 - pred.ou

for (i in 1:length(lim.vec)) {
  lim = lim.vec[i]
  comb.vec[i] <- sum(pred.u[test$Y == 1] > lim & pred.uo[test$Y == 1] > lim)/sum(pred.u > lim & pred.uo > lim)
}






                  