require(xgboost)
require(data.table)

source('first_steps.R')


# Matches lookback
k <- 12

# Parameter for exp smoothing
alpha <- 0.6



## Data column names (in original data) of interest:
data.columns <- c('FTHG','FTAG','HS','AS','HST','AST','HC','AC')

data.columns.home <- c('FTHG','HS','HST','HC')
#only need to iterate through home version


#HP.names <- paste('HP',1:k,sep='')
#AP.names <- paste('AP',1:k,sep='')

all.names = c()

for (stat in data.columns) {
  stat.names.f <- paste(stat,'f.names',sep='') 
  stat.names.a <- paste(stat,'a.names',sep='')
  
  assign(stat.names.f,paste(stat,'f',1:k,sep=''))
  assign(stat.names.a,paste(stat,'a',1:k,sep=''))
  
  all.names = c(all.names, eval(parse(text=stat.names.f)),eval(parse(text=stat.names.a)))
}



#Goals
HGf.names <- paste('HGf',1:k,sep='')  
HGa.names <- paste('HGa',1:k,sep='') 

AGf.names <- paste('AGf',1:k,sep='')  
AGa.names <- paste('AGa',1:k,sep='') 

#Shots
HSf.names <- paste('HSf',1:k,sep='')
HSa.names <- paste('HSa',1:k,sep='')

ASf.names <- paste('ASf',1:k,sep='')
ASa.names <- paste('ASa',1:k,sep='')

#Shots on target
HSTf.names <- paste('HSTf',1:k,sep='')
HSTa.names <- paste('HSTa',1:k,sep='')

ASTf.names <- paste('ASTf',1:k,sep='')
ASTa.names <- paste('ASTa',1:k,sep='')

#Corners
HCf.names <- paste('HCf',1:k,sep='')
HCa.names <- paste('HCa',1:k,sep='')

ACf.names <- paste('ACf',1:k,sep='')
ACa.names <- paste('ACa',1:k,sep='')

#Offsides
#HOf.names <- paste('HOf',1:k,sep='')
#HOa.names <- paste('HOa',1:k,sep='')
#
#AOf.names <- paste('AOf',1:k,sep='')
#AOa.names <- paste('AOa',1:k,sep='')


# Also need (actually dont need)
#info.names <- c('Date','HomeTeam','AwayTeam')
#all.names <- c(info.names,P.names,Gf.names,Ga.names,Sf.names,Sa.names,
#               STf.names,STa.names,Cf.names,Ca.names,Of.names,Oa.names)

all.names <- c(HP.names,AP.names,HGf.names,AGf.names,HGa.names,AGa.names,
               HSf.names,ASf.names,HSa.names,ASa.names,HSTf.names,ASTf.names,
               HSTa.names,ASTa.names,HCf.names,ACf.names,HCa.names,ACa.names,
               'Y')
               #HOf.names,AOf.names,HOa.names,AOa.names,'Y')

# Y will be the target column, aka number of goals





# Test with simple dataset
dt <- data.table(read.csv('data/BL1_BL2_2010-2017.csv'))
dt <- dt[complete.cases(dt[,..data.columns])] #Remove rows missing data we need


dt <- data.table(read.csv('data/Bundesliga/D1_16.csv'))

dt <- convert.and.sort.date(dt)
teams <- get.teams(dt)





# We need to start from matchday 3, so we start from 60
start = 1
i = 1 #row index for dt.fun

dt.fun <- data.table(matrix(0,nrow = (dim(dt)[1]-(start-1)),ncol = length(all.names)))
colnames(dt.fun) = all.names


for (row in 1:nrow(dt)) {
  print(row)
  
  date <- dt[row]$Date
  hometeam <- dt[row]$HomeTeam
  awayteam <- dt[row]$AwayTeam
  
  for (stat in data.columns) {
    
  }
  
  
}



for (row in start:dim(dt)[1]) {
  print(row)
  
  # Many warnings are raised here, investigate!
  
  date <- dt[row]$Date
  hometeam <- dt[row]$HomeTeam
  awayteam <- dt[row]$AwayTeam

  # Goals 
  dt.fun[i,(HGf.names) := as.list(exp.smoothing(rev(get.full.stats(dt,'FTHG', hometeam,
                                                                   date,k,for.against = 1)), alpha))]
  dt.fun[i,(HGa.names) := as.list(exp.smoothing(rev(get.full.stats(dt,'FTHG', hometeam,
                                                                   date,k,for.against = 0)), alpha))]
  
  dt.fun[i,(AGf.names) := as.list(exp.smoothing(rev(get.full.stats(dt,'FTHG', awayteam,
                                                                   date,k,for.against = 1)), alpha))]
  dt.fun[i,(AGa.names) := as.list(exp.smoothing(rev(get.full.stats(dt,'FTHG', awayteam,
                                                                   date,k,for.against = 0)), alpha))]
  
  # Shots
  dt.fun[i,(HSf.names) := as.list(exp.smoothing(rev(get.full.stats(dt,'HS', hometeam,
                                                                   date,k,for.against = 0)), alpha))]
  dt.fun[i,(HSa.names) := as.list(exp.smoothing(rev(get.full.stats(dt,'HS', hometeam,
                                                                   date,k,for.against = 1)), alpha))]
  
  dt.fun[i,(ASf.names) := as.list(exp.smoothing(rev(get.full.stats(dt,'HS', awayteam,
                                                                   date,k,for.against = 0)), alpha))]
  dt.fun[i,(ASa.names) := as.list(exp.smoothing(rev(get.full.stats(dt,'HS', awayteam,
                                                                   date,k,for.against = 1)), alpha))]
  # Shots on target
  dt.fun[i,(HSTf.names) := as.list(exp.smoothing(rev(get.full.stats(dt,'HST', hometeam,
                                                                   date,k,for.against = 0)), alpha))]
  dt.fun[i,(HSTa.names) := as.list(exp.smoothing(rev(get.full.stats(dt,'HST', hometeam,
                                                                   date,k,for.against = 1)), alpha))]
  
  dt.fun[i,(ASTf.names) := as.list(exp.smoothing(rev(get.full.stats(dt,'HST', awayteam,
                                                                    date,k,for.against = 0)), alpha))]
  dt.fun[i,(ASTa.names) := as.list(exp.smoothing(rev(get.full.stats(dt,'HST', awayteam,
                                                                    date,k,for.against = 1)), alpha))]
  # Corners
  dt.fun[i,(HCf.names) := as.list(exp.smoothing(rev(get.full.stats(dt,'HC', hometeam,
                                                                    date,k,for.against = 0)), alpha))]
  dt.fun[i,(HCa.names) := as.list(exp.smoothing(rev(get.full.stats(dt,'HC', hometeam,
                                                                    date,k,for.against = 1)), alpha))]
  
  dt.fun[i,(ACf.names) := as.list(exp.smoothing(rev(get.full.stats(dt,'HC', awayteam,
                                                                   date,k,for.against = 0)), alpha))]
  dt.fun[i,(ACa.names) := as.list(exp.smoothing(rev(get.full.stats(dt,'HC', awayteam,
                                                                   date,k,for.against = 1)), alpha))]
  
  # Offsides
  #dt.fun[i,(HOf.names) := as.list(exp.smoothing(rev(get.full.stats(dt,'HO', hometeam,
  #                                                                 date,k,for.against = 0)), alpha))]
  #dt.fun[i,(HOa.names) := as.list(exp.smoothing(rev(get.full.stats(dt,'HO', hometeam,
  #                                                                 date,k,for.against = 1)), alpha))]
  #
  #dt.fun[i,(AOf.names) := as.list(exp.smoothing(rev(get.full.stats(dt,'HO', awayteam,
  #                                                                 date,k,for.against = 0)), alpha))]
  #dt.fun[i,(AOa.names) := as.list(exp.smoothing(rev(get.full.stats(dt,'HO', awayteam,
  #                                                                 date,k,for.against = 1)), alpha))]
  
  # Y
  dt.fun[i,Y := dt[row]$FTHG + dt[row]$FTAG]
  
  i = i + 1 
}


# Delete some columns
dt.fun[,(HP.names) := NULL]
dt.fun[,(AP.names) := NULL]

dt.fun[,(HOf.names) := NULL]
dt.fun[,(HOa.names) := NULL]
dt.fun[,(AOf.names) := NULL]
dt.fun[,(AOa.names) := NULL]

       
# Remove zero-rows
dt.fun = dt.fun[1:(dim(dt)[1]-start)]


fit <- lm(Y~., data=dt.pr[,-c('Div','div')])

for (a in sample(dim(dt.pr)[1],25)){
  cat(sprintf('pred: %f, actual: %d\n',predict(fit,dt.pr[a,]),dt.pr[a,Y]))
}




p = 0.75 #Train/test proportion

smp_size <- floor(p * nrow(dt.fun))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(dt.fun)), size = smp_size)

train <- dt.fun[train_ind, ]
test <- dt.fun[-train_ind, ]



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
                         nrounds = c(4000),
                         alpha = c(0.0001))


                      
#best: test. 1.735278

xg <- xgb.train(params = tune.grid,
                data = dtrain,
                nrounds = 4000,
                objective = "reg:linear",
                eval_metric = 'rmse',
                early_stopping_rounds = 20,
                watchlist = watchlist)
                
 

test.idx <- sample(dim(test)[1],25)
pred.vals <- predict(xg,xgb.DMatrix(data = as.matrix(test[test.idx,-'Y'])),ntreelimit = 361)#xg$best_iteration)
              
for (i in 1:length(test.idx)) {
  cat(sprintf('pred: %f, actual: %d\n',pred.vals[i],test[test.idx[i],Y]))
}
  



