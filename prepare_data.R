require(xgboost)
require(data.table)

source('first_steps.R')


## Data column names (in original data) of interest:
data.columns <- c('FTHG','FTAG','HS','AS','HST','AST','HC','AC')
data.columns.home <- rep(c('FTHG','HS','HST','HC'),each=2) #this is used as a help vector for iteration

# Load the dataset
dt <- data.table(read.csv('data/processed/Full_raw/E_F_SP_I_D.csv'))
dt <- dt[complete.cases(dt[,..data.columns])] #Remove rows missing data of interest

# Convert and sort date
dt <- convert.and.sort.date(dt)
dt <- trim.whitespace.factors(dt)

# Need to add 'Entella' to HomeTeam-factors due to some irregularity
add.factor.home <- c('Entella')
levels(dt$HomeTeam) <- c(levels(dt$HomeTeam), add.factor.home)


teams <- get.teams(dt)



# Now we create the feature names 
all.names = c()

# How many matches backward we wanna save
k = 5

for (stat in data.columns) {
  stat.names.f <- paste(stat,'f.names',sep='') 
  stat.names.a <- paste(stat,'a.names',sep='')
  
  assign(stat.names.f,paste(stat,'f',1:k,sep=''))
  assign(stat.names.a,paste(stat,'a',1:k,sep=''))
  
  all.names = c(all.names, eval(parse(text=stat.names.f)),eval(parse(text=stat.names.a)))
}

all.names = c(all.names,'Div','Y')





# Put processed data in dt.pr
dt.pr <- data.table(matrix(0,nrow = nrow(dt),ncol = length(all.names)))
colnames(dt.pr) = all.names

### Set parameters!!

k = 8
alpha = 0.8



for (row in 1:nrow(dt)) {
  print(row)
  
  # Get current datarow values
  date <- dt[row]$Date
  hometeam <- dt[row]$HomeTeam
  awayteam <- dt[row]$AwayTeam
  
  ## NOTE 
  # Could probably optimize by adding all updates to one vector, THEN update df.
  
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
  
  dt.pr[row, Div := dt[row]$Div]
  dt.pr[row,Y := dt[row]$HC + dt[row]$AC]
  
}


# Remove NA (due to required burn-in time)
dt.pr <- dt.pr[complete.cases(dt.pr)]


write.csv(dt.pr,'data/processed/data_5_corners.csv')
