
require(data.table)

source('first_steps.R')


k = 5
alpha = 0.0025

## Data column names (in original data) of interest:
data.columns <- c('FTHG','FTAG','HS','AS','HST','AST','HC','AC')
data.columns.home <- rep(c('FTHG','HS','HST','HC'),each=2) #this is used as a help vector for iteration

# Load the dataset
dt <- data.table(read.csv('data/processed/Full_raw/Big2504.csv'))
dt <- dt[complete.cases(dt[,..data.columns])] #Remove rows missing data of interest

# Convert and sort date
dt <- convert.and.sort.date(dt)
dt <- trim.whitespace.factors(dt)

mean.var <- compute.mean.var(dt)
dt <- zero.mean.variance(dt, unit.var = 1)

# Need to add 'Entella' to HomeTeam-factors due to some irregularity
add.factor.home <- c('Entella')
levels(dt$HomeTeam) <- c(levels(dt$HomeTeam), add.factor.home)


all.names = c()
for (stat in data.columns) {
  stat.names.hf <- paste(stat,'hf.names',sep='')   #Home-team, for
  stat.names.ha <- paste(stat,'ha.names',sep='')   #Home-team, against
  
  stat.names.af <- paste(stat,'af.names',sep='')   #Away-team, for
  stat.names.aa <- paste(stat,'aa.names',sep='')   #Away-team, against
  
  assign(stat.names.hf,paste(stat,'hf',1:k,sep=''))
  assign(stat.names.ha,paste(stat,'ha',1:k,sep=''))
  
  assign(stat.names.af,paste(stat,'af',1:k,sep=''))
  assign(stat.names.aa,paste(stat,'aa',1:k,sep=''))
  
  all.names = c(all.names, eval(parse(text=stat.names.hf)),eval(parse(text=stat.names.ha)),
                eval(parse(text=stat.names.af)),eval(parse(text=stat.names.aa)))
}

all.names = c(all.names,'Div','Y','Y_1','Y_2')
#Y_1 and Y_2 are home and away corners needed to compute actual amount of corners (using mean/var)
teams <- get.teams(dt)



# Put processed data in dt.pr
dt.pr <- data.table(matrix(0.000,nrow = nrow(dt),ncol = length(all.names)))
colnames(dt.pr) = all.names

dt.pr[,Div := as.factor(Div)]
dt.pr[,Date := seq.Date(from =as.Date("1970-01-01"), by = 0,length.out = nrow(dt))]



#nrow(dt)

for (row in 1:nrow(dt)) {
  print(row)
  
  # Get current datarow values
  date <- dt[row]$Date
  hometeam <- dt[row]$HomeTeam
  awayteam <- dt[row]$AwayTeam
  
  ## NOTE 
  # Could probably optimize by adding all updates to one vector, THEN update df.
  
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
    
    
    dt.pr[row,(feature.hf) := as.list(rev(exp.discount(get.home.stat(dt, stat, hometeam,
                                                                     date, k))))]
    dt.pr[row,(feature.ha) := as.list(rev(exp.discount(get.away.stat(dt, stat, hometeam, 
                                                                     date, k))))]
    
    dt.pr[row,(feature.af) := as.list(rev(exp.discount(get.home.stat(dt, stat, awayteam,
                                                                     date, k))))]
    dt.pr[row,(feature.aa) := as.list(rev(exp.discount(get.away.stat(dt, stat, awayteam, 
                                                                     date, k))))]
  }
  
  dt.pr[row, 'Div'] = dt[row]$Div
  dt.pr[row, 'Date'] = date
  dt.pr[row, 'Y'] = dt[row]$HC + dt[row]$AC
  dt.pr[row, 'Y_1'] = dt[row]$HC # split up home and away target to recompute identity using mean and var.
  dt.pr[row, 'Y_2'] = dt[row]$AC
  
}


# Remove NA (due to required burn-in time)
dt.pr <- dt.pr[complete.cases(dt.pr)]


write.csv(dt.pr,'data/processed/full_pr_2504.csv')
write.csv(mean.var,'data/processed/meanvar_full_pr_2504.csv')
