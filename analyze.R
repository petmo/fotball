### Do some pre-analyzing of full raw data

require(data.table)

## Data column names (in original data) of interest:
data.columns <- c('FTHG','FTAG','HS','AS','HST','AST','HC','AC')
data.columns.home <- rep(c('FTHG','HS','HST','HC'),each=2) #this is used as a help vector for iteration


# Load the dataset
dt.analyze <- data.table(read.csv('data/processed/Full_raw/E_F_SP_I_D.csv'))
dt.analyze <- dt.analyze[complete.cases(dt.analyze[,..data.columns])] #Remove rows missing data of interest



# (Corner) means per division
aggregate(dt.analyze[,c('HC','AC')],list(dt.analyze$Div),mean)

aggregate(dt.analyze[,c('FTHG','FTAG')],list(dt.analyze$Div),mean)


agg.dt <- data.table(aggregate(dt[,c('HC','AC')],list(dt$HomeTeam),mean))
agg.dt[,'diff']  = (agg.dt[['HC']] - agg.dt[['AC']])
