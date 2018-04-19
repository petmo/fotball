require(data.table)

raw.data <- data.table(read.csv('data/SC0.csv'))


# Convert date to format
raw.data$Date <- as.Date(raw.data$Date,"%d/%m/%y")

teams <- levels(raw.data$HomeTeam)