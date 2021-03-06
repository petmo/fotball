require(data.table)
require(plyr)

### Splice single-seasons .csvs to big .csv
### Example: BL1, BL2 2010-2017


path = 'data/raw'

BL.files = list.files(path = path,pattern="*\\.csv",recursive=T) # Get filenames in path
BL.files <- paste(path,'/',BL.files,sep ='') # Paste path to filename
all.seasons = lapply(BL.files, read.csv) #Read csvs
all.seasons = lapply(all.seasons,data.table) #Convert .csv to data.table

dt.full <- rbind.fill(all.seasons)
dt.full <- data.table(dt.full)
# Bind all data.tables together; missing col should be NA
# Note: Rogue columns (X1,...) 

write.csv(dt.full,'data/processed/Full_raw/Big3004.csv')

