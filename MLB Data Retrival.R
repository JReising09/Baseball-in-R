#install.packages("pitchRx")
library(pitchRx)
#install.packages("dplyr")
library(dplyr)
#install.packages("RSQLite")
library(RSQLite)

##################################################################################
# This does not seem to work
my_db <- src_sqlite("pitchRx.sqlite3", create = TRUE)
my_db

# Scrape the 2018 data
files <- c("inning/inning_all.xml", "players.xml")
scrape(start = "2018-03-29", end = "2018-10-29", suffix = files, connect = my_db$con)


####################################################################################
library(mlbgameday)

innings_df <- get_payload(start = "2018-03-29", end = "2018-10-30")
pitchdat<-innings_df$pitch
atbatdat<-innings_df$atbat
actiondat<-innings_df$action
runnerdat<-innings_df$runner
View(runnerdat)

write.csv(pitchdat, file = "pitchdata sample.csv")
write.csv(atbatdat, file = "atbatdata sample.csv")
write.csv(actiondat, file = "actiondata sample.csv")
write.csv(runnerdat, file = "runnerdata sample.csv")

#################################################################################
# Parallel Processing Attempt

library(doParallel)
library(DBI)

# First we need to register our parallel cluster.
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)  
registerDoParallel(cl)

# Create the database in our working directory.
con <- dbConnect(RSQLite::SQLite(), dbname = "gameday.sqlite3")

# Collect all games, including pre and post-season for the 2016 season.
get_payload(start = "2017-04-04", end = "2017-04-05", dataset = 'inning_hit')

# Don't forget to stop the cluster when finished.
stopImplicitCluster()
rm(cl)

#################################################################################
alltables = dbListTables(con) #List tables in DataBase
alltables

pitch_2018 = dbGetQuery( con,'select * from pitch' )

write.csv(pitch_2018, "2018 Pitch Data.csv")

View(head(pitch_2018))

ls("package:mlbgameday")

