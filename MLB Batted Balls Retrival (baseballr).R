install.packages("baseballr")
library(baseballr)
install.packages("mlbgameday")
library(mlbgameday)
library(readr)
library(dplyr)
install.packages("devtools")
library(devtools)
devtools::install_github("BillPetti/baseballr")

atbatdata_sample <- read_csv("C:/Users/jreis/Dropbox/ETSU Graduate Work/Fall 2018 Graduate Studies/atbatdata sample.csv")
pitchdata_sample <- read_csv("C:/Users/jreis/Dropbox/ETSU Graduate Work/Fall 2018 Graduate Studies/pitchdata sample.csv")

pitch.dat <- inner_join(pitchdata_sample, atbatdata_sample, by = c("num", "url"))


hit.dat<-scrape_statcast_savant(start_date = "2017-04-03", end_date = "2017-04-04", player_type = "batter") # %>% 
#  filter(type == "X") %>%
#  select(3,7,54:56) %>%


#pitch.dat<-pitch.dat[,-c(1,3,4,6,7,10,20,32,37:39,42,43,48:52,55:59,
                         63,64,71,76,77,79:83)]

############# Try to get entire 2018 season #############

library(doParallel)
library(DBI)
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)  
registerDoParallel(cl)

# Set a new date when you want to download new data

begindate<- as.Date("2018-03-29")
enddate<- as.Date("2018-10-29")
enddate-begindate

# It seems this scrape tool can only get up to 40000 observations at a time, so I
# estimated that I could fit 10 days worth of data at and time and write the following
# loop to retieve and append every 10 days pitching data locally to Pitches18

Pitches18<-scrape_statcast_savant(start_date = paste(begindate),
                                  end_date = paste(begindate+9),
                                  player_type = "batter")

for (i in 1:21){
n<-scrape_statcast_savant(start_date = paste(begindate+(10*i)),
                          end_date = paste(begindate+(10*(i+1)-1)),
                          player_type = "batter")
Pitches18<-rbind(Pitches18,n)
}

setwd("C:/Users/jreis/Dropbox/ETSU Graduate Work/Fall 2018 Graduate Studies")
write.csv(Pitches18, "MLB StatCast 2018.csv")

stopImplicitCluster()
rm(cl)


#############################################################################################
attach(Pitches18)
BattedBalls18<-subset(Pitches18, hc_x>=0, 
                      select = c(sv_id, release_speed,release_pos_x,release_pos_y,
                                 release_pos_z,p_throws,stand,balls,strikes,pfx_x,pfx_z,
                                 plate_x,plate_z,vx0,vy0,vz0,ax,ay,az,launch_angle,launch_speed,
                                 hit_distance_sc))

# Replace p_throws with 1 for Right and 0 for Left
for (i in 1:length(BattedBalls18$sv_id)){
  if (BattedBalls18$p_throws[i]=="R"){
    BattedBalls18$p_throws[i]= 1
    }else{
      BattedBalls18$p_throws[i]=0
    }
}

# Replace stand with 1 for Right and 0 for left
for (i in 1:length(BattedBalls18$sv_id)){
  if (BattedBalls18$stand[i]=="R"){
    BattedBalls18$stand[i]= 1
  }else{
    BattedBalls18$stand[i]=0
  }
}

# Need to remove rows with NA's (Errors in the recording of the data I assume)
BattedBalls18<- BattedBalls18[complete.cases(BattedBalls18),]


#Save csv version of the data
write.csv(BattedBalls18, "BattedBalls18.csv")

# Correaltion Plot for continuous variables
#install.packages("ggcorrplot")
library(ggcorrplot)

corr <- round(cor(BattedBalls18[,c(2:5,10:22)]), 2)
ggcorrplot(corr)

#predictors<-c(sv_id, release_speed,release_pos_x,release_pos_y,
#              release_pos_z,p_throws,stand,balls,strikes,pfx_x,pfx_z,
#              plate_x,plate_z,vx0,vy0,vz0,ax,ay,az,launch_angle,launch_speed)

baseball.reg<-lm(hit_distance_sc~ release_speed+release_pos_x+release_pos_y+
                 release_pos_z+p_throws+stand+balls+strikes+pfx_x+pfx_z+
                 plate_x+plate_z+vx0+vy0+vz0+ax+ay+az+launch_angle+launch_speed)
