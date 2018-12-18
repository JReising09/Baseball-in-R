
# Monday data set (Duke)
load(url("http://stat.duke.edu/courses/Summer17/sta101.001-2/uploads/project/mondayBaseball.Rdata"))

# Tuesday data set (Duke)
load(url("http://stat.duke.edu/courses/Summer17/sta101.001-2/uploads/project/tuesdayBaseball.Rdata"))


Monday.Numeric<-mondayBaseball[,sapply(mondayBaseball[1:31,], class)=="numeric"]
Monday.Numeric<-Monday.Numeric[,-c(12,13)]
Monday.Numeric<-Monday.Numeric[complete.cases(Monday.Numeric),]


# Remove Inning balls, and strikes
Monday.Numeric<-Monday.Numeric[,-c(1:3)]

#Remove eephus pitches below 55 mph (anomolies)
Monday.Numeric<-Monday.Numeric[which(Monday.Numeric$releaseVelocity >55),]

#Remove Spin Direction as it is has an unknown transformation..
# both curveballs and fastballs have similar rotations (from left and right pitchers)
Monday.Numeric<-Monday.Numeric[,-4]

attach(Monday.Numeric)

#Check correlations
cmat<-cor(Monday.Numeric, use = 'complete.obs')
corrplot(cmat, type = "upper", method = 'number', number.cex = 0.5)


#Check response vs predictors

plot(probCalledStrike ,releaseVelocity) #weak linear
#plot(spinDir ,releaseVelocity) #curvilinear - need to center
plot(spinRate, releaseVelocity) # weak linear
plot(locationHoriz, releaseVelocity) #weak linear
plot(locationVert, releaseVelocity) #weak linear
plot(movementHoriz, releaseVelocity) #weak linear
plot(movementVert, releaseVelocity) #positive linear


pitch.reg.full<-lm(releaseVelocity~ . , data = Monday.Numeric)

#Check for multicolinearity - looks good
vif(pitch.reg.full)

summary(pitch.reg.full)
anova(pitch.reg.full)

# IT appears we need all of these variables, let's check
# Subset Model Selection
pitch.reg.subsets<-regsubsets(releaseVelocity~ .,data=Monday.Numeric,nbest=2)
plot(pitch.reg.subsets, scale = "adjr2", main = "Adjusted R^2")

#Subsets indicate full model is the best model with removing probCalledStrike as a close second

#Stepwise Regression Model Selection
null<-lm(releaseVelocity~1)
step(null,scope=list(lower=null,upper=pitch.reg.full),direction="forward")

#Forward Regression appears to be the same result.

plot(pitch.reg.full)

# Plot Fitted vs Residuals
plot(fitted(pitch.reg.full), resid(pitch.reg.full))
# Appears to be outliers, but overall constant variance... check formally.

#install.packages("lmtest")
library(lmtest)
bptest(pitch.reg.full)

# BP Test, fail to reject null, and we have non-constant variance
# USe box cox to suggest tranformation of Y (release velo)

library(MASS)

boxcox(pitch.reg.full,lambda = seq(-5, 5, 1/10))

# Suggests Y^4

pitch.trans.reg<- lm(releaseVelocity^4~ . , data = Monday.Numeric)

summary(pitch.trans.reg)
anova(pitch.trans.reg)

#Still no good, move on to non-parametric methods

#Weighted Least Squares
abs.res<- abs(resid(pitch.reg.full))
# Regressing the absolute residuals against the predictors 
reg.fit2 <- lm(abs.res ~ probCalledStrike+spinRate+locationHoriz+
                 locationVert+movementHoriz+movementVert)
# Defining the weights using the fitted values from this second regression: 
weight.vec <- 1/((fitted(reg.fit2))^2)


tab<-cbind(Monday.Numeric,resid(pitch.reg.full), abs(resid(pitch.reg.full)), fitted(reg.fit2), weight.vec)
head(tab)

# Using the weights option in lm to get the WLS estimates: 

reg.fit.wls <- lm(releaseVelocity~ probCalledStrike+spinRate+locationHoriz+
                    locationVert+movementHoriz+movementVert, weights = weight.vec)

summary(reg.fit.wls)

anova(reg.fit.wls)

summary(pitch.reg.full)

# note there is almost no change in the estimates


#Outlier and influential observation Detection

# Outliers in Y
ints.outliers<-which(abs(rstandard(pitch.reg.full))>2.5)
length(ints.outliers)
# We have 1288 outliers but so we will remove them for better predictive ability

# Influential Observations
n<-nrow(Monday.Numeric)
p<-ncol(Monday.Numeric)


dffits.rule<-2*sqrt(p/n)

dffits.rule

dffits.i<-dffits(model = pitch.reg.full)

dffits.infs<-which(abs(dffits.i)>dffits.rule) #Shows which observations have |dffits.i| > 2*sqrt(p/n)
length(dffits.infs)
# WE have 4248 influential outlier observations

length(unique(c(ints.outliers,dffits.infs)))
# 4319 unique outliers and influential outliers so many of them are the same observations.


red.monday<- Monday.Numeric[-unique(c(ints.outliers,dffits.infs))]

attach(red.monday)

new.reg<-lm(releaseVelocity~ ., data = red.monday)

summary(new.reg)
############################################################################################

write.csv(mondayBaseball, "mondayBaseball.csv")
write.csv(tuesdayBaseball, "tuesdayBaseball.csv")


#############################################################################################
#Import Data from Source
load(url("http://stat.duke.edu/courses/Summer17/sta101.001-2/uploads/project/mondayBaseball.Rdata"))

# For looking at ball and strikes... Will probably be useful for logistic 
# regression or classification models

# Subsetting data set and removing NA's
PitchMonday<- mondayBaseball[,c(5,7,8,9,12,16,22,23,25,26:31)]
PitchMonday<- PitchMonday[!is.na(PitchMonday$spinRate), ]
PitchMonday<- PitchMonday[!is.na(PitchMonday$probCalledStrike), ]

names(PitchMonday)<-c("Inning","Balls","Strikes","Outs","BatterHand","PitcherHand",
                      "ProbCalledStrike","PitchResult","ReleaseVelocity","SpinRate",
                      "SpinDir","HorizLocation","VertLocation","HorizMovement",
                      "VertMovement")

# Change factors to characters
PitchMonday$BatterHand<-as.character(PitchMonday$BatterHand)
PitchMonday$PitcherHand<-as.character(PitchMonday$PitcherHand)
PitchMonday$PitchResult<-as.character(PitchMonday$PitchResult)

#Subset data to only include pitch types SL (strike looking) and B (ball)
PitchMonday<-PitchMonday[which(PitchMonday$PitchResult=="SL" | PitchMonday$PitchResult=="B"),]

#Now we have 37598 observation to evaluate strictly for pitch calls

# Replace PitcherHand with 1 for Right and 0 for Left
for (i in 1:nrow(PitchMonday)){
  if (PitchMonday$PitcherHand[i]=="R"){
    PitchMonday$PitcherHand[i]= 1
  }else{
    PitchMonday$PitcherHand[i]=0
  }
}
# Replace BatterHand with 1 for Right and 0 for left
for (i in 1:nrow(PitchMonday)){
  if (PitchMonday$BatterHand[i]=="R"){
    PitchMonday$BatterHand[i]= 1
  }else{
    PitchMonday$BatterHand[i]=0
  }
}

# Replace PitchResult with 1 for strike and 0 for ball
for (i in 1:nrow(PitchMonday)){
  if (PitchMonday$PitchResult[i]=="SL"){
    PitchMonday$PitchResult[i]= 1
  }else{
    PitchMonday$PitchResult[i]=0
  }
}

# Change characters of binary variables to numeric
PitchMonday$BatterHand<-as.numeric(PitchMonday$BatterHand)
PitchMonday$PitcherHand<-as.numeric(PitchMonday$PitcherHand)
PitchMonday$PitchResult<-as.numeric(PitchMonday$PitchResult)

attach(PitchMonday)






# Adding Categorical Vars for balls and strikes

# Attach the new balls and strike indicator variable df's to Monday.numeric

original.colnames<-colnames(Monday.Numeric)

Monday.Numeric<-data.frame(strike.df, balls.df, Monday.Numeric
                           
colnames(Monday.Numeric)<- c("B0","B1","B2","B3","S0","S1","S2", original.colnames[-c(1,2)])
                           

# Create 4 columns for different balls (0,1,2,3)

balls.mat<- matrix(data = rep(0,4*nrow(Monday.Numeric)), ncol = 4,nrow = nrow(Monday.Numeric))

for (i in 1:nrow(Monday.Numeric)){
  if (Monday.Numeric$balls[i]==0){
    balls.mat[i,1]=1
  } else if (Monday.Numeric$balls[i]==1){
    balls.mat[i,2]=1
  } else if (Monday.Numeric$balls[i]==2){
    balls.mat[i,3]=1
  } else if (Monday.Numeric$balls[i]==3){
    balls.mat[i,4]=1
  }
}

# Create columns for strikes (0,1,2)
strike.mat<-matrix(data = rep(0,3*nrow(Monday.Numeric)), ncol = 3,nrow = nrow(Monday.Numeric))

for (i in 1:nrow(Monday.Numeric)){
  if (Monday.Numeric$strikes[i]==0){
    strike.mat[i,1]=1
  } else if (Monday.Numeric$strikes[i]==1){
    strike.mat[i,2]=1
  } else if (Monday.Numeric$strikes[i]==2){
    strike.mat[i,3]=1
  } 
}

# Change matrices to DF's
strike.df<-as.data.frame(strike.mat)
balls.df<-as.data.frame(balls.mat)

