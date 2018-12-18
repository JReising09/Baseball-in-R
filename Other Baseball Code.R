
##########################################################################################
# Full Model
pitch.reg.full<-lm(ProbCalledStrike~ .,data = PitchMonday)
# Check Multicolinearity
vif(pitch.reg.full) #Does not indicate multicolinearity

#Check out summary and anova
summary(pitch.reg.full)
anova(pitch.reg.full)


#Model Selection Subset
pitch.reg.subsets<-regsubsets(ProbCalledStrike~ .,data=PitchMonday,nbest=2)
plot(pitch.reg.subsets, scale = "adjr2", main = "Adjusted R^2")

#Stepwise Regression Model Selection
null<-lm(ProbCalledStrike~1)
step(null,scope=list(lower=null,upper=pitch.reg.full),direction="forward")

pitch.red.reg<-lm(formula = ProbCalledStrike ~ VertLocation + Strikes + Balls + 
                    Outs + ReleaseVelocity + BatterHand + SpinRate + PitcherHand + 
                    HorizLocation + VertMovement)

plot(fitted(pitch.red.reg),resid(pitch.red.reg))

plot(log(ProbCalledStrike),VertLocation)


############################################################################################
#Logistic Model Selection
pitch.reg.full<-glm(PitchResult~Inning+Balls+Strikes+Outs+BatterHand+PitcherHand
                    +ReleaseVelocity+SpinRate+SpinDir+HorizLocation+VertLocation
                    +HorizMovement+VertMovement, family = binomial(link = "logit"))

summary(pitch.reg.full)

install.packages("glmulti")
library(glmulti)

# This is going to take a while with our large dataset
glmulti.logistic.out <-
  glmulti(PitchResult~Inning+Balls+Strikes+Outs+BatterHand+PitcherHand
          +ReleaseVelocity+SpinRate+SpinDir+HorizLocation+VertLocation
          +HorizMovement+VertMovement, 
          level = 1,               # No interaction considered
          method = "h",            # Exhaustive approach
          crit = "aic",            # AIC as criteria
          confsetsize = 5,         # Keep 5 best models
          plotty = F, report = F,  # No plot or interim reports
          fitfunction = "glm",     # glm function
          family = binomial)       # binomial family for logistic regression

## Show 5 best models (Use @ instead of $ for an S4 object)
glmulti.logistic.out@formulas

#############################################################################################
# Batted Balls again

BattedMonday<- mondayBaseball[,c(22,25,26:31,33,34)]
BattedMonday<- BattedMonday[!is.na(BattedMonday$battedBallAngle), ]
BattedMonday<- BattedMonday[!is.na(BattedMonday$spinRate), ]
BattedMonday<- BattedMonday[!is.na(BattedMonday$probCalledStrike), ]


#Cor Mat
cmat<-cor(BattedMonday)
corrplot(cmat, type = "upper", method = 'number', number.cex = 0.5)

#########################################################################################
pitch2data<-PitchMonday[,c(8:15)]
# REmove obs with 0 prob of being called strike with ProbCalled Strike as response
#pitch2data<-pitch2data[which(pitch2data$ProbCalledStrike !=0),]
#Log Transform for ProbCalledStrike
#pitch2data$ProbCalledStrike<-log(pitch2data$ProbCalledStrike)

scaled.pitch.dat<-scale(pitch2data[,2:8],colMeans(pitch2data[,2:8]))
scaled.pitch.dat<-cbind(scaled.pitch.dat,pitch2data$PitchResult)
colnames(scaled.pitch.dat)[8]<-"PitchResult"
scaled.pitch.dat<-as.data.frame(scaled.pitch.dat)

attach(scaled.pitch.dat)

#Cor Matrix
cmat<-cor(scaled.pitch.dat)
corrplot(cmat, type = "upper", method = 'number', number.cex = 0.5)


#pitch.reg.full<-lm(log(ProbCalledStrike)~ReleaseVelocity+SpinRate+HorizLocation+VertLocation)

#summary(pitch.reg.full)


pitch.log.reg<-glm(PitchResult~HorizLocation+VertLocation,
                   family = binomial(link = "logit"))


summary(pitch.log.reg)
anova(pitch.log.reg)

plot(fitted(pitch.log.reg))


###########################################################################################

pitch.reg.full<-lm(ProbCalledStrike~ ., data = pitch2data)

anova(pitch.reg.full)

pitch.red.reg<-lm(ProbCalledStrike~ReleaseVelocity+SpinRate+HorizLocation+VertLocation)

anova(pitch.red.reg)
summary(pitch.red.reg)

###########################################################################################
#Plot pitches and called strikes
pitch2data<-PitchMonday[,c(7:15)]
# REmove obs with 0 prob of being called strike 
pitch2data<-pitch2data[which(pitch2data$ProbCalledStrike !=0),]

plot(pitch2data$HorizLocation,pitch2data$VertLocation, col =pitch2data$PitchResult+2,
     xlim = c(-3,3), main = "2016 Called Balls and Strikes" )
rect(-.7,1.5,.7,3.5,border = "blue", lwd = 2)

###########################################################################################
# General Additive Model
pitch2data<-PitchMonday[,c(8:15)]

install.packages("mgcv")
require(mgcv)
pitch.gam<-gam(pitch2data$PitchResult ~ s(pitch2data$HorizLocation, pitch2data$VertLocation),
      family=binomial, 
      data=pitch2data)

anova.gam(pitch.gam) #No idea what this means at this time...
