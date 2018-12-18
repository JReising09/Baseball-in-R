
# Tuesday data set (Duke)
load(url("http://stat.duke.edu/courses/Summer17/sta101.001-2/uploads/project/tuesdayBaseball.Rdata"))

Tuesday.Numeric<-tuesdayBaseball[,sapply(tuesdayBaseball[1:31,], class)=="numeric"]
Tuesday.Numeric<-Tuesday.Numeric[,-c(12,13)]
Tuesday.Numeric<-Tuesday.Numeric[complete.cases(Monday.Numeric),]


# Remove Inning balls, and strikes and spin direction
Tuesday.Numeric<-Tuesday.Numeric[,-c(1:3)]
Tuesday.Numeric<-Tuesday.Numeric[,-4]

test.dat<-Tuesday.Numeric[complete.cases(Tuesday.Numeric),]

#Note that the test data had 75 incomplete observations that were removed.


# Model prediction with the original full model
predict.tuesday<- predict(pitch.reg.full, test.dat[,-2])
actual.responses<-test.dat$releaseVelocity

plot(predict.tuesday,actual.responses)
abline(a=0,b=1, col="red")

#### Model Validation

#Need to use MSPR and MSE instead since we are splitting the data

n.star<-nrow(test.dat)
mspr<-sum((test.dat$releaseVelocity- predict.tuesday)^2)/n.star
mse<-mean(pitch.reg.full$residuals^2)

mspr/mse

# The ratio 0.9139 indicates that the model had adequate predictive ability without
# evidence of the model over fitting the training data.

# Model Accuracy

tues.pred.act<-cbind(predict.tuesday,actual.responses)

mean(apply(tues.pred.act, 1, min)/ apply(tues.pred.act, 1, max))


######################
# Does the model imporve after accounting for influential outliers?
# new reg after removing influential measures
test.dat<-Tuesday.Numeric[complete.cases(Tuesday.Numeric),]

#Note that the test data had 75 incomplete observations that were removed.

predict.tuesday<- predict(new.reg, test.dat[,-2])
actual.responses<-test.dat$releaseVelocity

plot(predict.tuesday,actual.responses)
abline(a=0,b=1, col="red")


#### Model Validation

#Need to use MSPR and MSE instead since we are splitting the data

n.star<-nrow(test.dat)
mspr<-sum((test.dat$releaseVelocity- predict.tuesday)^2)/n.star
mse<-mean(pitch.reg.full$residuals^2)

mspr/mse


# Same ratio.. there is not any significant diference in the predictive capability 
# between the original full model or the model adjusted for outlier influence.


# Model Accuracy

tues.pred.act<-cbind(predict.tuesday,actual.responses)

mean(apply(tues.pred.act, 1, min)/ apply(tues.pred.act, 1, max))

# 96.74% Accuracy

# Same as the regular full model. this is probably the best we can do 
# in a general Multiple Linear Model

# Next Semester we will explore new model types wit the same data.



