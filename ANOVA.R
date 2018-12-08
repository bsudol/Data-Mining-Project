setwd("/Users/basiasudol/Documents/LABWD/young-people-survey")

responses <- read.table("responses.csv", header = TRUE, sep =",")
responses$Music <- NULL;responses$Dance <- NULL;responses$Dance<- NULL;responses$Folk<- NULL;responses$Country<-NULL;responses$Classical.music<-NULL;responses$Musical<-NULL;responses$Pop<-NULL;responses$Rock<-NULL;responses$Metal.or.Hardrock<-NULL;responses$Punk<-NULL;responses$Hiphop..Rap<-NULL;responses$Reggae..Ska<-NULL;responses$Swing..Jazz<-NULL;responses$Rock.n.roll<-NULL;responses$Alternative<-NULL;responses$Latino<-NULL;responses$Techno..Trance<-NULL;responses$Opera<-NULL;responses$Movies<-NULL;responses$Horror<-NULL;responses$Thriller<-NULL;responses$Comedy<-NULL;responses$Romantic<-NULL;responses$Sci.fi<-NULL;responses$War<-NULL;responses$Fantasy.Fairy.tales<-NULL;responses$Animated<-NULL;responses$Animated<-NULL;responses$Documentary<-NULL;responses$Western<-NULL;responses$Action<-NULL;responses$History<-NULL;responses$Psychology<-NULL;responses$Politics<-NULL;responses$Mathematics<-NULL;responses$Physics<-NULL;responses$Internet<-NULL;responses$PC<-NULL;responses$Economy.Management<-NULL;responses$Biology<-NULL;responses$Chemistry<-NULL;responses$Reading<-NULL;responses$Geography<-NULL;responses$Foreign.languages<-NULL;responses$Medicine<-NULL;responses$Law<-NULL;responses$Cars<-NULL;responses$Art.exhibitions<-NULL;responses$Religion<-NULL;responses$Countryside..outdoors<-NULL;responses$Dancing<-NULL;responses$Musical.instruments<-NULL;responses$Writing<-NULL
#clean out lines involving music movies and interests for this portion of the study to save data when we omit lines with missing responses. did it manually because it was messing up the CSV when I tried to do it in excel
dim(responses)

#create new applicable variables
responses$Sport = (responses$Active.sport+responses$Passive.sport)/2 #new variable for average 'sportiness'
responses$BMI = responses$Weight/(responses$Height/100)^2

#finish setting up data make training and test sets
responses <- na.omit(responses)
dim(responses)
View(responses)
set.seed(1)
train_idx = sample(1:nrow(responses), 3/4*nrow(responses))
test_idx = responses[-train_idx,]
train_idx = responses[train_idx,]






#model selection using ANOVA
#GAMS including splines and linear fns of spending on healthy food, concern about health, healthy eating, smoking and drinking habits, sports, internet usage.
#including and excluding age and weight/height
#response variable = Happiness in life (1-5 scale)

library(gam)
library(splines)

# Variables to analyze given best subset selection:
# Energy levels, loneliness, changing the past, Personality, dreams, Fun with friends, Reliability, Parents advice
gam001 = lm(Happiness.in.life ~ Health + Healthy.eating + Sport + Spending.on.healthy.eating + Smoking + BMI + Age, data = responses)
summary(gam001) #compare to this one where there are non significant variables... much higher p values
#these two are linear models of the best subset selection variables and the lowest Gini from boosting tree.
gam01 = lm(Happiness.in.life ~ Energy.levels + Loneliness + Changing.the.past + Personality + Dreams + Fun.with.friends, data = responses)
gam02 = lm(Happiness.in.life ~ Energy.levels + Loneliness + Changing.the.past + Number.of.friends + Mood.swings + BMI + Healthy.eating, data = responses)
summary(gam01) #more significant variables than gam02
summary(gam02)


#Best gini
#loneliness, energy (spline), BMI polynomial (quad), changing past polynomial, numfriends, mood swings, healthy eating
#based on noninear analysis, we visually inspected these variables and suspect nonlinearity (polynomial relationships) in four of these
# variables in particular, so we will do cross validation to find the best degrees of freedom
#looking at the results from the linear model, mood swings and BMI and healthy eating were the least significant so lets also consider those
# high in priority to make nonlinear in the GAM.
library(boot)






#CROSS VALIDATION
#loneliness
cv.error=rep(0,4)
for (i in 1:20){
  for (i in 1:4) {
    glm.fit = glm(Happiness.in.life ~ poly(Loneliness, i), data = responses)
    cv.error[i] = cv.error[i] + cv.glm(responses, glm.fit, K=20)$delta[1]
  }
}
plot(cv.error/20, type="b") #1 or 3

#energy levels
cv.error=rep(0,4)
for (i in 1:20){
  for (i in 1:4) {
    glm.fit = glm(Happiness.in.life ~ poly(Energy.levels, i), data = responses)
    cv.error[i] = cv.error[i] + cv.glm(responses, glm.fit, K=20)$delta[1]
  }
}
plot(cv.error/20, type="b") #degree 2

#BMI
cv.error=rep(0,4)
for (i in 1:20){
  for (i in 1:4) {
    glm.fit = glm(Happiness.in.life ~ poly(BMI, i), data = responses)
    cv.error[i] = cv.error[i] + cv.glm(responses, glm.fit, K=20)$delta[1]
  }
}
plot(cv.error/20, type="b") #degree 1

#changing the past
cv.error=rep(0,4)
for (i in 1:20){
  for (i in 1:4) {
    glm.fit = glm(Happiness.in.life ~ poly(Changing.the.past, i), data = responses)
    cv.error[i] = cv.error[i] + cv.glm(responses, glm.fit, K=20)$delta[1]
  }
}
plot(cv.error/20, type="b") #degree 1

#numfriends
for (i in 1:20){
  cv.error=rep(0,4)
  for (i in 1:4) {
    glm.fit = glm(Happiness.in.life ~ poly(Number.of.friends, i), data = responses)
    cv.error[i] = cv.error[i] + cv.glm(responses, glm.fit, K=20)$delta[1]
  }
}
plot(cv.error/20, type="b") #degree 2

#Mood swings
cv.error=rep(0,4)
for (i in 1:20){
  for (i in 1:4) {
    glm.fit = glm(Happiness.in.life ~ poly(Mood.swings, i), data = responses)
    cv.error[i] = cv.error[i] + cv.glm(responses, glm.fit, K=20)$delta[1]
  }
}
plot(cv.error/20, type="b") #degree 1

#healthy eating
cv.error=rep(0,4)
for (i in 1:20){
  for (i in 1:4) {
    glm.fit = glm(Happiness.in.life ~ poly(Healthy.eating, i), data = responses)
    cv.error[i] = cv.error[i] + cv.glm(responses, glm.fit, K=20)$delta[1]
  }
}
plot(cv.error/20, type="b") #degree 1






#FIRST ANOVA USING CROSS VALIDATION RESULTS
gama = lm(Happiness.in.life ~ Energy.levels + Loneliness + Changing.the.past + Number.of.friends + Mood.swings + BMI +Healthy.eating, data = responses)
gamb = gam(Happiness.in.life ~ Loneliness + poly(Energy.levels,2) + Changing.the.past + Number.of.friends + Mood.swings + BMI, data = responses)
gamc = gam(Happiness.in.life ~ poly(Energy.levels,3) + poly(Loneliness,2) + Changing.the.past + Number.of.friends +  Mood.swings + BMI + Healthy.eating, data = responses)
gamd = gam(Happiness.in.life ~ s(Energy.levels,2) + poly(Loneliness,3) + Changing.the.past + Number.of.friends + Mood.swings + BMI + Healthy.eating, data = responses)
anova(gama, gamb, gamc, gamd, test='F')
#None of these were significant over the linear model... try more combinations.

#ANOVA 2
#going to take the nonlinear variables from Max's analysis and make sure to put those in polynomials the most (also paid attention to visually nonlinear/ lower p val in lm variables)
gam1 = lm(Happiness.in.life ~ Energy.levels + Loneliness + Changing.the.past + Number.of.friends + Mood.swings + BMI +Healthy.eating, data = responses)
gam2 = gam(Happiness.in.life ~ Energy.levels + poly(Loneliness,2) + Changing.the.past + Number.of.friends + poly(Mood.swings,2) + poly(BMI,2), data = responses)
gam4 = gam(Happiness.in.life ~ poly(Energy.levels,2) + poly(Loneliness,2) + Changing.the.past + Number.of.friends +  poly(Mood.swings,2) + BMI + Healthy.eating, data = responses)
gam3 = gam(Happiness.in.life ~ s(Energy.levels,2) + Loneliness + poly(Changing.the.past,2) + Number.of.friends + Mood.swings + BMI + Healthy.eating, data = responses)
gam5 = gam(Happiness.in.life ~ s(Energy.levels) + poly(Loneliness,2) + poly(Changing.the.past,2) + Number.of.friends + Mood.swings + BMI + poly(Healthy.eating,2), data = responses)
anova(gam1, gam2, gam3, gam4, gam5, test='F')
#gam3 is the best and has compelling evidence that this is better than a linear relationship

#ANOVA 3
#try more functions against gam3:
gamaa = gam(Happiness.in.life ~ s(Energy.levels,2) + Loneliness + poly(Changing.the.past,2) + Number.of.friends + Mood.swings + BMI + Healthy.eating, data = responses)
gambb = gam(Happiness.in.life ~ poly(Energy.levels,2) + poly(Loneliness,3) + poly(Changing.the.past,2) + Number.of.friends + Mood.swings + BMI + Healthy.eating, data = responses)
gamcc = gam(Happiness.in.life ~ s(Energy.levels,2) + s(Loneliness,3) + poly(Changing.the.past,2) + Number.of.friends + Mood.swings + BMI + Healthy.eating, data = responses)
anova(gamaa, gambb, gamcc, test='F')
#gamcc is the best here

#results: they are all fairly similar. we can conclude that the second one is slightly more accurate, but all three are good
#we can go into pros and cons of each model and which ones are computationally less expensive and other reasons to pick one 
#model over the other



#COMPARE PREDICTION ACCURACIES of best models
#now take linear model of all best subset selection variables, plus gam3 and gamcc, and compare prediction accuracy.
summary(gam01) #low Rsquared of 0.4 so I dont think this will make the best predictions
summary(gam3) #lots of significant variables, AIC of 1583
summary(gamcc) #same amt of significant variables, AIC of 1586

#par(mfrow=c(1,3))
#plot.gam(gam01, se=TRUE, col='green')
#plot(gam3, se=TRUE,col="blue")
#plot(gamcc, se=TRUE, col="red")

#train new models just on the training set
gamLIN = lm(Happiness.in.life ~ Energy.levels + Loneliness + Changing.the.past + Personality + Dreams + Fun.with.friends, data = train_idx)
gamFirst = gam(Happiness.in.life ~ s(Energy.levels,2) + Loneliness + poly(Changing.the.past,2) + Number.of.friends + Mood.swings + BMI + Healthy.eating, data = train_idx)
gamSecond = gam(Happiness.in.life ~ s(Energy.levels,2) + s(Loneliness,3) + poly(Changing.the.past,2) + Number.of.friends + Mood.swings + BMI + Healthy.eating, data = train_idx)
gamBest = gam(Happiness.in.life ~ s(Personality,2) + Loneliness + Changing.the.past + s(Energy.levels,2) + Dreams + Fun.with.friends , data = responses)

predLIN=predict(gamLIN, newdata=test_idx)
predFirst = predict(gamFirst, newdata = test_idx)
predSecond = predict(gamSecond, newdata = test_idx)
predBest = predict(gamBest, newdata = test_idx)

accLIN = mean((predLIN-test_idx$Happiness.in.life)^2)
accFirst = mean((predFirst-test_idx$Happiness.in.life)^2)
accSecond = mean((predSecond-test_idx$Happiness.in.life)^2)
accBest = mean((predBest-test_idx$Happiness.in.life)^2)
accLIN
accFirst
accSecond
accBest

#results: they are all fairly similar. we can conclude that the second one is slightly more accurate, but all three are good
#we can go into pros and cons of each model and which ones are computationally less expensive and other reasons to pick one 
#model over the other



#################
#################
#################
#################


#do more CV for the other vars: personality, dreams, fun with friends
#Personality
cv.error=rep(0,4)
for (i in 1:20){
  for (i in 1:4) {
    glm.fit = glm(Happiness.in.life ~ poly(Personality, i), data = responses)
    cv.error[i] = cv.error[i] + cv.glm(responses, glm.fit, K=20)$delta[1]
  }
}
plot(cv.error/20, type="b") #degree 2
#Dreams
cv.error=rep(0,4)
for (i in 1:20){
  for (i in 1:4) {
    glm.fit = glm(Happiness.in.life ~ poly(Dreams, i), data = responses)
    cv.error[i] = cv.error[i] + cv.glm(responses, glm.fit, K=20)$delta[1]
  }
}
plot(cv.error/20, type="b") #degree 1
#Fun.with.friends
cv.error=rep(0,3)
for (i in 1:20){
  for (i in 1:3) {
    glm.fit = glm(Happiness.in.life ~ poly(Fun.with.friends, i), data = responses)
    cv.error[i] = cv.error[i] + cv.glm(responses, glm.fit, K=20)$delta[1]
  }
}
plot(cv.error/20, type="b") #degree 1


gamlinear = lm(Happiness.in.life ~ Energy.levels + Loneliness + Changing.the.past + Personality + Dreams + Fun.with.friends , data = responses)
gamone = gam(Happiness.in.life ~ poly(Energy.levels,2) + Loneliness + Changing.the.past + Personality + Dreams + Fun.with.friends , data = responses)
gamtwo = gam(Happiness.in.life ~ poly(Personality,2) + Loneliness + Changing.the.past + Energy.levels + Dreams + Fun.with.friends , data = responses)
gamthree = gam(Happiness.in.life ~ poly(Personality,2) + Loneliness + Changing.the.past + poly(Energy.levels,2) + Dreams + Fun.with.friends , data = responses)
gamfour = gam(Happiness.in.life ~ s(Personality,2) + Loneliness + Changing.the.past + s(Energy.levels,2) + Dreams + Fun.with.friends , data = responses)
gamfive = gam(Happiness.in.life ~ s(Personality,2) + Loneliness + Changing.the.past + Energy.levels + Dreams + Fun.with.friends , data = responses)
gamsix = gam(Happiness.in.life ~ Personality + Loneliness + Changing.the.past + s(Energy.levels,2) + Dreams + Fun.with.friends , data = responses)
anova(gamlinear,gamone,gamtwo,gamthree,gamfour, gamfive, gamsix, test='F')
#gam four with two splines is the only significant model
