#Lindsay's Part Boosting#
#Hello!!
#Alexa
#Hello!! Max
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
train_idx = responses[train_idx,]
test_idx = responses[-train_idx,]


#model selection using ANOVA
#GAMS including splines and linear fns of spending on healthy food, concern about health, healthy eating, smoking and drinking habits, sports, internet usage.
#including and excluding age and weight/height
#response variable = Happiness in life (1-5 scale)

library(gam)
library(splines)

####5. Boosting####
#### 5.1 Boosting on Training Data ####
library(gbm)
set.seed(2)
train=sample(1:nrow(responses), nrow(responses)/2)
Happy=ifelse(responses$Happiness.in.life<=3.7,0,1)
responses_happy=data.frame(responses,Happy)
responses.test=responses_happy[-train,]
#boost.responses= gbm(Happy~ Health + Healthy.eating + Sport + Internet.usage + Spending.on.healthy.eating + Spending.on.looks + Smoking + Age + God + Number.of.friends + Friends.versus.money + Alcohol + Education + Pets + Fun.with.friends, data = responses_happy[train,], distribution = "bernoulli", n.trees = 1000, interaction.depth = 4) 
boost.responses= gbm(Happy~.-Happiness.in.life, data = responses_happy[train,], distribution = "bernoulli", n.trees = 2000, interaction.depth = 4) 
#default shrinkage = 0.001
summary(boost.responses, cBars=10)


yhat.boost = predict(boost.responses, responses.test, n.trees=2000)
Happy_test=Happy[-train]
yhat.boost<-ifelse(yhat.boost>0, 1,0)
C<-table(yhat.boost, Happy_test)
classification_error<-(C[1,2]+C[2,1])/393
classification_error

####5.2 Boosting on Test Data####
test_responses<-read.table("Data-Mining Youth Survey (Responses) - Form Responses 1.csv", header = TRUE, sep =",")


