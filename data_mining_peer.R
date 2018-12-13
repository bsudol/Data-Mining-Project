setwd("/Users/basiasudol/Documents/LABWD/young-people-survey")

newest <- read.table("peer_responses_NEW.csv", header = TRUE, sep =",")
#clean out lines involving music movies and interests for this portion of the study to save data when we omit lines with missing responses. did it manually because it was messing up the CSV when I tried to do it in excel

newest$BMI = (newest$Weight/2.205)/(newest$Height*2.54/100)^2
View(newest)


responses <- read.table("responses.csv", header = TRUE, sep =",")
responses$Music <- NULL;responses$Dance <- NULL;responses$Dance<- NULL;responses$Folk<- NULL;responses$Country<-NULL;responses$Classical.music<-NULL;responses$Musical<-NULL;responses$Pop<-NULL;responses$Rock<-NULL;responses$Metal.or.Hardrock<-NULL;responses$Punk<-NULL;responses$Hiphop..Rap<-NULL;responses$Reggae..Ska<-NULL;responses$Swing..Jazz<-NULL;responses$Rock.n.roll<-NULL;responses$Alternative<-NULL;responses$Latino<-NULL;responses$Techno..Trance<-NULL;responses$Opera<-NULL;responses$Movies<-NULL;responses$Horror<-NULL;responses$Thriller<-NULL;responses$Comedy<-NULL;responses$Romantic<-NULL;responses$Sci.fi<-NULL;responses$War<-NULL;responses$Fantasy.Fairy.tales<-NULL;responses$Animated<-NULL;responses$Animated<-NULL;responses$Documentary<-NULL;responses$Western<-NULL;responses$Action<-NULL;responses$History<-NULL;responses$Psychology<-NULL;responses$Politics<-NULL;responses$Mathematics<-NULL;responses$Physics<-NULL;responses$Internet<-NULL;responses$PC<-NULL;responses$Economy.Management<-NULL;responses$Biology<-NULL;responses$Chemistry<-NULL;responses$Reading<-NULL;responses$Geography<-NULL;responses$Foreign.languages<-NULL;responses$Medicine<-NULL;responses$Law<-NULL;responses$Cars<-NULL;responses$Art.exhibitions<-NULL;responses$Religion<-NULL;responses$Countryside..outdoors<-NULL;responses$Dancing<-NULL;responses$Musical.instruments<-NULL;responses$Writing<-NULL
responses$Sport = (responses$Active.sport+responses$Passive.sport)/2 #new variable for average 'sportiness'
responses$BMI = responses$Weight/(responses$Height/100)^2
responses <- na.omit(responses)
set.seed(1)
train_idx = sample(1:nrow(responses), 3/4*nrow(responses))
test_idx = responses[-train_idx,]
train_idx = responses[train_idx,]

#train new models just on the training set
library(gam)
library(splines)
#train new models just on the training set
gamLIN = lm(Happiness.in.life ~ Energy.levels + Loneliness + Changing.the.past + Personality + Dreams + Fun.with.friends, data = train_idx)
gamFirst = gam(Happiness.in.life ~ s(Energy.levels,2) + Loneliness + poly(Changing.the.past,2) + Number.of.friends + Mood.swings + BMI + Healthy.eating, data = train_idx)
gamSecond = gam(Happiness.in.life ~ s(Energy.levels,2) + s(Loneliness,3) + poly(Changing.the.past,2) + Number.of.friends + Mood.swings + BMI + Healthy.eating, data = train_idx)
gamBest = gam(Happiness.in.life ~ s(Personality,2) + Loneliness + Changing.the.past + s(Energy.levels,2) + Dreams + Fun.with.friends , data = train_idx)

predLIN=predict(gamLIN, newdata=newest)
predFirst = predict(gamFirst, newdata = newest)
predSecond = predict(gamSecond, newdata = newest)
predBest = predict(gamBest, newdata = newest)

accLIN = mean((predLIN-newest$Happiness.in.life)^2)
accFirst = mean((predFirst-newest$Happiness.in.life)^2)
accSecond = mean((predSecond-newest$Happiness.in.life)^2)
accBest = mean((predBest-newest$Happiness.in.life)^2)
accLIN
accFirst
accSecond
accBest

predBest

