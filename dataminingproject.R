setwd("/Users/basiasudol/Documents/LABWD/young-people-survey")

responses <- read.table("responses.csv", header = TRUE, sep =",")
responses$Music <- NULL;responses$Dance <- NULL;responses$Dance<- NULL;responses$Folk<- NULL;responses$Country<-NULL;responses$Classical.music<-NULL;responses$Musical<-NULL;responses$Pop<-NULL;responses$Rock<-NULL;responses$Metal.or.Hardrock<-NULL;responses$Punk<-NULL;responses$Hiphop..Rap<-NULL;responses$Reggae..Ska<-NULL;responses$Swing..Jazz<-NULL;responses$Rock.n.roll<-NULL;responses$Alternative<-NULL;responses$Latino<-NULL;responses$Techno..Trance<-NULL;responses$Opera<-NULL;responses$Movies<-NULL;responses$Horror<-NULL;responses$Thriller<-NULL;responses$Comedy<-NULL;responses$Romantic<-NULL;responses$Sci.fi<-NULL;responses$War<-NULL;responses$Fantasy.Fairy.tales<-NULL;responses$Animated<-NULL;responses$Animated<-NULL;responses$Documentary<-NULL;responses$Western<-NULL;responses$Action<-NULL;responses$History<-NULL;responses$Psychology<-NULL;responses$Politics<-NULL;responses$Mathematics<-NULL;responses$Physics<-NULL;responses$Internet<-NULL;responses$PC<-NULL;responses$Economy.Management<-NULL;responses$Biology<-NULL;responses$Chemistry<-NULL;responses$Reading<-NULL;responses$Geography<-NULL;responses$Foreign.languages<-NULL;responses$Medicine<-NULL;responses$Law<-NULL;responses$Cars<-NULL;responses$Art.exhibitions<-NULL;responses$Religion<-NULL;responses$Countryside..outdoors<-NULL;responses$Dancing<-NULL;responses$Musical.instruments<-NULL;responses$Writing<-NULL
#clean out lines involving music movies and interests for this portion of the study to save data when we omit lines with missing responses. did it manually because it was messing up the CSV when I tried to do it in excel
dim(responses)

#create new applicable variables
responses$Sport = (responses$Active.sport+responses$Passive.sport)/2 #new variable for average 'sportiness'
responses$Interneta[responses$Internet.usage == "most of the day"] <- "1"
responses$Interneta[responses$Internet.usage == "few hours a day"] <- "3"
responses$Interneta[responses$Internet.usage == "less than an hour a day"] <- "5"
responses$Smokingbin[responses$Smoking=="former smoker"] <- "Yes"
responses$Smokingbin[responses$Smoking=="current smoker"] <- "Yes"
responses$Smokingbin[responses$Smoking=="tried smoking"] <- "No"
responses$Smokingbin[responses$Smoking=="never smoked"] <- "No"

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
gam0 = lm(Happiness.in.life ~ Health + Healthy.eating + Sport + Interneta + Spending.on.healthy.eating + Smoking + Age, data = responses)
summary(gam0)
gam01 = lm(Happiness.in.life ~ Health + Healthy.eating + Sport + Interneta + Spending.on.healthy.eating + Smoking + BMI, data = responses)
summary(gam01)
#R squared is very low here and BMI seems to not be significant as a predictor. Take out some high p value variables and do again:
gam02 = lm(Happiness.in.life ~ Healthy.eating + Sport + Interneta + Spending.on.healthy.eating, data = responses)
summary(gam02)

#ANOVA
#could try to do this twice, one with , subset=(gender!="female") and one for !=male
#take the variables that are included in best subset selection and do an ANOVA
gam1 = lm(Happiness.in.life ~ Sport + Healthy.eating + Interneta + Spending.on.healthy.eating, data = responses)
gam2=gam(Happiness.in.life ~ Sport + Healthy.eating + Interneta + s(Spending.on.healthy.eating), data = responses)
gam3=gam(Happiness.in.life ~ Sport + Healthy.eating + Interneta + s(Spending.on.healthy.eating), data = responses)
gam4=gam(Happiness.in.life ~ poly(Sport,3) + Healthy.eating + Interneta + poly(Spending.on.healthy.eating, 3), data = responses)
gam5=gam(Happiness.in.life ~ Sport + s(Healthy.eating) + Interneta + poly(Spending.on.healthy.eating, 4), data = responses)
gam6=gam(Happiness.in.life ~ s(Sport) + Healthy.eating + Interneta + poly(Spending.on.healthy.eating, 3), data = responses)
anova(gam1, gam2, gam3, gam4, gam5 ,test="F")
#gam2 is the only one that has a good p value here (less than 0.05 means to accept the hypothesis)

summary(gam2)

## SUBSET SELECTIONS ##
library(leaps)

#attempt to simply find subset of all variables:
#Forward selection
ss.all.fwd.5 = regsubsets(Happiness.in.life~.,data=responses,nvmax=5, method = 'forward')
#Get following error:
#Warning message:
#In leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax, force.in = force.in,  :
#                 8  linear dependencies found


#Try removing categorical variables, only numerical:
num_responses = responses[sapply(responses, is.numeric)] 
ss.all.fwd.5 = regsubsets(Happiness.in.life~.,data=num_responses,nvmax=5, method = 'forward')
#Get only 1 linear dependency this time

#search for significant correlation via P value:
#install.packages("Hmisc")
library(Hmisc)

correlations = rcorr(as.matrix(num_responses))

for (i in 1:ncol(num_responses)){
  for (j in 1:ncol(num_responses)){
    if ( !is.na(correlations$P[i,j])){
      if (correlations$P[i,j] < .05) {
        print(paste(rownames(correlations$P)[i], "-" , colnames(correlations$P)[j], ": ", correlations$P[i,j]))
      }
    }
  }
}

#Way too many "significant" correlations (what might that mean?). Just look for high correlation vals now:
for (i in 1:ncol(num_responses)){
  for (j in 1:ncol(num_responses)){
    if ( !is.na(correlations$r[i,j])){
      if ( (abs(correlations$r[i,j]) >= .80) && (abs(correlations$r[i,j]) != 1 )) {
        print(paste(rownames(correlations$r)[i], "-" , colnames(correlations$r)[j], ": ", correlations$r[i,j]))
      }
    }
  }
}

#Try running without Weight/BMI
num_responses.fixed = subset(num_responses, select = -c(BMI))
ss.all.fwd.5 = regsubsets(Happiness.in.life~.,data=num_responses.fixed,nvmax=5, method = 'forward')
#1 linear dependency still found

#For some reseaon Sport is causing the problem. Couldn't tell ya why.
num_responses.fixed2 = subset(num_responses, select = -c(Sport))

#5 VARIABLES
#Forward
ss.fwd.5 = regsubsets(Happiness.in.life~.,data=num_responses.fixed2,nvmax=5, method = 'forward')
#IT FUCKING WORKED OH MY GOD
summary(ss.fwd.5)
#Energy levels, loneliness, changing the past, personality, dreams

#Backward
ss.bwd.5 = regsubsets(Happiness.in.life~.,data=num_responses.fixed2,nvmax=5, method = 'backward')
summary(ss.bwd.5)
# Energy levels, loneliness, changing the past, Personality, dreams

#Best
ss.best.5 = regsubsets(Happiness.in.life~.,data=num_responses.fixed2,nvmax=5, method = 'exhaustive', really.big = TRUE)
summary(ss.best.5)
# Energy levels, loneliness, changing the past, Personality, dreams

#10 VARIABLES
#Forward
ss.fwd.10 = regsubsets(Happiness.in.life~.,data=num_responses.fixed2,nvmax=10, method = 'forward')
summary(ss.fwd.10)
# Energy levels, Loneliness, changing the past, Personality, Dreams, 
# Fun with friends, Parents advice, Reliability, Achievements,

#Backward
ss.bwd.10 = regsubsets(Happiness.in.life~.,data=num_responses.fixed2,nvmax=10, method = 'backward')
summary(ss.bwd.10)
# Energy levels, Loneliness, changing the past, Personality, Dreams, 
# Fun with friends, Reliability, Achievements, Parents advice, Phobia of darkness

#Best - Yup so running this crashed my computer, do not recommend
ss.best.10 = regsubsets(Happiness.in.life~.,data=num_responses.fixed2,nvmax=10, method = 'exhaustive', really.big = TRUE)
summary(ss.best.10)