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
responses$Smokingnum[responses$Smoking=="never smoked"] <- "5"
responses$Smokingnum[responses$Smoking=="tried smoking"] <- "4"
responses$Smokingnum[responses$Smoking=="current smoker"] <- "1"
responses$Smokingnum[responses$Smoking=="former smoker"] <- "2"
responses$Smokingbin[responses$Smoking=="former smoker"] <- "Yes"
responses$Smokingbin[responses$Smoking=="current smoker"] <- "Yes"
responses$Smokingbin[responses$Smoking=="tried smoking"] <- "No"
responses$Smokingbin[responses$Smoking=="never smoked"] <- "No"

#finish setting up data
responses <- na.omit(responses)
dim(responses)
View(responses)


#model selection using ANOVA
#GAMS including splines and linear fns of spending on healthy food, concern about health, healthy eating, smoking and drinking habits, sports, internet usage.
#including and excluding age and weight/height
#response variable = Happiness in life (1-5 scale)

library(gam)
library(splines)
gam0 = lm(Happiness.in.life ~ Health + Healthy.eating + Sport + Interneta + Spending.on.healthy.eating + Smokingnum, data = responses)
summary(linear)

#ANOVA
#could try to do this twice, one with , subset=(gender!="female") and one for !=male
#take the variables that are included in best subset selection and do an ANOVA 
gam1 = lm(Happiness.in.life ~ Health + Healthy.eating + Interneta + Spending.on.healthy.eating, data = responses)
gam2=gam(wage~year+age+education,data=Wage, subset=(education!="1. < HS Grad"))
gam3=gam(wage~year+s(age,2)+education,data=Wage, subset=(education!="1. < HS Grad"))
gam4=gam(wage~year+s(age,5)+education,data=Wage, subset=(education!="1. < HS Grad"))
gam5=gam(wage~year+s(age,8)+education,data=Wage, subset=(education!="1. < HS Grad"))
anova(gam1, gam2, gam3, gam4, gam5 ,test="F")
