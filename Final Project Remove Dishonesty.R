####Section 2.2####

#In this section we search for and mark possible dishonesty
#in answering the survey. First we search for people who might
#have answered the questions randomly. Here we look for people
#who are very distanced from the mean response for each
#question. We marked people who were more than two three
#deviations away from from the "average response" when you
#add how far away they were from the average response for each
#question. Secondly we look for people who answered the survey
#with the same response over and over again. We take the mode
#of their responses and check to see how large it is. If
#they answered the survey over 90% with the same answer,
#then they are marked. There is no way to know for sure if they
#actually were dishonest in answering, however this could be
#a strong prediction to remove dishonest responses from the
#dataset for future analysis.


####Section 2.2.1####

responsesalt = read.csv("~/Downloads/young-people-survey/responses.csv")

#Replace NA values with the mean of that column
for(i in 1:ncol(responsesalt)){
  responsesalt[is.na(responsesalt[,i]), i] <- mean(responsesalt[,i], na.rm = TRUE)
}

#Remove the factors for this analysis
v= c()

for (i in 1:ncol(responsesalt)){
  if (is.factor(responsesalt[,i])){
    v = c(v,i)
  }
}

responsesalt = responsesalt[,-v]


#Remove unopinionated data
responsesalt = responsesalt[ , -which(names(responsesalt) %in% c("Height" , "Age" , "Weight" , "Number.of.siblings"))]

rem = rep(0,nrow(responsesalt)) #Initialize an array which will mark potential responses to remove

means = rep(0,ncol(responsesalt))

#Create a vector that stores the mean of each column
for(j in 1:ncol(responsesalt)){
  means[j] <- mean(responsesalt[,j])
}



distance = rep(0,nrow(responsesalt))

for (i in 1:nrow(responsesalt)){
  for (j in 1:ncol(responsesalt)){
    distance[i] = distance[i] + abs(responsesalt[i,j] - means[j])
  }
}

m = mean(distance)
s = sd(distance)


# Record the total distance each person is from the mean of
#each column
for (i in 1:nrow(responsesalt)){
  if (distance[i] > m + 3*s)
    rem[i] = 1
}

#Try lof

install.packages("Rlof")
library("Rlof")

k = seq(100,150)

lof = lof(responsesalt,k)

maxlof = rep(0,nrow(responsesalt))
for (i in 1:nrow(responsesalt)){
  maxlof[i] = max(lof[i,])
}
maxlof


#We could weight based on their LOF value
# We can see in comparing the LOF values and the varience
#from the predictors, we can see that it is almost a perfect
#match between the two. Therefore we can say that it is a pretty
#safe bet that we can label these as outliers.

for (i in 1:nrow(responsesalt)){
  if (rem[i] == 0 | maxlof[i] < 1.23 ){
    rem[i] = 0
  }
}

#https://stats.stackexchange.com/questions/138675/choosing-a-k-value-for-local-outlier-factor-lof-detection-analysis
#https://cran.r-project.org/web/packages/Rlof/Rlof.pdf
#https://en.wikipedia.org/wiki/Local_outlier_factor
#Above returns a list of values where they were far away from the mean

####Section 2.2.2####


#function that returns the mode of a vector
getmode <- function(v) {
  uniqv <- unique(v)
  return(uniqv[which.max(tabulate(match(v, uniqv)))])
}

#A loop that returns a vector with the number of times each
#person's most common response was recorded

modelist = rep(0,nrow(responsesalt))
for (i in 1:nrow(responsesalt)){
  occurences<-table(unlist(responsesalt[i,])) #Table of how many times each response was recorded in   a given input
  modelist[i] = occurences[[toString(getmode(responsesalt[i,])[1,1])]] #Pulls the mode value from the above table
}

for (i in 1:nrow(responsesalt)){ #A loop that marks if a person answered the same answer more than 90% of the time
  occurences<-table(unlist(responsesalt[i,]))
  if (occurences[[toString(getmode(responsesalt[i,])[1,1])]] > .9*ncol(responsesalt)){
    rem[i] = 1
  }
}

#Above indexes when a person enters the same value over and over again

ind = c()
for (i in 1:nrow(responsesalt)){
  if (rem[i] == 1){
    ind = c(ind,i)
  }
}

#ind is print out of the data points to remove

####Section 6.2.1####

#Below is a way to visualize non-linearity. Uses
#ggplots to show colors where there are higher density points.

install.packages("tidyverse")
install.packages("viridis")
install.packages("RColorBrewer")

library(tidyverse)
library(viridis)
library(RColorBrewer)

get_density <- function(x, y, n = 100) {
  dens <- MASS::kde2d(x = x, y = y, n = n)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

responses = read.csv("~/Downloads/young-people-survey/responses.csv")

#Replace NA values with the mean of that column
for(i in 1:ncol(responses)){
  responses[is.na(responses[,i]), i] <- mean(responses[,i], na.rm = TRUE)
}

responses$BMI = responses$Weight/(responses$Height/100)^2

c = which(responses$BMI > 35)
responses = responses[-c,]

dat = data.frame(responses$BMI, responses$Happiness.in.life)
dat$density <- get_density(dat[,1], dat[,2])
ggplot(dat,) + geom_point(aes(dat[,1], dat[,2], color = density)) + scale_color_viridis() + xlab(colnames(dat)[1]) + ylab("Happiness")

####Section 3####


#Below predicts against the mean of Happiness 
l = lm(responsesalt$Happiness.in.life~1 , data = responsesalt)
summary(l)

ll = predict(l)
mean((responsesalt$Happiness.in.life - ll)^2)

#[1] 0.6761451

####MSE Of Base Test Data####

peer_responses_NEW.1 <- read.csv("~/Downloads/peer_responses_NEW-1.csv")

lll = predict(l, newdata = peer_responses_NEW.1)

mean((peer_responses_NEW.1$Happiness.in.life - lll)^2)

#[1] 1.648721
