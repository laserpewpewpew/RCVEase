###############################
# RCV - Ease of Understanding #
###############################
library(foreign)

library(foreign)
mydata <- read.dta("Updated.dta")

library(aod)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)

# housekeeping recodes
mydata$newedu[mydata$education=="incomplete"] <- 1
mydata$newedu[mydata$education=="highschool"] <- 2
mydata$newedu[mydata$education=="somecollege"] <- 3
mydata$newedu[mydata$education=="collegegraduate"] <- 4
mydata$newedu[mydata$education=="post-graduate"] <- 5

mydata$newwhite[mydata$white=="non-white"] <- 0
mydata$newwhite[mydata$white=="white"] <- 1

mydata$polint[mydata$q26=="Hardly at all?"] <- 1
mydata$polint[mydata$q26=="Only now and then, or"] <- 2
mydata$polint[mydata$q26=="Some of the time"] <- 3
mydata$polint[mydata$q26=="Most of the time"] <- 4


#############################################
# Ease of Understanding Voting Instructions #
#############################################
mydata$easy.miss[mydata$q4=="Very easy"] <- 4
mydata$easy.miss[mydata$q4=="Somewhat easy"] <- 3 
mydata$easy.miss[mydata$q4=="Somewhat difficult"] <- 2 
mydata$easy.miss[mydata$q4=="Very difficult"] <- 1 
mydata$easy.miss[mydata$q4=="Dont know [VOL]"] <- NA
mydata$easy.miss[mydata$q4=="Refused [VOL]"] <- NA

mydata$easy.nomiss[mydata$q4=="Very easy"] <- 5
mydata$easy.nomiss[mydata$q4=="Somewhat easy"] <- 4
mydata$easy.nomiss[mydata$q4=="Somewhat difficult"] <- 2
mydata$easy.nomiss[mydata$q4=="Very difficult"] <- 1
mydata$easy.nomiss[mydata$q4=="Dont know [VOL]"] <- 3
mydata$easy.nomiss[mydata$q4=="Refused [VOL]"] <- 3
mydata$easy.nomiss[is.na(mydata$q4)] <- 3

mydata$easy.miss.f <- factor(mydata$easy.miss, labels=c("Very Difficult", "Somewhat Difficult", "Somewhat Easy", "Very Easy"))
mydata$easy.nomiss.f <- factor(mydata$easy.nomiss, labels=c("Very Difficult", "Somewhat Difficult", "No opinion", "Somewhat Easy", "Very Easy"))


mydata$easylog[mydata$easy.miss>=4] <- 1
mydata$easylog[mydata$easy.miss<4] <- 0

mydata$easylog2[mydata$easy.miss>=3] <- 1
mydata$easylog2[mydata$easy.miss<3] <- 0

mydata$RCVeaselog[mydata$q21=="Extremely well"] <- 1
mydata$RCVeaselog[mydata$q21=="Very well"] <- 1
mydata$RCVeaselog[mydata$q21=="Somewhat well, or"] <- 0
mydata$RCVeaselog[mydata$q21=="Not at all well?"] <- 0
mydata$RCVeaselog[mydata$q21=="Dont know [VOL]"] <- 0
mydata$RCVeaselog[mydata$q21=="Refused [VOL]"] <- 0

# Dummies for black, latino, asian; not just the 'white' variable
mydata$latino[mydata$qd4=="Yes"] <- 1
mydata$latino[mydata$qd4=="No"] <- 0
mydata$latino[mydata$qd4=="Refused"] <- 0
mydata$latino[mydata$qd4=="Dont know"] <- 0

mydata$black[mydata$qd3==2] <- 1
mydata$black[mydata$qd3==1] <- 0
mydata$black[mydata$qd3==3] <- 0
mydata$black[mydata$qd3==4] <- 0
mydata$black[mydata$qd3==8] <- 0
mydata$black[mydata$qd3==9] <- 0

mydata$asian[mydata$qd3==2] <- 0
mydata$asian[mydata$qd3==1] <- 0
mydata$asian[mydata$qd3==3] <- 1
mydata$asian[mydata$qd3==4] <- 0
mydata$asian[mydata$qd3==8] <- 0
mydata$asian[mydata$qd3==9] <- 0

# Income 

mydata$inc[mydata$qd7=="Less than $10,000"] <- 1
mydata$inc[mydata$qd7=="10 to under $20,000"] <- 1
mydata$inc[mydata$qd7=="20 to under $30,000"] <- 2
mydata$inc[mydata$qd7=="30 to under $40,000"] <- 3
mydata$inc[mydata$qd7=="40 to under $50,000"] <- 4
mydata$inc[mydata$qd7=="50 to under $75,000"] <- 5
mydata$inc[mydata$qd7=="75 to under $100,000"] <- 6
mydata$inc[mydata$qd7=="100 to under $150,000"] <- 7
mydata$inc[mydata$qd7=="$150,000 or more"] <- 8
mydata$inc[mydata$qd7=="Dont know [VOL]"] <- 0
mydata$inc[mydata$qd7=="Refused [VOL]"] <- 0

mydata$inc[mydata$qd7a=="No"] <- 1
mydata$inc[mydata$qd7a=="Yes"] <- 4.38

mydata$inc.miss[mydata$inc==0] <-1
mydata$inc.miss[mydata$inc!=0] <-0

# ease of understanding RCV

mydata$voted[mydata$yes_voted=="Yes"] <- 1
mydata$voted[mydata$yes_voted=="No"] <- 0
mydata$voted <- as.numeric(mydata$voted)

norcv <- mydata[mydata$rcv_voting==0 ,]
rcv <- mydata[mydata$rcv_voting==1 ,]
voters <- mydata[mydata$voted==1 ,]

norcv.v <- norcv[norcv$voted==1 ,]
rcv.v <- rcv[rcv$voted==1 ,]

mydata$easy.f <- factor(mydata$easy, labels=c("Very Difficult", "Somewhat Difficult", "Don't know", "Somewhat Easy", "Very Easy"))

# include age* rcv
olog.easy1 <- polr(easy.nomiss.f ~ rcv_voting + black + latino + asian + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + inc + inc.miss + rcv_voting*age, data=mydata, Hess = TRUE)


olog.easy <- polr(easy.nomiss.f ~ black + latino + asian + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + inc + inc.miss, data=rcv.v, Hess = TRUE)
olog.easy2 <- polr(easy.nomiss.f ~ black + latino + asian + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + inc + inc.miss, data=norcv.v, Hess = TRUE)

olog.easy.rcv <- polr(easy.nomiss.f ~ black + latino + asian + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + inc + inc.miss, data=rcv, Hess = TRUE)
olog.easy.norcv<- polr(easy.nomiss.f ~ black + latino + asian + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + inc + inc.miss, data=norcv, Hess = TRUE)


log.easy <- glm(easylog ~ newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + voted, data=norcv, family="binomial")
log.easy.rcv <- glm(easylog ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + voted, data=rcv, family="binomial")

log.easy2 <- glm(easylog2 ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + voted, data=mydata, family="binomial")
log.easy3 <- glm(RCVeaselog ~ rcv_voting + newwhite + age + male  + newedu + employed + married + democrat + republican + electoral_winner + polint + voted, data=mydata, family="binomial")



require(stargazer)
stargazer(olog.easy1)
stargazer(olog.easy, olog.easy2)
stargazer(olog.easy.rcv, olog.easy.norcv)


########################
# TABLES TABLES TABLES #
########################

# first table: full RCV model with negative coef (w/o interaction) 
# (show with & w/o voting control)
# then 2 subsample models, subsetted by RCV and non-RCV (show w/ and w/o voting control)
# then 3rd table with interaction RCV*age  (with control for voting and w/o? question)
# then 4th, question easeRCV (specfic to RCV voters) logit 
# predicted probabilities - variables of interest = RCV and age (interaction term)
