# Coarsened Exact Matching analysis of calibration training app data
# Gruetzemacher, Lee and Paradice


rm(list=ls());
library(systemfit)
library(plm)
library(lmtest)
library(dplyr)
library(MatchIt)
library("ggplot2")
library(multiwayvcov)
library(lmtest)
library(lfe)
library(cem)


#LOAD DATA
ag<-read.csv(file='/Users/g/Dropbox/_Forecasting/calibration/data&analysis/data-all-games_anon.csv') #my calculations
ag$CRT<-scale(ag$CRT)
#ag$Q3<-scale(ag$Q3)

#SUMMARIES
table(ag$D120, ag$D60)
summary(lm(BSCORE~D60+D120, ag))



############################################
# 60 Question treatment group 
############################################
ag$ctrl<-0
ag$ctrl[ag$D120==0 & ag$D60==0]<-1
dat.m <- subset(ag, ag$D60==1 | ag$ctrl==1)
dat.m <- subset(dat.m, select = c("X", "D60", "EXPERTISE", "CRT", "BSCORE" ))
imbalance(dat.m$D60, data=dat.m[c("EXPERTISE", "CRT")])
m.out <- cem(treatment="D60", data=dat.m, drop=c("X", "BSCORE"))
cem.w <- m.out$w
dat.m <- cbind(dat.m, cem.w)
dat.m <- dat.m[which(cem.w>0.00000001),]
dat.m <- subset(dat.m, select=c("X", "cem.w"))
dat.match <- merge(ag, dat.m, by="X")
imbalance(dat.match$D60, data=dat.match[c("EXPERTISE", "CRT")])
#ONE-TAILED TEST
#p-VALUE HALVED WHEN BETA SIGN IS AS PREDICTED
#BRIER SCORE
out <- lm(BSCORE ~ D60+FEMALE, data = dat.match, weights = cem.w)
summary(out)
#CALIBRATION
out <- lm(CAL ~ D60+FEMALE, data = dat.match, weights = cem.w)
summary(out)
#OVERCONFIDENCE
out <- lm(OCONF ~ D60+FEMALE, data = dat.match, weights = cem.w)
summary(out)



#############################################
# 120 Question treatment group 
#############################################
ag$ctrl<-0
ag$ctrl[ag$D120==0 & ag$D60==0]<-1
dat.m <- subset(ag, ag$D120==1 | ag$ctrl==1)
dat.m <- subset(dat.m, select = c("X", "D120", "BSCORE", "EXPERTISE", "CRT"))
imbalance(dat.m$D120, data=dat.m[c("EXPERTISE", 'CRT')])
m.out <- cem(treatment="D120", data=dat.m, drop=c("X", "BSCORE"))
cem.w <- m.out$w
dat.m <- cbind(dat.m, cem.w)
dat.m <- dat.m[which(cem.w>0.00000001),]
dat.m <- subset(dat.m, select=c("X", "cem.w"))
dat.match <- merge(ag, dat.m, by="X")
imbalance(dat.match$D120, data=dat.match[c("EXPERTISE", 'CRT')])
#ONE-TAILED TESTS
#p-VALUE HALVED WHEN BETA SIGN IS AS PREDICTED
#BRIER SCORE
out <- lm(BSCORE ~ D120+FEMALE, data = dat.match, weights = cem.w)
summary(out)
#CALIBRATION
out <- lm(CAL ~ D120+FEMALE, data = dat.match, weights = cem.w)
summary(out)
#OVERCONFIDENCE
out <- lm(OCONF ~ D120+FEMALE, data = dat.match, weights = cem.w)
summary(out)

