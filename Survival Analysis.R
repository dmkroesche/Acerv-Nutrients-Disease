# set working directory. This wd is different than the one used for calculating S/H
setwd("C:/Users/12677/OneDrive - University of Miami/Desktop/Acerv_Nutrients_project1")

# load packages
library(readr)
library(ggplot2)
library(Rmisc)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(survminer)
library(survival)

# Survival analysis lets you analyze the rates of occurrence of events over time, without assuming the rates are constant.
#lets you model the time until an event occurs, or compare the time-to-event between different groups, or how time-to-event correlates with quantitative variables.

#clear environment
rm(list=ls())

# read in data file(s)
NDSH = read_csv("qPCR samples_R.csv") #S/H data
ND_all = read_csv("All_ND_R.csv") # all frags survivorship

#format dates, added binary variable "category" to NDSH, 
# added "SurvivalTime" variable, added "SH" variable (A.Acerv on log10 scale)
NDSH <-  NDSH %>% 
  mutate(
    `Date Disease` = mdy(`Date Disease`), 
    `Date of Mortality` = mdy(`Date of Mortality`),
    survivalTime= `Date of Mortality`- `Date Disease`,
    survivalTime=ifelse(is.na(survivalTime), 11, survivalTime), 
    category=ifelse(Mortality=="Lived", 0, 1),
    SH=log10(A.Acerv)
  )

# nutrients, diseased, and combo variables in NDSH converted from "character" type data to "factor" type
NDSH$Combo <- as.factor(NDSH$Combo)
NDSH$Diseased <- as.factor(NDSH$Diseased)
NDSH$Nutrients <- as.factor(NDSH$Nutrients)


# same thing as above for ND_all data frame
ND_all <-  ND_all %>% 
  mutate(
    survivalTime= `Days Survived`,
    category=ifelse(Mortality=="Lived", 0, 1),
    Disease=as.factor(Disease),
    Nutrients=as.factor(Nutrients)
  )

ND_all$Disease <- as.factor(ND_all$Disease)
ND_all$Nutrients <- as.factor(ND_all$Nutrients)
# calculate the difference between start and end time in days.
### Error: Problem with `mutate()` input `timesurvived`.
### x character string is not in a standard unambiguous format
### i Input `timesurvived` is `as.numeric(difftime(Date.of.Mortality, Date.Disease, units = "days"))`.


# survMod is a Cox Fit Proportional Hazards Regression Model
?coxph
survMod <- coxph(Surv(survivalTime, category)~Diseased+SH, data=NDSH)
summary(survMod)


# "coxph" fits a Cox Proportional Hazards Regression Model. 
# Below are Cox PHRM's for the NDSH data frame.
?coxph

# Coxph #1: Disease matters for survival, SH doesn't 
survMod_dis_SH <- coxph(Surv(survivalTime, category)~Diseased+SH, data=NDSH)
summary(survMod_dis_SH)

# Coxph #2: Disease treatment matters for survival, Nutrients treatment doesn't

survMod_dis_nut <- coxph(Surv(survivalTime, category)~Diseased+Nutrients, data=NDSH)
summary(survMod_dis_nut)

surv_pvalue(fit=survMod_dis_nut) # surv_pvalue didn't work. Ask Rich maybe.

# Plot of survival probability of pathogen vs. placebo over time
ggsurvplot(fit = survfit(Surv(survivalTime, category)~Diseased, data=NDSH))+
labs(title="A. cervicornis Survivorship by Disease Treatment")
# datSH is a subset of NDSH with only Pathogen treatment fragments. 
datSH <- NDSH %>%
  filter(Diseased=='Pathogen', Timepoint=="T1")

# Coxph of just disease treatment fragments. SH not important for survival within the disease treatment.
shMod <- coxph(Surv(survivalTime, category)~SH, data=datSH)
summary(shMod)

datSH <- NDSH %>%
  filter(Diseased=='Pathogen') #this is the same code from above to make a subset?

#This didn't work. Not sure what this was supposed to be. Graph made a separate category for every SH value. 
ggsurvplot(fit = survfit(Surv(survivalTime, category)~log10(A.Acerv), data=datSH))

# This shows how SH does not predict relative risk (I think)
predSH <- expand.grid(SH=-seq(.5, 1.3, 0.05))
fitSH <- data.frame(predict(shMod, type='risk', se.fit=TRUE, newdata=predSH))
fitSH <- cbind(predSH, fitSH) 

ggplot(fitSH, aes(SH, fit)) +
  geom_line() +
  geom_ribbon(aes(ymin=fit-se.fit, ymax=fit+se.fit), alpha=0.3) +
  geom_hline(yintercept = 1, color='red')

# Coxph for the ND_all data frame. This has more data than NDSH. Elevated nutrients appears to effect survivorship for pathogen treatments.
# not sure if I understand this one 100%
survMod <- coxph(Surv(survivalTime, category)~Disease*Nutrients, data=ND_all)
summary(survMod)
survdiff(Surv(survivalTime, category)~Disease+Nutrients, data=ND_all)
ggsurvplot(fit = survfit(Surv(survivalTime, category)~Disease+Nutrients, data=ND_all))+
labs(title = "A. cervicornis Survivorship by Nutrients-Disease Combination")
#Take out tank 2
dat <- ND_all %>%
  filter(Tank!=2| is.na(Tank))



#mutate(Treatment=ifelse(Tank==2, "Probiotic", "NOthing"))

survMod <- coxph(Surv(survivalTime, category)~Disease*Nutrients, data=dat)
summary(survMod)
ggsurvplot(fit = survfit(Surv(survivalTime, category)~Disease+Nutrients, data=dat))

datPro <- ND_all %>%
  mutate(Treatment=as.factor(ifelse(Tank==2, "Probiotic", "noPri")))

proMod <- coxph(Surv(survivalTime, category)~Disease+Treatment, data=datPro)
summary(proMod)

ggsurvplot(fit = survfit(Surv(survivalTime, category)~Disease+Treatment, data=datPro))

