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

#format as dates in a tibble
NDSH <-  NDSH %>% 
  mutate(
    `Date Disease` = mdy(`Date Disease`), 
    `Date of Mortality` = mdy(`Date of Mortality`),
    survivalTime= `Date of Mortality`- `Date Disease`,
    survivalTime=ifelse(is.na(survivalTime), 11, survivalTime), 
    category=ifelse(Mortality=="Lived", 0, 1),
    SH=log10(A.Acerv)
  )
NDSH$Combo <- as.factor(NDSH$Combo)
NDSH$Diseased <- as.factor(NDSH$Diseased)
NDSH$Nutrients <- as.factor(NDSH$Nutrients)

ND_all <-  ND_all %>% 
  mutate(
    survivalTime= `Days Survived`,
    category=ifelse(Mortality=="Lived", 0, 1),
    Disease=as.factor(Disease),
    Nutrients=as.factor(Nutrients)
  )
NDSH$Combo <- as.factor(NDSH$Combo)
NDSH$Diseased <- as.factor(NDSH$Diseased)
NDSH$Nutrients <- as.factor(NDSH$Nutrients)
# calculate the difference between start and end time in days.
### Error: Problem with `mutate()` input `timesurvived`.
### x character string is not in a standard unambiguous format
### i Input `timesurvived` is `as.numeric(difftime(Date.of.Mortality, Date.Disease, units = "days"))`.
survMod <- coxph(Surv(survivalTime, category)~Diseased+SH, data=NDSH)
summary(survMod)

survMod <- coxph(Surv(survivalTime, category)~Diseased+Nutrients, data=NDSH)
surv_pvalue(fit=survMod)
ggsurvplot(fit = survfit(Surv(survivalTime, category)~Diseased, data=NDSH))

datSH <- NDSH %>%
  filter(Diseased=='Pathogen', Timepoint=="T1")


shMod <- coxph(Surv(survivalTime, category)~SH, data=datSH)
summary(shMod)

datSH <- NDSH %>%
  filter(Diseased=='Pathogen')

ggsurvplot(fit = survfit(Surv(survivalTime, category)~log10(A.Acerv), data=datSH))

predSH <- expand.grid(SH=-seq(.5, 1.3, 0.05))
fitSH <- data.frame(predict(shMod, type='risk', se.fit=TRUE, newdata=predSH))
fitSH <- cbind(predSH, fitSH) 

ggplot(fitSH, aes(SH, fit)) +
  geom_line() +
  geom_ribbon(aes(ymin=fit-se.fit, ymax=fit+se.fit), alpha=0.3) +
  geom_hline(yintercept = 1, color='red')

survMod <- coxph(Surv(survivalTime, category)~Disease*Nutrients, data=ND_all)
summary(survMod)
survdiff(Surv(survivalTime, category)~Disease+Nutrients, data=ND_all)
ggsurvplot(fit = survfit(Surv(survivalTime, category)~Disease+Nutrients, data=ND_all))

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
