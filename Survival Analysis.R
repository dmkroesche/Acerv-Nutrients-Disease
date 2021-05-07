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

#################################################################################

# ***********NDSH data frame**************

#  "coxph" = Cox Fit Proportional Hazards Regression Model
# this one is a Cox PH Model of disease and S/H Ratio on survival time
# NOTE: Tank 2 already removed
?coxph

#tell R treatment order. order data frame 
levels(NDSH$Diseased)
?factor
NDSH$Diseased <- factor(NDSH$Diseased, levels=c('Placebo', 'Pathogen'))
levels(NDSH$Diseased)

# do stats for each variable separately - Ana suggestion

# Coxph #1: disease+SH
# Disease matters for survival, SH doesn't 
survMod_dis_SH <- coxph(Surv(survivalTime, category)~Diseased+SH, data=NDSH)
survMod_dis_SH
summary(survMod_dis_SH)

# Coxph #2: diseased*SH
survMod_dis_SH2 <- coxph(Surv(survivalTime, category)~Diseased*SH, data=NDSH)
survMod_dis_SH2
summary(survMod_dis_SH2)

# Kaplan-Meier Plot #1: survival probability curve of pathogen vs. placebo over time
# *** better if done with ND_all data
ggsurvplot(fit = survfit(Surv(survivalTime, category)~Diseased, data=NDSH))+
labs(title="A. cervicornis Survivorship by Disease Treatment")

# datSH is a subset of NDSH with only Pathogen treatment fragments. 
datSH <- NDSH %>%
  filter(Diseased=='Pathogen', Timepoint=="T1")

# Coxph #3: of just disease treatment fragments. SH not important for survival within the disease treatment.
shMod <- coxph(Surv(survivalTime, category)~SH, data=datSH)
summary(shMod)


# This shows how SH does not predict relative risk
predSH <- expand.grid(SH=-seq(.5, 1.3, 0.05))
fitSH <- data.frame(predict(shMod, type='risk', se.fit=TRUE, newdata=predSH))
fitSH <- cbind(predSH, fitSH) 

ggplot(fitSH, aes(SH, fit)) +
  geom_line() +
  geom_ribbon(aes(ymin=fit-se.fit, ymax=fit+se.fit), alpha=0.3) +
  geom_hline(yintercept = 1, color='red')

#######################################################################################

# ***********ND_all data frame**************

# This has more data than NDSH. 
# Use this for everything except SH (for obvious reasons)

# order ND_all$Disease
levels(ND_all$Disease)
?factor
ND_all$Disease <- factor(ND_all$Disease, levels=c('Placebo', 'Pathogen'))
levels(NDSH$Diseased)

# Coxph #4: don't use this because tank 2 was not removed yet. Redo with tank 2 removed
survMod <- coxph(Surv(survivalTime, category)~Disease*Nutrients, data=ND_all)
summary(survMod)
survdiff(Surv(survivalTime, category)~Disease+Nutrients, data=ND_all)
ggsurvplot(fit = survfit(Surv(survivalTime, category)~Disease+Nutrients, data=ND_all))+
labs(title = "A. cervicornis Survivorship by Nutrients-Disease Combination")

###############################################################################

#Tank 2 taken out for everything below. This is important because tank 2 behaved strangely (probiotic? idk)

#Take out tank 2
dat <- ND_all %>%
  filter(Tank!=2| is.na(Tank))

#mutate(Treatment=ifelse(Tank==2, "Probiotic", "Nothing"))

#tell R treatment order. order data frame 
levels(dat$Disease)
dat$Disease <- factor(dat$Disease, levels=c('Placebo', 'Pathogen'))
levels(NDSH$Diseased)

# Coxph #5: of Nutrients Disease interaction ******** use this
survMod <- coxph(Surv(survivalTime, category)~Disease*Nutrients, data=dat)
survMod
summary(survMod)

# Coxph #6: Just disease vs placebo ***** use this
survMod2 <- coxph(Surv(survivalTime, category)~Disease, data=dat)
survMod2
summary(survMod2)

# Kaplan-Meier Plot #2: survival probability curve of pathogen vs. placebo over time
ggsurvplot(fit = survfit(Surv(survivalTime, category)~Disease, data=dat))+
  labs(title="A. cervicornis Survivorship by Disease Treatment")

# Kaplan-Meier plot #3: Nutrients and disease treatment w. tank 2 removed
# ********** use this
Fill.colour<-c ("light blue", "red","dark blue","dark red")

All<-ggsurvplot((fit = survfit(Surv(survivalTime, category)~Disease+Nutrients, data=dat)), 
                palette=Fill.colour)+
  labs(title = "A. cervicornis Survivorship by Nutrients-Disease Combination")
All  

# Kaplan-Meier plot #3: "Probiotic" plot ******* put this on hold for now
datPro <- ND_all %>%
  mutate(Treatment=as.factor(ifelse(Tank==2, "Probiotic", "noPri")))

proMod <- coxph(Surv(survivalTime, category)~Disease+Treatment, data=datPro)
summary(proMod)

ggsurvplot(fit = survfit(Surv(survivalTime, category)~Disease+Treatment, data=datPro))


#******* If I have time, maybe make some nutrients, disease survival plots using Ana's data****
# lots of mortality during Nutrients only phase. Including that might make this make more sense

######################################################################################################

# make a plot with seperate survivorship curves for each genotype

#below copied and edited from Ana's github

# Genotype-Treatment model

dat$Treatment<-paste(dat$Nutrients, dat$Disease, sep = "-" )
dat <- dat %>%
  filter(Genotype!="U41"| is.na(Genotype))

# Kaplan-Meier estimator. The "log-log" confidence interval is preferred.
fit2 <- survfit(Surv(survivalTime, category) ~ Genotype + Treatment, data = dat)
summary(fit2)

# Plot the survival model
GenTre<-ggsurvplot(fit2, data = dat, pval = TRUE,
                   risk.table=F,  tables.height=0.5)
GenTre

ggsurvplot_facet(fit2, data = dat, facet.by="Treatment", 
                 #risk.table=T, tables.height=0.5, 
                 nrow = 6, alpha=1,
                 linetype=1) +
  geom_vline(xintercept = 46, linetype="dashed", color = "gray")

ggsurvplot_facet(fit2, data = dat, 
                 facet.by="Genotype", 
                 # risk.table=T, tables.height=0.5, 
                 nrow = 3, alpha=1, linetype=1) +
  geom_vline(xintercept = 46, linetype="dashed", color = "gray")+
  scale_color_manual(values = c("red", "light blue","dark red","dark blue"))+
  labs(title="Survivorship by Genotype: Disease Phase")

# Cox PH #6: Genotype*Treatment ********** genuinely confused about what I did here. 
survMod_geno <- coxph(Surv(survivalTime, category)~Genotype*Treatment, data=dat)
survMod_geno
summary(survMod_geno)

# Cox PH #7:
survMod_geno2 <- coxph(Surv(survivalTime, category)~Genotype+Treatment, data=dat)
survMod_geno2
summary(survMod_geno2)

# Cox PH #7:
survMod_geno3 <- coxph(Surv(survivalTime, category)~Genotype, data=dat)
survMod_geno3
summary(survMod_geno3)


#######################################################################################

# same thing as above, using Ana's .csv so that it includes nutrients only phase 

# Import and organize data

# Data
Survival.data<-read.csv("Acer_Mortality2.csv", header = TRUE)
summary(Survival.data)
Survival.data$Date<-as.Date(Survival.data$Date)
Survival.data$Day<-as.numeric(Survival.data$Date)-18518
Survival.data$Fu.time_texp<-Survival.data$Day
Survival.data$Treatment<-paste(Survival.data$Nutrients, Survival.data$Disease, sep = "-" )
Survival.data <- Survival.data %>%
  filter(Genotype!="U41"| is.na(Genotype))
summary(Survival.data$Genotype)

#Survival.data$Genotype<-factor 
#   (Survival.data$Genotype, 
#   levels=c("G_48", "G_62","G_31", "G_08","G_07", "G_50"))
Survival.data$Genotype<-as.factor(Survival.data$Genotype)
summary(Survival.data$Genotype)

## Model 2 (Genotype-Treatemnt) with experimnatl data (excluding cooked tank)

# Filter data 
Survival.data_1<-Survival.data
Survival.data_1<-filter(Survival.data_1, Treatment!="Ambient-Heat")
Survival.data_1$Treatment<-factor(Survival.data_1$Treatment,
                                  levels = c("Ambient-Placebo", "Ambient-Pathogen",
                                             "NH4-Placebo", "NH4-Pathogen"))
Survival.data_1$Genotype<-factor(Survival.data_1$Genotype, 
                                 levels=c("FM19", "U44","FM6", "FM9","FM14", "Elkhorn",
                                          "K2", "Acerv2", "Kelsey-1", "Cooper-9"))
summary(Survival.data_1$Genotype)
summary(Survival.data_1$Treatment)

# Create survival object (Fit survival data using the Kaplan-Meier method)
surv_object_1 <- Surv(time = Survival.data_1$Fu.time_texp, 
                      event = Survival.data_1$Fu.stat_exp)
surv_object_1 

# Kaplan-Meier estimator. The "log-log" confidence interval is- preferred.

fit1 <- survfit(surv_object_1 ~ Treatment, data = Survival.data_1)
summary(fit1)

fit2 <- survfit(surv_object_1 ~ Genotype + Treatment, data = Survival.data_1)
summary(fit2)

# Plot the survival modelper genotye and treatment 
GenTre_1<-ggsurvplot(fit2, data = Survival.data_1, pval = TRUE,
                     risk.table=F,  tables.height=0.5)
GenTre_1

GenotypeP<-ggsurvplot_facet(fit2, data = Survival.data_1, facet.by="Treatment", 
                            risk.table=F, # tables.height=0.5, 
                            nrow = 6, alpha=1, linetype=1) +
  geom_vline(xintercept = 46, linetype="dashed", 
             color = "gray")
GenotypeP

TreatmentP<-ggsurvplot_facet(fit2, data = Survival.data_1, 
                             facet.by="Genotype", 
                             # risk.table=T, tables.height=0.5, 
                             nrow = 3, alpha=1,
                             linetype=1) +
  geom_vline(xintercept = 48, linetype="dashed", color = "gray") +
  scale_color_manual(values = c("light blue", "red","dark blue","dark red"))+
  labs(title="Survivorship by Genotype: Nutrients and Disease Phases")
TreatmentP

# Kaplan-Meier plot using Ana's data

Fill.colour<-c ("light blue", "red","dark blue","dark red")

All_genotypes<-ggsurvplot(fit1, data=Survival.data_1,
                          pval = TRUE, conf.int = F, risk.table=F,
                          palette=Fill.colour,
                          break.time.by=7, xlim=c(0,60)) +
  ggtitle("A. cervicornis Survivorship by Nutrients-Disease Combination")
All_genotypes

# Coxph: of Nutrients Disease interaction
survMod_ana <- coxph(Surv(Day, Fu.stat_exp)~Disease+Nutrients, data=Survival.data_1)
survMod_ana
summary(survMod_ana)


#########################################################################################
#Trying to change colors and add lines
#copying it here and leaving originals alone in case I fuck up

# Treatment model

dat$Treatment<-paste(dat$Nutrients, dat$Disease, sep = "-" )

# Kaplan-Meier plot by treatment using Ana's data
ggsurvplot(fit = survfit(Surv(Day, Fu.stat_exp)~Treatment, data=Survival.data_1))+
  scale_color_manual(values = c("light blue", "red","dark blue","dark red"))+
  geom_vline(xintercept = 46, linetype="dashed", 
             color = "gray")+
  labs(title = "A. cervicornis Survivorship by Nutrients-Disease Combination")

## It won't let me change the color or add lines.
## Error message: Error in ggsurvplot(fit = survfit(Surv(Day, Fu.stat_exp) ~ Treatment,  : 
## non-numeric argument to binary operator
## In addition: Warning message:
## Incompatible methods ("+.ggsurv", "+.gg") for "+" 

############################################################################################

