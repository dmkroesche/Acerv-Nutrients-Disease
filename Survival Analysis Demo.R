# set working directory. This wd is different than the one used for calculating S/H
setwd("C:/Users/12677/OneDrive - University of Miami/Desktop/Acerv_Nutrients_project1")

#clear environment
rm(list=ls())

# time: survival time in days
# status: censoring status: 1= censored, 2= dead
# sex: Male=1 Female=2
# censoring- occurs if a subject has not experienced the event of interest by the end of data collection

# Dealing with dates in R (instead of calculated survival times)

date_ex <- 
  tibble(
    sx_date = c("2007-06-22", "2004-02-13", "2010-10-27"), 
    last_fup_date = c("2017-04-15", "2018-07-04", "2016-10-31")
  )

date_ex

# the above variables show up as characters, but we need them as dates
#use the next line of code to format as dates in a tibble
date_ex %>% 
  mutate(
    sx_date = as.Date(sx_date, format = "%Y-%m-%d"), 
    last_fup_date = as.Date(last_fup_date, format = "%Y-%m-%d") 
  )

# if your date is in format m/d/Y then you would need format = "%m/%d/%Y"
# You can also use the lubridate package to format dates
date_ex %>% 
  mutate(
    sx_date = ymd(sx_date), 
    last_fup_date = ymd(last_fup_date)
  )

# Now that dates have been formatted, we need to calculate the difference between start and end time in some units.  In base R, use difftime to calculate the number of days between our two dates and convert it to a numeric value using as.numeric. Then convert to years by dividing by 365.25, the average number of days in a year.
date_ex %>% 
  mutate(
    os_yrs = 
      as.numeric(
        difftime(last_fup_date, 
                 sx_date, 
                 units = "days")) / 365.25
  )

# calculate survival times using lubridate
date_ex %>% 
  mutate(
    os_yrs = 
      as.duration(sx_date %--% last_fup_date) / dyears(1)
  )

# event indicator: 1 if event observed (i.e. Ti≤Ci), 0 if censored (i.e. Ti>Ci) (or 1 and 2, or True and False)
# Survival function - The probability that a subject will survive beyond any given specified time: S(t)=Pr(T>t)=1−F(t)

# Survival probability at a certain time, S(t), is a conditional probability of surviving beyond that time, given that an individual has survived just prior to that time.
# Survival probability = number alive/number alive just prior (previous day)
# The Kaplan-Meier estimate of survival probability is the product of these conditional probabilities up until that time. results in a step function, where there is a step down each time an event occurs.

as_tibble(lung)
lung <- as_tibble(lung)
lung
Surv(lung$time, lung$status)[1:10]
# followed by a + if the subject was censored

#  survfit function creates survival curves based on a formula
f1 <- survfit(Surv(time, status) ~ 1, data = lung)
names(f1)
# plot survfit
plot(survfit(Surv(time, status) ~ 1, data = lung), 
     xlab = "Days", 
     ylab = "Overall survival probability")



###############################################################################

library(survival)
?lung

head(lung)
class(lung)
dim(lung)
View(lung)

# nicer view of data
as_tibble(lung)
lung <- as_tibble(lung)
lung

?Surv

s <- Surv(lung$time, lung$status)
class(s)
s
head(lung)

?survfit

survfit(s~1)
survfit(Surv(time, status)~1, data=lung)
sfit <- survfit(Surv(time, status)~1, data=lung)
sfit

#shows a life table
summary(sfit)

# fit survival curves separately by sex
sfit <- survfit(Surv(time, status)~sex, data=lung)
sfit
summary(sfit)

?summary.survfit

# create a sequence of numbers going from one number to another number by increments of yet another number with the seq() function
# ?summary.survfit
range(lung$time)
seq(0, 1100, 100)
summary(sfit, times=seq(0, 1000, 100))

# visualize it with a Kaplan-Meier plot.
sfit <- survfit(Surv(time, status)~sex, data=lung)
plot(sfit)

#package similar to ggplot
library(survminer)
ggsurvplot(sfit)

#Really good plot with color, legend, confidence interval etc. 
ggsurvplot(sfit, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c("Male", "Female"), legend.title="Sex",  
           palette=c("dodgerblue2", "orchid2"), 
           title="Kaplan-Meier Curve for Lung Cancer Survival", 
           risk.table.height=.15)

# Exercise Set 1

## This dataset has survival and recurrence information on 929 people from a clinical trial on colon cancer chemotherapy. There are two rows per person, indidicated by the event type (etype) variable – etype==1 indicates that row corresponds to recurrence; etype==2 indicates death. 

?colon

library(dplyr)
colon <- as_tibble(colon)
colondeath <- filter(colon, etype==2)

# Or, using base subset()
# colondeath <- subset(colon, etype==2)
?head
head(colondeath)

?colon

survfit(Surv(..., ...,)~..., data=colondeath)
