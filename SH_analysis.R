
# set working directory. This wd is different than the one used for calculating S/H
setwd("C:/Users/12677/OneDrive - University of Miami/Desktop/Acerv_Nutrients_project1")

# load packages
library(readr)
library(ggplot2)
library(Rmisc)

#clear environment
rm(list=ls())

# read in data file(s)
# "Cleaned up T1 Nutrients s.H .csv contains T1 data with outliers and duplicates removed
NDSH = read.csv("Cleaned up T1 Nutrients S.H .csv")
T1 = subset(NDSH, Timepoint=T1)
T1

# attach function used so I don't need to type T1$variable.name each time. Make sure to detach later
attach(T1)

# Basic box plot
SHbox <- ggplot(T1, aes(x=Nutrients, y=A.Acerv)) + 
  geom_boxplot()
SHbox

#Using tapply to find the average S/H for each Nutrients treatment
tapply(A.Acerv,Nutrients,mean)

# summary of data
summary=summarySE(T1,measurevar = 'A.Acerv',groupvars = c('Nutrients'))
summary

# bar graph 
SHbar <- ggplot(summary, aes(x=Nutrients, y=A.Acerv))
SHbar <- SHbar+geom_col(fill='lightgray')
SHbar=SHbar+geom_errorbar(aes(ymin=A.Acerv-se, ymax=A.Acerv+se),width=.1,size=0.5)
SHbar

# using tapply to determine if variance is the same
tapply(A.Acerv, Nutrients, var)

# variances are approximately the same. We can use the Bartlett test to test if variances are significantly different 
bartlett.test(A.Acerv ~ Nutrients)
# Bartlett test p-value = 0.7929, variances are not significantly different. So we can use a parametric test

# general t-test to test if the difference in the means is significant
t.test(A.Acerv ~ Nutrients, var.equal=TRUE)
# p-value = 0.1689 is not below 0.05 :( no significant difference

# specific t-test to see if S/H for Ambient is significantly greater than NH4
# create vectors for each treatment
####### Error: object of type 'closure' is not subsettable
Ambient=A.Acerv[data$Nutrients=='Ambient']
#here we do the same for green
NH4=A.Acerv[data$Nutrients=='NH4']
#now we can do a one-sided t-test
t.test(Ambient,NH4,alternative = 'greater',var.equal=TRUE)
#report the results as A one-sided T-test found ... significantly more Ambient treatment than NH4
# (13+/-4 v 5+/-2) T(18)=-4.57, P=0.0001

