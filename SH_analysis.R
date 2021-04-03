
# set working directory. This wd is different than the one used for calculating S/H
setwd("C:/Users/12677/OneDrive - University of Miami/Desktop/Acerv_Nutrients_project1")

# load packages
library(readr)
library(ggplot2)
library(Rmisc)

#clear environment
rm(list=ls())

# read in data file(s)
NDSH = read.csv("qPCR samples_R.csv")

#subset samples into before and after disease exposure
b4disease = subset(NDSH, Sequence=="OnlyNutrients")
b4disease

afterdisease = subset(NDSH, Sequence=="Disease")

# scatterplot of S/H after nutrients but before disease


# Basic box plot for before and after disease
SHboxb4 <- ggplot(b4disease, aes(x=Nutrients, y=A.Acerv)) + 
  geom_boxplot()
SHboxb4

SHboxafter <- ggplot(afterdisease, aes(x=Nutrients, y=A.Acerv)) + 
  geom_boxplot()
SHboxafter

#Using tapply to find the average S/H for each Nutrients treatment
tapply(b4disease$A.Acerv,b4disease$Nutrients,mean)
tapply(afterdisease$A.Acerv, afterdisease$Nutrients,mean)

# summary of data before disease exposure
summaryb4=summarySE(b4disease,measurevar = 'A.Acerv',groupvars = c('Nutrients'))
summaryb4

#summary of data after disease exposure
summaryafter=summarySE(afterdisease,measurevar='A.Acerv',groupvars=c('Nutrients'))
summaryafter

# bar graph of Ambient vs NH4 treatments before diease exposure
SHbarb4 <- ggplot(summaryb4, aes(x=Nutrients, y=A.Acerv))
SHbarb4 <- SHbarb4+geom_col(fill='lightgray')
SHbarb4 =SHbarb4+geom_errorbar(aes(ymin=A.Acerv-se, ymax=A.Acerv+se),width=.1,size=0.5)
SHbarb4

# bar graph of Ambient vs NH4 treatments after diease exposure
SHbarafter <- ggplot(summaryafter, aes(x=Nutrients, y=A.Acerv))
SHbarafter <- SHbarafter+geom_col(fill='lightgray')
SHbarafter =SHbarafter+geom_errorbar(aes(ymin=A.Acerv-se, ymax=A.Acerv+se),width=.1,size=0.5)
SHbarafter

# using tapply to determine if variance is the same before disease
tapply(b4disease$A.Acerv, b4disease$Nutrients, var)

# variances are approximately the same. We can use the Bartlett test to test if variances are significantly different 
bartlett.test(b4disease$A.Acerv ~ b4disease$Nutrients)
# Bartlett test p-value = 0.1876, variances are not significantly different. So we can use a parametric test

# general t-test to test if the difference in the means is significant
t.test(b4disease$A.Acerv ~ b4disease$Nutrients, var.equal=TRUE)
# p-value = 0.7469 is not below 0.05, no significant difference

# specific t-test to see if S/H for Ambient is significantly greater than NH4
# create vectors for each treatment
b4Ambient=A.Acerv[b4disease$Nutrients=='Ambient']
b4Ambient

b4NH4=A.Acerv[b4disease$Nutrients=='NH4']
b4NH4

# specific t-test to see if S/H for Ambient is significantly greater than NH4 after diease exposure
# create vectors for each treatment
afterAmbient=A.Acerv[afterdisease$Nutrients=='Ambient']
afterAmbient

afterNH4=A.Acerv[afterdisease$Nutrients=='NH4']
afterNH4

#now we can do a one-sided t-test for before disease exposure
t.test(b4Ambient,b4NH4,alternative = 'greater',var.equal=TRUE)
#report the results as "A one-sided T-test found that the S/H ratio is not significantly higher for corals exposed to the Ambient treatment than the NH4 treatment.

#same thing for after disease exposure. One sided t-test
t.test(afterAmbient,afterNH4,alternative = 'greater',var.equal=TRUE)

#ANOVA

