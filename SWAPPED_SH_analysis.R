# set working directory. This wd is different than the one used for calculating S/H
setwd("C:/Users/12677/OneDrive - University of Miami/Desktop/Acerv_Nutrients_project1")

# load packages
library(readr)
library(ggplot2)
library(Rmisc)

# Hypothesis #1: Corals in NH4 treatment will have elevated S/H

#clear environment
rm(list=ls())

# read in data file(s)
SWAPPED_NDSH = read.csv("SWAPPED_qPCR samples_R.csv")

#subset samples into before and after disease exposure
SWAPPED_b4disease = subset(SWAPPED_NDSH, Sequence=="OnlyNutrients")
SWAPPED_b4disease

SWAPPED_afterdisease = subset(SWAPPED_NDSH, Sequence=="Disease")

# scatterplot of S/H after nutrients but before disease
theme_set (theme_classic() + theme(panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank(), 
                                   axis.line = element_line(colour = "black"),
                                   legend.position="bottom",
                                   axis.text.x = element_text(angle = 90, vjust = 0.5),
                                   plot.title = element_text(size=12, face="bold"),
                                   #panel.border = element_rect(colour = "black", fill=NA, size=1)
                                   panel.border = element_blank()
))

SWAPPED_SHb4scatter <- ggplot(aes(x=Tag, y=A.Acerv, colour=Nutrients), data = SWAPPED_b4disease,) +
  geom_point() 
SWAPPED_SHb4scatter

# Basic box plot for before and after disease
SWAPPED_SHboxb4 <- ggplot(SWAPPED_b4disease, aes(x=Nutrients, y=A.Acerv)) + 
  geom_boxplot()
SWAPPED_SHboxb4

SWAPPED_SHboxafter <- ggplot(SWAPPED_afterdisease, aes(x=Nutrients, y=A.Acerv)) + 
  geom_boxplot()
SWAPPED_SHboxafter

#Using tapply to find the average S/H for each Nutrients treatment
tapply(SWAPPED_b4disease$A.Acerv,SWAPPED_b4disease$Nutrients,mean)
tapply(SWAPPED_afterdisease$A.Acerv, SWAPPED_afterdisease$Nutrients,mean)

# summary of data before disease exposure
SWAPPED_summaryb4=summarySE(SWAPPED_b4disease,measurevar = 'A.Acerv',groupvars = c('Nutrients'))
SWAPPED_summaryb4

#summary of data after disease exposure
SWAPPED_summaryafter=summarySE(SWAPPED_afterdisease,measurevar='A.Acerv',groupvars=c('Nutrients'))
SWAPPED_summaryafter

# bar graph of Ambient vs NH4 treatments before diease exposure
SWAPPED_SHbarb4 <- ggplot(SWAPPED_summaryb4, aes(x=Nutrients, y=A.Acerv))
SWAPPED_SHbarb4 <- SWAPPED_SHbarb4+geom_col(fill='lightgray')
SWAPPED_SHbarb4 =SWAPPED_SHbarb4+geom_errorbar(aes(ymin=A.Acerv-se, ymax=A.Acerv+se),width=.1,size=0.5)
SWAPPED_SHbarb4

# bar graph of Ambient vs NH4 treatments after diease exposure
SWAPPED_SHbarafter <- ggplot(SWAPPED_summaryafter, aes(x=Nutrients, y=A.Acerv))
SWAPPED_SHbarafter <- SWAPPED_SHbarafter+geom_col(fill='lightgray')
SWAPPED_SHbarafter = SWAPPED_SHbarafter+geom_errorbar(aes(ymin=A.Acerv-se, ymax=A.Acerv+se),width=.1,size=0.5)
SWAPPED_SHbarafter

# using tapply to determine if variance is the same before disease
tapply(SWAPPED_b4disease$A.Acerv, SWAPPED_b4disease$Nutrients, var)

# variances are approximately the same. We can use the Bartlett test to test if variances are significantly different 
bartlett.test(SWAPPED_b4disease$A.Acerv ~ SWAPPED_b4disease$Nutrients)

# ANOVA
SWAPPED_b4ANOVA=aov(A.Acerv~Nutrients,SWAPPED_b4disease)
summary(SWAPPED_b4ANOVA)

# Shapiro Test for normal distribution
shapiro.test(resid(SWAPPED_b4ANOVA))

# qq-plot
qqnorm(resid(SWAPPED_b4ANOVA))
qqline(resid(SWAPPED_b4ANOVA,lty=2))

### Distribution is not normal. Log transform or use Kruskal-Wallace test?

# specific t-test to see if S/H for Ambient is significantly greater than NH4
# create vectors for each treatment
SWAPPED_b4Ambient=SWAPPED_b4disease$A.Acerv[SWAPPED_b4disease$Nutrients=='Ambient']
SWAPPED_b4Ambient

SWAPPED_b4NH4=SWAPPED_b4disease$A.Acerv[SWAPPED_b4disease$Nutrients=='NH4']
SWAPPED_b4NH4

#now we can do a one-sided t-test: testing if the swapped NH4 treatment is significantly higher than the ambient treatment
t.test(SWAPPED_b4NH4,SWAPPED_b4Ambient,alternative = 'greater',var.equal=TRUE)

#report the results as "A one-sided T-test found that the S/H ratio is not significantly higher for corals exposed to the NH4 treatment than the Ambient treatment.

#### *****this next part is important*****

# summary of data before disease exposure by genotype
SWAPPED_summaryb4geno=summarySE(SWAPPED_b4disease,measurevar = 'A.Acerv',groupvars = c('Genotype','Nutrients'))
SWAPPED_summaryb4geno

#grouped bar plot by genotype
SWAPPED_genobar = ggplot(SWAPPED_summaryb4geno, aes(fill=Nutrients, y=A.Acerv, x=Genotype)) + 
  geom_bar(position='dodge', stat='identity')

SWAPPED_genobar = SWAPPED_genobar+geom_errorbar(aes(ymin=A.Acerv-se, ymax=A.Acerv+se), width=.1, size= 0.5, 
                                position=position_dodge(.9))
SWAPPED_genobar

