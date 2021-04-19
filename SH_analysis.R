
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
NDSH = read.csv("qPCR samples_R.csv")

#subset samples into before and after disease exposure
b4disease = subset(NDSH, Sequence=="OnlyNutrients")

afterdisease = subset(NDSH, Sequence=="Disease")

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

SHb4scatter <- ggplot(aes(x=Tag, y=A.Acerv, colour=Nutrients), data = b4disease,) +
  geom_point() +
  labs(title = "Symbiont Abundance by Treatment",
       y = "S/H Ratio", x = "Fragment #")
SHb4scatter

#before disease scatter plot by genotype, grouped by treatment
SHb4genoscatter <- ggplot(aes(x=Tag, y=A.Acerv, colour=Genotype, group=Nutrients), data = b4disease,) +
  geom_point() +
  facet_wrap(~ Nutrients)+
labs(title = "Symbiont Abundance by Genotype",
     y = "S/H Ratio", x = "Fragment #")
SHb4genoscatter


# after disease exposure scatter plot of S/H
SHafterscatter <- ggplot(aes(x=Tag, y=A.Acerv, colour=Nutrients), data = afterdisease) +
  geom_point() +
  labs(title = "Symbiont Abundance After Disease Exposure",
       y = "S/H Ratio", x = "Fragment #")
SHafterscatter

# Before and after disease scatter plots (not sure I'll actually include this)
SHb4afterscatter <- ggplot(aes(x=Tag, y=A.Acerv, colour=Nutrients), data = NDSH) +
  geom_point() +
  facet_wrap(~Timepoint)+
  labs(title = "Symbiont Abundance After Nutrients, Disease Exposure",
       y = "S/H Ratio", x = "Fragment #")
SHb4afterscatter

# Basic box plot for before and after disease (not sure I'll include this either)
SHboxb4 <- ggplot(b4disease, aes(x=Nutrients, y=A.Acerv)) + 
  geom_boxplot()
SHboxb4

SHboxafter <- ggplot(afterdisease, aes(x=Nutrients, y=A.Acerv)) + 
  geom_boxplot()
SHboxafter

SHboxplots <- ggplot(NDSH, aes(x=Nutrients, y=A.Acerv)) + 
  geom_boxplot()+
  facet_wrap(~ Timepoint)
SHboxplots
labs(title = "",
     y = "S/H Ratio", x = "Nutrients Treatment")

#Using tapply to find the average S/H for each Nutrients treatment
tapply(b4disease$A.Acerv,b4disease$Nutrients,mean)
tapply(afterdisease$A.Acerv, afterdisease$Nutrients,mean)

# summary of data before disease exposure
summaryb4=summarySE(b4disease,measurevar = 'A.Acerv',groupvars = c('Nutrients'))
summaryb4

#summary of data after disease exposure
summaryafter=summarySE(afterdisease,measurevar='A.Acerv',groupvars=c('Nutrients'))
summaryafter

# bar graph of Ambient vs NH4 treatments before disease exposure
SHbarb4 <- ggplot(summaryb4, aes(x=Nutrients, y=A.Acerv))
SHbarb4 <- SHbarb4+geom_col(fill='lightgray')
SHbarb4 =SHbarb4+geom_errorbar(aes(ymin=A.Acerv-se, ymax=A.Acerv+se),width=.1,size=0.5)+
labs(title = "Symbiont Abundance by Treatment",
       y = "S/H Ratio", x = "Nutrients Treatment")
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

# ANOVA
b4ANOVA=aov(A.Acerv~Nutrients,b4disease)
summary(b4ANOVA)

# Shapiro Test for normal distribution
shapiro.test(resid(b4ANOVA))

# qq-plot
qqnorm(resid(b4ANOVA))
qqline(resid(b4ANOVA,lty=2))

### Do a non-parametric instead
### Distribution is not normal. Log transform or use Kruskal-Wallace test?

# general t-test to test if the difference in the means is significant
t.test(b4disease$A.Acerv ~ b4disease$Nutrients, var.equal=TRUE)

# p-value = 0.7469 is not below 0.05, no significant difference

# specific t-test to see if S/H for Ambient is significantly greater than NH4
# create vectors for each treatment
b4Ambient=b4disease$A.Acerv[b4disease$Nutrients=='Ambient']
b4Ambient

b4NH4=b4disease$A.Acerv[b4disease$Nutrients=='NH4']
b4NH4

# specific t-test to see if S/H for Ambient is significantly greater than NH4 after diease exposure
# create vectors for each treatment
afterAmbient=afterdisease$A.Acerv[afterdisease$Nutrients=='Ambient']
afterAmbient

afterNH4=afterdisease$A.Acerv[afterdisease$Nutrients=='NH4']
afterNH4

#now we can do a one-sided t-test for before disease exposure
t.test(b4Ambient,b4NH4,alternative = 'greater',var.equal=TRUE)
#report the results as "A one-sided T-test found that the S/H ratio is not significantly higher for corals exposed to the Ambient treatment than the NH4 treatment.

#same thing for after disease exposure. One sided t-test
t.test(afterAmbient,afterNH4,alternative = 'greater',var.equal=TRUE)

# summary of data before disease exposure by genotype
summaryb4geno=summarySE(b4disease,measurevar = 'A.Acerv',groupvars = c('Genotype','Nutrients'))
summaryb4geno

#grouped bar plot by genotype
genobar = ggplot(summaryb4geno, aes(fill=Nutrients, y=A.Acerv, x=Genotype)) + 
  geom_bar(position='dodge', stat='identity')

genobar = genobar+geom_errorbar(aes(ymin=A.Acerv-se, ymax=A.Acerv+se), width=.1, size= 0.5, 
              position=position_dodge(.9))+
  labs(title = "Symbiont Abundance After Nutrient Treatments",
       y = "S/H Ratio", x = "Genotype")
genobar

genoANOVA=aov(A.Acerv~Genotype:Nutrients,b4disease)
summary(genoANOVA)

TukeyHSD(genoANOVA)

#grouped bar by genotype and tank (not enough replication per tank for error bars)
summarygenotank=summarySE(b4disease,measurevar = 'A.Acerv',groupvars = c('Genotype','Tank_D1'))
summarygenotank

class(summarygenotank$Tank_D1)="character"

genotank = ggplot(summarygenotank, aes(fill=Tank_D1, y=A.Acerv, x=Genotype)) + 
  geom_bar(position='dodge', stat='identity')

genotank = genotank+geom_errorbar(aes(ymin=A.Acerv-se, ymax=A.Acerv+se), width=.1, size= 0.5, 
                                position=position_dodge(.9))+
  labs(title = "Symbiont Abundance After Nutrient Treatments",
       y = "S/H Ratio", x = "Genotype")
genotank

# average S/H by tank
summarytank=summarySE(b4disease,measurevar = 'A.Acerv',groupvars = c('Tank_D1'))
summarytank

class(summarytank$Tank_D1)="character"

SHtank <- ggplot(summarytank, aes(x=Tank_D1, y=A.Acerv))
SHtank <- SHtank+geom_col(fill='lightgray')
SHtank =SHtank+geom_errorbar(aes(ymin=A.Acerv-se, ymax=A.Acerv+se),width=.1,size=0.5)+
  labs(title = "Mean Symbiont Abundance by Tank",
       y = "S/H Ratio", x = "Tank")

SHtank


#######################################################



# Hypothesis #2: Higher algal densities = higher disease mortality

# S/H vs Mortality scatter plot
mortalityscatter <- ggplot(aes(x=Tag, y=A.Acerv, colour=Mortality), data = b4disease,) +
  geom_point() 
mortalityscatter

#### Rich's suggestion: parse them out by survivorship, using nutrients as a predictor, and use S/H and genotype as co-variates 

# step 1: parse out by survivorship


#subset samples taken before disease exposure by Mortality fates
?subset
b4lived = subset(b4disease, Mortality=='Lived')
b4lived

b4died = subset(b4disease, Mortality=='Died')
b4died

# summary of S/H for corals the survived and died. S/H is almost the same for all. 
summarylived=summarySE(b4lived,measurevar = 'A.Acerv',groupvars = c('Nutrients'))
summarylived

summarydied=summarySE(b4died,measurevar='A.Acerv', groupvars=c('Nutrients'))
summarydied

mort_count = tapply(b4disease$Mortality,b4disease$Nutrients,count)

# graph of mortality by genotype
mortalitybar=ggplot(b4disease,aes(x=Mortality))
mortalitybar=mortalitybar+geom_bar(aes(y = (..count..)/sum(..count..)))  
mortalitybar=mortalitybar+scale_y_continuous(labels=scales::percent)
mortalitybar=mortalitybar+labs(y='Mortality, %')+
facet_wrap(~ Nutrients)
mortalitybar

# mortality over time
### error
class(b4disease$Date.of.Mortality)="date"


ggplot(data = b4disease, aes(Date.of.Mortality, n, color = Genotype)) +
  geom_line(size = 1) +
  geom_point() + 
  labs(title = "Mortality by Genotype",
       y = "Count of coral deaths", x = "")

    
