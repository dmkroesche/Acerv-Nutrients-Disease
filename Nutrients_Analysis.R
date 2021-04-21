# set working directory. This wd is different than the one used for calculating S/H
setwd("C:/Users/12677/OneDrive - University of Miami/Desktop/Acerv_Nutrients_project1")

# load packages
library(readr)
library(ggplot2)
library(Rmisc)

#clear environment
rm(list=ls())

# read in data file(s)
NH4data = read.csv("ERL_Nuts_Compiled_R.csv")


# Nutrients line graph
NH4_line=ggplot(NH4data, aes(x=Timepoint, y=NH4, group=Tank, color=Tank)) +
  geom_line()
NH4_line=NH4_line+scale_fill_brewer(palette="Spectral")
NH4_line=NH4_line+geom_point(size=1)
NH4_line=NH4_line+labs(y='NH4 (uM)', title='Ammonium Concentrations')
NH4_line

# NH4 line graph grouped by treatment
NH4_group=ggplot(NH4data, aes(x=Timepoint, y=NH4, group=Treatment, color=Treatment)) +
  geom_line()
NH4_group=NH4_group+scale_fill_brewer(palette="Spectral")
NH4_group=NH4_group+geom_point(size=1)
NH4_group=NH4_group+labs(y='NH4 (uM)', title='Ammonium Concentrations')
NH4_group


# Graph other Nutrients
Nutdata = read.csv("ERL_Nuts_other_R.csv")
class(Nutdata$Tank)="character"

# Phosphate line graph
PO4_line=ggplot(Nutdata, aes(x=Timepoint, y=PO4, group=Tank, color=Tank)) +
  geom_line()
PO4_line=PO4_line+scale_fill_brewer(palette="Spectral")
PO4_line=PO4_line+geom_point(size=1)
PO4_line=PO4_line+labs(y='PO4 (uM)', title='Phosphate Concentrations')
PO4_line

# Nitrate line graph
NO3_line=ggplot(Nutdata, aes(x=Timepoint, y=NO3, group=Tank, color=Tank)) +
  geom_line()
NO3_line=NO3_line+scale_fill_brewer(palette="Spectral")
NO3_line=NO3_line+geom_point(size=1)
NO3_line=NO3_line+labs(y='NO3 (uM)', title='Nitrate Concentrations')
NO3_line

# find average difference in ammonium (1) overall (2) before disease phase


