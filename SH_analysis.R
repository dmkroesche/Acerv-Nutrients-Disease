
setwd("C:/Users/12677/OneDrive - University of Miami/Desktop/Acerv_Nutrients_project1")

NDSH = read.csv("Nutrients S.H.csv")
T0 = subset(NDSH, Timepoint=T1)
T0

library(ggplot2)
# Basic box plot
SHbox <- ggplot(T0, aes(x=1, y=A.Acerv)) + 
  geom_boxplot()
SHbox
