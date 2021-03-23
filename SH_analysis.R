
setwd("C:/Users/12677/OneDrive - University of Miami/Desktop/Acerv_Nutrients_project1")

NDSH = read.csv("Cleaned up Nutrients S.H.csv")
T1 = subset(NDSH, Timepoint=T1)
T1

library(ggplot2)
# Basic box plot
SHbox <- ggplot(T1, aes(x=Nutrients, y=A.Acerv)) + 
  geom_boxplot()
SHbox
