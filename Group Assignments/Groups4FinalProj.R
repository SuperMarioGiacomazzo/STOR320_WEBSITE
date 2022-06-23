setwd("D:/Mario Documents/UNC/STOR 320/STOR320_WEBSITE/Group Assignments")

library(tidyverse)
library(readxl)

#Read Rosters
LAB0=read_excel("Lab 400.xlsx")
LAB1=read_excel("Lab 401.xlsx")

#Gather Names
LAB0_NAMES=LAB0[1:dim(LAB0)[1],c(1,2)]
LAB1_NAMES=LAB1[1:dim(LAB1)[1],c(1,2)]

#Random Assignment
set.seed(216,sample.kind="Rejection")
LAB0_NAMES_2=LAB0_NAMES[sample(1:dim(LAB0)[1],size=dim(LAB0)[1]),]
LAB1_NAMES_2=LAB1_NAMES[sample(1:dim(LAB1)[1],size=dim(LAB1)[1]),]


#Lab Labels
LAB0_LABEL=rep("Lab 400",length=dim(LAB0)[1])
LAB1_LABEL=rep("Lab 401",length=dim(LAB1)[1])

#Bring Names with Labels
GROUPS.SECTION1=as.data.frame(rbind(
  cbind(LAB=LAB0_LABEL,LAB0_NAMES_2),
  cbind(LAB=LAB1_LABEL,LAB1_NAMES_2)
))

#Save Datasets
write_csv(GROUPS.SECTION1,file="STOR320.001 Group Assignments.csv")
