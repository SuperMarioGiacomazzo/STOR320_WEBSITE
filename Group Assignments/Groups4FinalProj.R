setwd("D:/Mario Documents/UNC/STOR 320/STOR320_WEBSITE/Group Assignments")

library(tidyverse)
library(readxl)

#Read Rosters
LAB0=read_excel("Lab 400.xlsx")
LAB1=read_excel("Lab 401.xlsx")
LAB2=read_excel("Lab 402.xlsx")
LAB3=read_excel("Lab 403.xlsx")
LAB4=read_excel("Lab 404.xlsx")
LAB5=read_excel("Lab 405.xlsx")
LAB6=read_excel("Lab 406.xlsx")
LAB7=read_excel("Lab 407.xlsx")

#Gather Names
LAB0_NAMES=LAB0[6:dim(LAB0)[1],1]$STOR320.001.SP22
LAB1_NAMES=LAB1[6:dim(LAB1)[1],1]$STOR320.001.SP22
LAB2_NAMES=LAB2[6:dim(LAB2)[1],1]$STOR320.001.SP22
LAB3_NAMES=LAB3[6:dim(LAB3)[1],1]$STOR320.001.SP22
LAB4_NAMES=LAB4[6:dim(LAB4)[1],1]$STOR320.002.SP22
LAB5_NAMES=LAB5[6:dim(LAB5)[1],1]$STOR320.002.SP22
LAB6_NAMES=LAB6[6:dim(LAB6)[1],1]$STOR320.002.SP22
LAB7_NAMES=LAB7[6:dim(LAB7)[1],1]$STOR320.002.SP22

#Random Assignment
set.seed(216,sample.kind="Rejection")
LAB0_NAMES_2=sample(LAB0_NAMES,size=length(LAB0_NAMES))
LAB1_NAMES_2=sample(LAB1_NAMES,size=length(LAB1_NAMES))
LAB2_NAMES_2=sample(LAB2_NAMES,size=length(LAB2_NAMES))
LAB3_NAMES_2=sample(LAB3_NAMES,size=length(LAB3_NAMES))
LAB4_NAMES_2=sample(LAB4_NAMES,size=length(LAB4_NAMES))
LAB5_NAMES_2=sample(LAB5_NAMES,size=length(LAB5_NAMES))
LAB6_NAMES_2=sample(LAB6_NAMES,size=length(LAB6_NAMES))
LAB7_NAMES_2=sample(LAB7_NAMES,size=length(LAB7_NAMES))


#Lab Labels
LAB0_LABEL=rep("Lab 400",length=length(LAB0_NAMES_2))
LAB1_LABEL=rep("Lab 401",length=length(LAB1_NAMES_2))
LAB2_LABEL=rep("Lab 402",length=length(LAB2_NAMES_2))
LAB3_LABEL=rep("Lab 403",length=length(LAB3_NAMES_2))
LAB4_LABEL=rep("Lab 404",length=length(LAB4_NAMES_2))
LAB5_LABEL=rep("Lab 405",length=length(LAB5_NAMES_2))
LAB6_LABEL=rep("Lab 406",length=length(LAB6_NAMES_2))
LAB7_LABEL=rep("Lab 407",length=length(LAB7_NAMES_2))

#Bring Names with Lables

GROUPS.SECTION1=as.data.frame(rbind(
  cbind(LAB0_LABEL,LAB0_NAMES_2),
  cbind(LAB1_LABEL,LAB1_NAMES_2),
  cbind(LAB2_LABEL,LAB2_NAMES_2),
  cbind(LAB3_LABEL,LAB3_NAMES_2)
))

GROUPS.SECTION2=as.data.frame(rbind(
  cbind(LAB4_LABEL,LAB4_NAMES_2),
  cbind(LAB5_LABEL,LAB5_NAMES_2),
  cbind(LAB6_LABEL,LAB6_NAMES_2),
  cbind(LAB7_LABEL,LAB7_NAMES_2)
))

#Save Datasets
write_csv(GROUPS.SECTION1,path=str_c(getwd(),"/STOR320.001 Group Assignments.csv"))
write_csv(GROUPS.SECTION2,path=str_c(getwd(),"/STOR320.002 Group Assignments.csv"))
