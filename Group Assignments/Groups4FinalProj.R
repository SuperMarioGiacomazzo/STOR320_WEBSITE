setwd("D:/Mario Documents/UNC/STOR 320/STOR320_WEBSITE/Group Assignments")

library(tidyverse)
library(readxl)

#Read Rosters
Section=read_excel("STOR320_Roster.xlsx")

#Extract By Class
Sophomore = Section$Name[which(Section$Year=="Sophomore")]
Junior = Section$Name[which(Section$Year=="Junior")]
Senior = Section$Name[which(Section$Year=="Senior")]

#Create Key Groups
set.seed(216)
Group1=c(Sophomore[1],sample(Senior,4))
set.seed(480)
Group2=c(Sophomore[2],sample(Senior[-which(Senior %in% Group1)],4))
set.seed(919)
Group6=sample(Senior[-which(Senior %in% c(Group1,Group2))],4)
set.seed(330)
Group3=c(sample(Senior[-which(Senior %in% c(Group1,Group2,Group6))],1),sample(Junior,4))
set.seed(440)
Group4=sample(Junior[-which(Junior %in% c(Group3))],5)
set.seed(661)
Group5=sample(Junior[-which(Junior %in% c(Group3,Group4))],5)

#Save Groups
Section$Group=NA
Section$Group[which(Section$Name %in% Group1)]=1
Section$Group[which(Section$Name %in% Group2)]=2
Section$Group[which(Section$Name %in% Group3)]=3
Section$Group[which(Section$Name %in% Group4)]=4
Section$Group[which(Section$Name %in% Group5)]=5
Section$Group[which(Section$Name %in% Group6)]=6
Section$Role="TBD"

#Drop Unimportant Variables
Section$Location=NULL
Section$Year=NULL

#Save Datasets
write_csv(Section,path=str_c(getwd(),"/STOR320 Group Assignments.csv"))
