setwd("D:/Mario Documents/UNC/STOR 320/STOR320_WEBSITE/Group Assignments")

library(tidyverse)
library(readxl)

#Read Rosters
LAB3=read_excel("STOR_320_403.xlsx")
LAB5=read_excel("STOR_320_405.xlsx")
LAB6=read_excel("STOR_320_406.xlsx")
LAB7=read_excel("STOR_320_407.xlsx")

#Gather Names
LAB3_NAMES=LAB3[6:dim(LAB3)[1],1]$STOR320.002.FA20
LAB5_NAMES=LAB5[6:dim(LAB5)[1],1]$STOR320.002.FA20
LAB6_NAMES=LAB6[6:dim(LAB6)[1],1]$STOR320.002.FA20
LAB7_NAMES=LAB7[6:dim(LAB7)[1],1]$STOR320.002.FA20

#Random Assignment
set.seed(216,sample.kind="Rejection")
LAB3_NAMES_2=sample(LAB3_NAMES,size=length(LAB3_NAMES))
LAB5_NAMES_2=sample(LAB5_NAMES,size=length(LAB5_NAMES))
LAB6_NAMES_2=sample(LAB6_NAMES,size=length(LAB6_NAMES))
LAB7_NAMES_2=sample(LAB7_NAMES,size=length(LAB7_NAMES))

#Group Numbers
LAB3_GROUPS=rep(1:5,length=length(LAB3_NAMES_2))
LAB5_GROUPS=rep(6:10,length=length(LAB5_NAMES_2))
LAB6_GROUPS=rep(11:15,length=length(LAB6_NAMES_2))
LAB7_GROUPS=rep(16:19,length=length(LAB7_NAMES_2))

#Bring Names with Groups

GROUPS=tibble(Names=c(LAB3_NAMES_2,LAB5_NAMES_2,LAB6_NAMES_2,LAB7_NAMES_2),
       Groups=c(LAB3_GROUPS,LAB5_GROUPS,LAB6_GROUPS,LAB7_GROUPS)) %>% 
        arrange(Groups)

#Save Datasets
write_csv(GROUPS,path=str_c(getwd(),"/STOR320 Group Assignments.csv"))
