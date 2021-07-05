setwd("D:/Mario Documents/UNC/STOR 320/STOR320_WEBSITE/Group Assignments")

library(tidyverse)
library(readxl)

#Read Rosters
LAB0=read_excel("STOR320_400.xlsx")
LAB1=read_excel("STOR320_401.xlsx")


#Gather Names
LAB0_NAMES=LAB0[6:dim(LAB0)[1],1]$STOR320.001.S221
LAB1_NAMES=LAB1[6:dim(LAB1)[1],1]$STOR320.001.S221

#Random Assignment
set.seed(216,sample.kind="Rejection")
LAB0_NAMES_2=sample(LAB0_NAMES,size=length(LAB0_NAMES))
LAB1_NAMES_2=sample(LAB1_NAMES,size=length(LAB1_NAMES))

#Group Numbers
LAB0_GROUPS=rep(1:3,each=5,length=length(LAB0_NAMES_2))
LAB1_GROUPS=rep(4:5,each=5,length=length(LAB1_NAMES_2))

#Bring Names with Groups

GROUPS=tibble(Names=c(LAB0_NAMES_2,LAB1_NAMES_2),
       Groups=c(LAB0_GROUPS,LAB1_GROUPS)) %>% 
        arrange(Groups)

#Save Datasets
write_csv(GROUPS,path=str_c(getwd(),"/STOR320 Group Assignments.csv"))
