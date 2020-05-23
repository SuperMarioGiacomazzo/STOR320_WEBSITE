library(tidyverse)
library(readxl)

#Read Rosters
Section=read_excel("STOR320_Roster.xlsx")

#Function to Divide Sections into Groups
Group.select.func<-function(data){
  student.names=data$Name
  set.seed(length(student.names))
  random.order=sample(1:length(student.names),replace=F)
  group=tibble(Order=random.order,Name=student.names) %>%
          arrange(Order) %>%
          mutate(Group=rep(1:(length(student.names)%/%5+(length(student.names)%%5!=0)),
                           each=5,length=length(student.names))) %>%
          mutate(Role="TBD") %>%
          select(-Order) %>%
          arrange(Group)
  return(group)
}

#Application of the Function
Final.Section=Group.select.func(Section)

#Save Datasets
write_csv(Final.Section,path=str_c(getwd(),"/STOR320 Group Assignments.csv"))
