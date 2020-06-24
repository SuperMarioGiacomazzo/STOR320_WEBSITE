# This is a good place to put libraries required for future work
knitr::opts_chunk$set(echo = TRUE)
options(scipen=999)
library(tidyverse)    #Loads the tidyverse suite of packages
library(xtable)       #Alternate to Kable package for printing html tables

#Create School Data
school.id=rep(1:20,each=20*2)
student.id=rep(rep(1:20,each=2),20)
type=rep(c("Score","Percentile"),20*20)
score2017=round(rnorm(20*20,mean=50,sd=10),0)
percentile2017=round(100*pnorm(score2017,mean=mean(score2017),sd=sd(score2017)),0)
score2018=round(rnorm(20*20,mean=75,sd=4),0)
percentile2018=round(100*pnorm(score2018,mean=mean(score2018),sd=sd(score2018)),0)
value2017=c(rbind(score2017,percentile2017))
value2018=c(rbind(score2018,percentile2018))

untidy.school = tibble(
  school=school.id,
  student=student.id,
  type=type,
  value2017=value2017,
  value2018=value2018) %>% 
  filter(!(school==1 & student==4)) %>% filter(!(school==12 & student==18)) %>%
  mutate(value2018=ifelse((school==1 & student==3)|(school==15 & student==18)|
                            (school==5 & student==12),NA,value2018))

tab.untidy.school=untidy.school %>%
  head(10) %>%
  xtable(digits=0,align="cccccc")

print(tab.untidy.school,type="html",include.rownames=F,
      html.table.attributes="align='center',
                             rules='rows',
                             width=50%,
                             frame='hsides',
                             border-spacing=5px"
)


#Part 1: Unique ID for Student
untidy.school %>% filter(student==1) %>% head(4)

untidy2.school = untidy.school %>%
                    unite(CID,school,student,sep=".",remove=F)
glimpse(untidy2.school)
                      
#Part 2: Gather Variables Containing Value
untidy3.school = untidy2.school %>%
                    rename(`2017`=value2017,`2018`=value2018) %>%
                    gather(`2017`:`2018`,key="Year",value="Value",convert=T)
glimpse(untidy3.school)

# Part 3: Spread Type of Value Into Multiple Columns
tidy.school = untidy3.school %>%
                    spread(key="type",value="Value")  

# Part 4: Missing Data Analysis
tab.tidy.school = tidy.school %>%
  arrange(school,student) %>%
  head(10) %>%
  xtable(digits=0,align="ccccccc")

print(tab.tidy.school,type="html",include.rownames=F,
      html.table.attributes="align='center',
      rules='rows',
      width=50%,
      frame='hsides',
      border-spacing=5px"
)

tidy2.school=tidy.school %>%
  complete(school,student,Year)

tab.tidy2.school = tidy2.school %>%
  head(10) %>%
  xtable(digits=0,align="ccccccc")

print(tab.tidy2.school,type="html",include.rownames=F,
      html.table.attributes="align='center',
      rules='rows',
      width=50%,
      frame='hsides',
      border-spacing=5px"
)

final.tidy.school = untidy.school %>%
                      rename(`2017`=value2017,`2018`=value2018) %>%
                      gather(`2017`:`2018`,key="Year",value="Value",convert=T) %>%
                      spread(key="type",value="Value")  %>%
                      complete(school,student,Year) %>%
                      unite(CID,school,student,sep=".",remove=F)

ggplot(final.tidy.school) +
  geom_boxplot(aes(x=as.factor(Year),y=Score,fill=as.factor(school))) + 
  guides(fill=F)+
  theme_minimal()

ggplot(final.tidy.school) + 
  geom_line(aes(x=Year,y=Score,color=CID)) +
  guides(color=F) +
  scale_x_discrete(lim=c(2017,2018),breaks=c(2017,2018),labels=c(2017,2018)) +
  theme_minimal()

ggplot(final.tidy.school) + 
  geom_line(aes(x=Year,y=Percentile,color=CID)) +
  guides(color=F) +
  scale_x_discrete(lim=c(2017,2018),breaks=c(2017,2018),labels=c(2017,2018)) +
  theme_minimal()
                      