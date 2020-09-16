options(scipen=999)
library(tidyverse)    #Essential Functions
library(rvest)        #Read Tables From Webpages
library(noncensus)    #Contains Zip Codes for US Cities


#1.1
URL.VIOLENT="https://en.wikipedia.org/wiki/List_of_United_States_cities_by_crime_rate#Crime_rates_per_100.2C000_people_.282012.29"
VIOLENT = URL.VIOLENT %>%
  read_html() %>%
  html_table(fill=T) %>%
  .[[1]]
str(VIOLENT)

#1.2
VIOLENT2=VIOLENT[-1,1:8]
colnames(VIOLENT2)=c("State","City","Population","Total","Murder","Rape","Robbery","Assault")
str(VIOLENT2)

#1.3
VIOLENT3=VIOLENT2 %>%
  mutate_at(3:8,as.numeric)
str(VIOLENT3)

#1.4
VIOLENT3[str_detect(VIOLENT3$City,"[,(0-9){1}]"),]$City

#1.5
VIOLENT4 = VIOLENT3 %>%
  mutate(City=str_replace_all(City,"[,(0-9){1}]","")) %>%
  mutate(State=str_replace_all(State,"[,(0-9){1}]",""))
VIOLENT5 = VIOLENT4 %>% 
  mutate(City=ifelse(City=="Charlotte-Mecklenburg","Charlotte",City),
         City=ifelse(City=="Savannah-Chatham Metropolitan","Savannah",City),
         City=ifelse(City=="Las Vegas Metropolitan Police Department","Las Vegas",City))
VIOLENT6 = VIOLENT5 %>%
  mutate(City=str_replace(City,"Metropolitan",""))
write_csv(VIOLENT6,"FINAL_VIOLENT.CSV")

#2.1
data(zip_codes)
ZIP=zip_codes
str(ZIP)

#2.2
ZIP2 = ZIP %>%
  group_by(city,state) %>%
  summarize(lat=mean(latitude),lon=mean(longitude)) %>%
  ungroup()
write_csv(ZIP2,"FINAL_ZIP.CSV")

#2.3
head(VIOLENT6[,1:4],3)
head(ZIP2,3)

#3.1
URL.STATE_ABBREV = "https://state.1keydata.com/state-abbreviations.php"
STATE_ABBREV = URL.STATE_ABBREV %>%
  read_html() %>%
  html_table(fill=T) %>%
  .[[3]] %>%
  .[-1,]
head(STATE_ABBREV)

#3.2
STATE_ABBREV_TOP = STATE_ABBREV[,1:2]
names(STATE_ABBREV_TOP)=c("State","state")
STATE_ABBREV_BOT = STATE_ABBREV[,3:4]
names(STATE_ABBREV_BOT)=c("State","state")
STATE_ABBREV2=rbind(STATE_ABBREV_TOP,STATE_ABBREV_BOT) %>% arrange(State)
head(STATE_ABBREV2)
write_csv(STATE_ABBREV2,"FINAL_STATE_ABBREV.CSV")
