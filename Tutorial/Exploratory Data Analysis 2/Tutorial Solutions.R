library(tidyverse)
library(Ecdat)
library(knitr)
library(kableExtra)

#Data
head(Wages1)

wage=as.tibble(Wages1) %>%
  rename(experience=exper) %>%
  arrange(school)
wage

#Part 1

###Question 1
ggplot(wage) +
  geom_bar(aes(x=experience))

wage %>%
  group_by(experience) %>%
  summarize(n=n()) %>%
  arrange(desc(n)) %>%
  summarize(common.exp=first(experience),
            common.n=first(n))

###Question 2 
ggplot(wage) +
  geom_bar(aes(x=school))

wage %>%
  group_by(school) %>%
  summarize(n=n()) %>%
  arrange(desc(school)) %>% 
  summarize(max.school=first(school),
            max.n=first(n))


#Part 2

###Follow-up to Questions 1-2
ggplot(wage) +
  geom_point(aes(x=school,y=experience),
             alpha=0.1,shape=16,size=2)

###Question 3
ggplot(wage)+
  geom_freqpoly(aes(x=wage,color=sex))

wage %>% 
  group_by(sex) %>%
  summarize(n=n(),mean=mean(wage),se=sd(wage)/sqrt(n),
            lb=mean-2*se,ub=mean+2*se)

###Question 4 
ggplot(data=wage) +
  geom_point(aes(x=experience,y=wage,color=sex))

###Question 5
ggplot(data=wage) +
  geom_point(aes(x=school,y=wage,color=sex))

###Question 6
wage %>%
  group_by(experience,school,sex) %>%
  summarize(n=n(),mean=mean(wage)) %>%
  ungroup() %>%
  ggplot() +
    geom_tile(aes(x=experience,y=school,fill=mean)) +
  scale_fill_gradientn(colors=c("black","lightskyblue","white"))+
  facet_grid(~sex) + theme_dark()

wage %>%
  group_by(experience,school,sex) %>%
  summarize(n=n(),sd=sd(wage)) %>%
  ungroup() %>%
  filter(n>10) %>%
  ggplot() +
  geom_tile(aes(x=experience,y=school,fill=sd)) +
  scale_fill_gradientn(colors=c("black","lightskyblue","white"))+
  facet_grid(~sex) + theme_dark()



