options(scipen=999)
library(tidyverse)    #Essential Functions
library(modelr)
DATA=read_csv("AirWaterTemp.csv",col_types=cols()) #River Data

#1.1
ggplot(data=DATA) +
  geom_point(aes(x=A,y=W),alpha=0.3)+
  geom_smooth(aes(x=A,y=W)) +
  theme_minimal()

#1.2
WAPLOT.func=function(Location){
  DATA %>% filter(L == Location) %>%
    ggplot()+
    geom_point(aes(x=A,y=W),alpha=0.3)+
    geom_smooth(aes(x=A,y=W)) +
    theme_minimal()
}

WAPLOT.func(103)
WAPLOT.func(105)
WAPLOT.func(918)

#1.3
set.seed(216)
TEST.LOCATIONS=sample(x=unique(DATA$L),size=3,replace=F)

TRAIN = anti_join(DATA,tibble(L=TEST.LOCATIONS),by="L")
TEST = semi_join(DATA,tibble(L=TEST.LOCATIONS),by="L")

#1.4
WAPLOT2.func=function(DATA){
  ggplot(data=DATA)+
    geom_point(aes(x=A,y=W),alpha=0.3)+
    geom_smooth(aes(x=A,y=W)) +
    theme_minimal()
}

WAPLOT2.func(TRAIN)
WAPLOT2.func(TEST)


#2.1
linmod=lm(W~A,data=TRAIN)
summary(linmod)

#2.2
TRAIN2 = TRAIN %>% add_predictions(linmod,var="linpred")
TEST2 = TEST %>% add_predictions(linmod,var="linpred")

#2.3
TRAIN3 = TRAIN2 %>% add_residuals(linmod,var="linres")
TEST3 = TEST2 %>% add_residuals(linmod,var="linres")


#3.1
poly2mod=lm(W~A+I(A^2),data=TRAIN)
poly3mod=lm(W~A+I(A^2)+I(A^3),data=TRAIN)
poly4mod=lm(W~A+I(A^2)+I(A^3)+I(A^4),data=TRAIN)
anova(linmod,poly2mod,poly3mod,poly4mod,test="Chisq")

#3.2
TRAIN4 =TRAIN3 %>% 
  add_predictions(poly2mod,var="poly2pred") %>%
  add_predictions(poly3mod,var="poly3pred") %>%
  add_predictions(poly4mod,var="poly4pred")
  
TEST4 =TEST3 %>% 
  add_predictions(poly2mod,var="poly2pred") %>%
  add_predictions(poly3mod,var="poly3pred") %>%
  add_predictions(poly4mod,var="poly4pred")

#3.3
TRAIN5 =TRAIN4 %>% 
  add_residuals(poly2mod,var="poly2res") %>%
  add_residuals(poly3mod,var="poly3res") %>%
  add_residuals(poly4mod,var="poly4res")

TEST5 =TEST4 %>% 
  add_residuals(poly2mod,var="poly2res") %>%
  add_residuals(poly3mod,var="poly3res") %>%
  add_residuals(poly4mod,var="poly4res")

