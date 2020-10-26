options(scipen=999)
library(tidyverse)    #Essential Functions
library(modelr)
library(purrr)
library(broom)
DATA=read_csv("AirWaterTemp.csv",col_types=cols()) #River Data

#1.1
NEST.DATA = DATA %>% group_by(L) %>% nest()
head(NEST.DATA)

#1.2
NEST.DATA %>% filter(L==103) %>% unnest() %>% glimpse()
NEST.DATA %>% filter(L!=103) %>% unnest() %>% glimpse()

#1.3
DATA2=DATA
DATA2$linpred=NA

TEST = NEST.DATA %>% filter(L==103) %>% unnest()
TRAIN  = NEST.DATA %>% filter(L!=103) %>% unnest()

linmod=lm(W~dplyr::lag(A,1),data=TRAIN)
linmodpred=predict(linmod,newdata=TEST)

DATA2$linpred[which(DATA2$L==103)]=linmodpred
head(DATA2)

#1.4
DATA2=DATA
DATA2$linpred=NA

for(k in unique(DATA2$L)){
  TEST = NEST.DATA %>% filter(L==k) %>% unnest()
  TRAIN  = NEST.DATA %>% filter(L!=k) %>% unnest()
  
  linmod=lm(W~A,data=TRAIN)
  linmodpred=predict(linmod,newdata=TEST)
  
  DATA2$linpred[which(DATA2$L==k)]=linmodpred
}

#1.5
RMSE.func=function(actual,predict){
  mse=mean((actual-predict)^2,na.rm=T)
  rmse=sqrt(mse)
  return(rmse)
}
RMSE.func(actual=DATA2$W,predict=DATA2$linpred)


#2.1
ggplot(data=DATA) +
  geom_point(aes(x=JULIAN_DAY,y=W,color=A),alpha=0.3) + 
  xlab("Day of Year") + ylab("Max Water Temperature") +
  guides(color=guide_legend(title="Max Air \nTemperature")) +
  theme_minimal()

#2.2
polymodel=lm(W~poly(A,4)+poly(JULIAN_DAY,3),data=na.omit(DATA))
tidy(polymodel)
glance(polymodel)


#2.3
DATA3=na.omit(DATA) %>% crossv_kfold(10)
head(DATA3)

#2.4
train.model.func=function(data,i,j){
  mod=lm(W~poly(A,i)+poly(JULIAN_DAY,j),data=data)
  return(mod)
}

i=4
j=3

DATA4=DATA3 %>% 
       mutate(tr.model=map(train,train.model.func,i=i,j=j))
head(DATA4)


#2.5
DATA4.PREDICT = DATA4 %>% 
          mutate(predict=map2(test,tr.model,~augment(.y,newdata=.x))) %>%
          select(predict) %>%
          unnest()
head(DATA4.PREDICT)
RMSE.func(actual=DATA4.PREDICT$W,predict=DATA4.PREDICT$.fitted)

