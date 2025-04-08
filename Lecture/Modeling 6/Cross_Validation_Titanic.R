library(tidyverse)
library(titanic)
library(purrr)
library(broom)
library(modelr)

titanic_train=titanic_train

set.seed(300)
titanic2=titanic_train %>%  mutate(L=sample(1:10,891,replace=T))

FINAL=NULL

for(k in 1:10){
  train=filter(titanic2,L!=k)
  test=filter(titanic2,L==k)
  model1=glm(Survived~Sex,data=train,family="binomial")
  model2=glm(Survived~Fare,data=train,family="binomial")
  predictions1=predict(model1,test,type="response")
  predictions2=predict(model2,test,type="response")
  test2=test %>% mutate(pred1=predictions1,
                        pred2=predictions2,
                        class1=ifelse(pred1<0.5,0,1),
                        class2=ifelse(pred2<0.5,0,1))
  FINAL=rbind(FINAL,test2)
}

mean(FINAL$Survived==FINAL$class1)
mean(FINAL$Survived==FINAL$class2)

1-mean(FINAL$Survived)
