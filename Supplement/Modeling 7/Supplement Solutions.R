options(scipen=999)
library(tidyverse)
library(modelr)
library(purrr)
library(broom)
library(class)
library(titanic)

T1=titanic_train[,c("Survived","SibSp","Parch","Fare")]
head(T1)

#1.1
T2 = T1 %>%
      mutate(Family=SibSp+Parch) %>%
      select(-SibSp,-Parch)

#1.2
ggplot(T2) +
  geom_point(aes(x=Family,y=Fare,color=factor(Survived)),alpha=c(0.3))+
  theme_minimal()+
  theme(text=element_text(size=20)) +
  guides(color=guide_legend(title="Survived"))

#2.1
Alice=c(3,100)
ggplot(T2) +
  geom_point(aes(x=Family,y=Fare,color=factor(Survived)),alpha=c(0.3))+
  geom_point(aes(x=Alice[1],y=Alice[2]),shape="A",size=8)+
  theme_minimal()+
  theme(text=element_text(size=20)) +
  guides(color=guide_legend(title="Survived"))

#2.2
k=5

dist.func=function(point1,point2){
  dist=sqrt(sum((point1-point2)^2))
  return(dist)
}

T3=T2 %>% 
      mutate(d=apply(select(T2,Family,Fare),1,dist.func,point2=Alice)) %>%
      arrange(d) %>%
      filter(rank(d,ties.method="min")<=k)

print(T3)

#2.3
ggplot(T3) +
  geom_point(aes(x=Family,y=Fare,color=factor(Survived)))+
  geom_point(aes(x=Alice[1],y=Alice[2]),shape="A",size=8)+
  theme_minimal()+
  xlim(min(T2$Family),max(T2$Family))+
  ylim(min(T2$Fare),max(T2$Fare))+
  theme(text=element_text(size=20)) +
  guides(color=guide_legend(title="Survived"))


#3.1
mean.Family=mean(T2$Family)
sd.Family=sd(T2$Family)
mean.Fare=mean(T2$Fare)
sd.Fare=sd(T2$Fare)

ST3= T2 %>% 
        mutate(Family=(Family-mean.Family)/sd.Family,
               Fare=(Fare-mean.Fare)/sd.Fare)

Z.Alice=(Alice-c(mean.Family,mean.Fare))/c(sd.Family,sd.Fare)
ggplot(ST3) +
  geom_point(aes(x=Family,y=Fare,color=factor(Survived)),alpha=c(0.3))+
  geom_point(aes(x=Z.Alice[1],y=Z.Alice[2]),shape="A",size=8)+
  theme_minimal()+
  xlab("Standardized Family")+
  ylab("Standardized Fare")+
  theme(text=element_text(size=20)) +
  guides(color=guide_legend(title="Survived"))

#3.2
ST4=ST3 %>% 
  mutate(d=apply(select(ST3,Family,Fare),1,dist.func,point2=Z.Alice)) %>%
  arrange(d) %>%
  filter(rank(d,ties.method="min")<=k)

print(ST4 %>%
        mutate(Family=sd.Family*Family+mean.Family,
               Fare=sd.Fare*Fare+mean.Fare)
)

ggplot(ST4) +
  geom_point(aes(x=Family,y=Fare,color=factor(Survived)),size=4)+
  geom_point(aes(x=Z.Alice[1],y=Z.Alice[2]),shape="A",size=8)+
  theme_minimal()+
  xlim(-1,10)+
  ylim(-1,10)+
  theme(text=element_text(size=20)) +
  guides(color=guide_legend(title="Survived"))

#4.1
k=500
ST5=ST3 %>% 
  mutate(d=apply(select(ST3,Family,Fare),1,dist.func,point2=Z.Alice)) %>%
  arrange(d) %>%
  filter(rank(d,ties.method="min")<=k)

ggplot(ST5) +
  geom_point(aes(x=Family,y=Fare,color=factor(Survived)),size=4)+
  geom_point(aes(x=Z.Alice[1],y=Z.Alice[2]),shape="A",size=8)+
  theme_minimal()+
  xlim(-1,10)+
  ylim(-1,10)+
  theme(text=element_text(size=20)) +
  guides(color=guide_legend(title="Survived"))

KNN.PREDICT=table(ST5$Survived)
print(KNN.PREDICT)

#4.2
library(class)
possible.k=1:250
accuracy.k=rep(NA,250)

for(k in 1:250){
  cv.out=knn.cv(train=select(ST3,Family,Fare),
                cl=factor(ST3$Survived,levels=c(0,1),labels=c("Died","Survived")),
                k=k)
  correct=mean(cv.out==factor(ST3$Survived,levels=c(0,1),labels=c("Died","Survived")))
  accuracy.k[k]=correct
}

ggplot(data=tibble(possible.k,accuracy.k)) +
  geom_line(aes(x=possible.k,y=accuracy.k),color="lightskyblue2",size=2) +
  theme_minimal() +
  xlab("Choice of k") +
  ylab("Percentage of Accurate Predictions") +
  theme(text=element_text(size=20))
  
#4.3
best.k=which.max(accuracy.k)
TEST = titanic_test[,c("SibSp","Parch","Fare")] %>%
        mutate(Family=SibSp+Parch) %>%
        select(-SibSp,-Parch) %>%
        mutate(Fare=(Fare-mean.Fare)/sd.Fare,
               Family=(Family-mean.Family)/sd.Family) %>%
        na.omit()

TEST2 = TEST %>% 
          mutate(Predict=knn(train=select(ST3,Family,Fare),
                             test=select(TEST,Family,Fare),
                             cl=factor(ST3$Survived,levels=c(0,1),
                                       labels=c("Died","Survived")),
                             k=best.k)) %>%
          mutate(Family=sd.Family*Family+mean.Family,
                 Fare=sd.Fare*Fare+mean.Fare)

ggplot(TEST2) +
  geom_point(aes(x=Family,y=Fare,color=Predict),
             size=2,alpha=0.3) +
  theme_minimal() +
  theme(text=element_text(size=20))
  
        




