options(scipen=999)
library(tidyverse)
library(modelr)
library(purrr)
library(broom)

#1.1
set.seed(216)
X=matrix(rnorm(100000),500,200)
beta=c(rep(5,5),rep(-2,5),rep(0,190))
set.seed(480)
epsilon=rnorm(500,0,10)
y=X%*%beta+epsilon

SIM.DATA=data.frame(y=y,X=X)

#1.2
lm.model=lm(y~.,data=SIM.DATA)
glance(lm.model)
param.est=lm.model$coefficients
param.conf=confint(lm.model)
param.lm=data.frame(cbind(param.est,param.conf))[-1,] #Remove Intercept
names(param.lm)=c("Estimate","Lower","Upper")
param.lm = param.lm %>%
  mutate(Significant=factor(ifelse(0>Lower & 0<Upper,"No","Yes")))

ggplot(param.lm[1:5,]) +
  geom_pointrange(aes(x=1:5,y=Estimate,ymin=Lower,ymax=Upper,color=Significant),size=2)+
  theme_minimal()+
  scale_color_manual(drop=F,values=c("lightskyblue2","gray"))+
  xlab("X1:X5")
ggplot(param.lm[6:10,]) +
  geom_pointrange(aes(x=6:10,y=Estimate,ymin=Lower,ymax=Upper,color=Significant),size=2)+
  theme_minimal()+
  scale_color_manual(values=c("lightskyblue2","gray"))+
  xlab("X6:X10")
ggplot(param.lm[11:200,]) +
  geom_pointrange(aes(x=11:200,y=Estimate,ymin=Lower,ymax=Upper,color=Significant))+
  theme_minimal()+
  scale_color_manual(values=c("lightskyblue2","gray"))+
  xlab("X11:X200")

#1.3
COEF=rep(NA,200)
P.VAL=rep(NA,200)
for(j in 1:200){
  individual.mod=lm(y~.,data=SIM.DATA[,c(1,j+1)])
  COEF[j]=coef(individual.mod)[2]
  P.VAL[j]=summary(individual.mod)$coefficients[2,4]
}

KEEP=P.VAL<0.01
ACTUAL=c(rep("NonZero",10),rep("Zero",190))

tibble(COEF,P.VAL,KEEP) %>% 
  ggplot() +
  geom_point(aes(x=COEF,y=P.VAL,color=KEEP),size=2) +
  geom_hline(yintercept=0.01,linetype="dashed")+
  scale_color_manual(values=c("lightskyblue2","gray"))+
  theme_minimal()


tibble(ACTUAL=ACTUAL,KEEP=KEEP) %>% 
  table() %>% prop.table()

#1.4
Cutoff=0.2

COEF=rep(NA,200)
P.VAL=rep(NA,200)
for(j in 1:200){
  individual.mod=lm(y~.,data=SIM.DATA[,c(1,j+1)])
  COEF[j]=coef(individual.mod)[2]
  P.VAL[j]=summary(individual.mod)$coefficients[2,4]
}

KEEP=P.VAL<Cutoff
ACTUAL=c(rep("NonZero",10),rep("Zero",190))

tibble(ACTUAL=ACTUAL,KEEP=KEEP) %>% 
  table() %>% prop.table()

lm.model=lm(y~.,data=SIM.DATA[,c(1,which(KEEP)+1)])
param.est=lm.model$coefficients
param.conf=confint(lm.model)
param.lm=data.frame(cbind(param.est,param.conf))[-1,] #Remove Intercept
names(param.lm)=c("Estimate","Lower","Upper")
param.lm = param.lm %>%
  mutate(Significant=factor(ifelse(0>Lower & 0<Upper,"No","Yes")))

ggplot(param.lm[1:5,]) +
  geom_pointrange(aes(x=1:5,y=Estimate,ymin=Lower,ymax=Upper,color=Significant),size=2)+
  theme_minimal()+
  scale_color_manual(drop=F,values=c("lightskyblue2","gray"))+
  xlab("X1:X5")
ggplot(param.lm[6:10,]) +
  geom_pointrange(aes(x=6:10,y=Estimate,ymin=Lower,ymax=Upper,color=Significant),size=2)+
  theme_minimal()+
  scale_color_manual(values=c("lightskyblue2","gray"))+
  xlab("X6:X10")
ggplot(param.lm[11:(dim(param.lm)[1]),]) +
  geom_pointrange(aes(x=11:(dim(param.lm)[1]),y=Estimate,ymin=Lower,ymax=Upper,color=Significant))+
  theme_minimal()+
  scale_color_manual(values=c("lightskyblue2","gray"))+
  xlab("X11:X200")

#2.1
library(glmnet)
ridge.mod=glmnet(x=as.matrix(SIM.DATA[,-1]),
                 y=as.vector(SIM.DATA[,1]),
                 alpha=0)
plot(ridge.mod,xvar="lambda")

#2.2
lasso.mod=glmnet(x=as.matrix(SIM.DATA[,-1]),
                 y=as.vector(SIM.DATA[,1]),
                 alpha=1)
plot(lasso.mod,xvar="lambda")

#2.3
enet.mod=glmnet(x=as.matrix(SIM.DATA[,-1]),
                y=as.vector(SIM.DATA[,1]),
                alpha=1/2)
plot(enet.mod,xvar="lambda")
