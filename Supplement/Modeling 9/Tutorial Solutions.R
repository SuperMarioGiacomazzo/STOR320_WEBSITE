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

#2.4
set.seed(216)
in.train=sample(1:500,floor(0.66*500))
SIM.TRAIN=SIM.DATA[in.train,]
SIM.TEST=SIM.DATA[-in.train,]

#Default: 10 Fold Cross Validation
RESULT=NULL
for (i in 0:10) {
  set.seed(216)
  cv.out = cv.glmnet(x=as.matrix(SIM.TRAIN[,-1]),
                     y=as.vector(SIM.TRAIN[,1]),
                     type.measure="mse", 
                     alpha=i/10)
  alpha=i/10
  best.lambda=cv.out$lambda.1se
  y.test=predict(cv.out,s=best.lambda,newx=as.matrix(SIM.TEST[,-1]))
  out.mse=mean((SIM.TEST$y-y.test)^2)
  RESULT=rbind(RESULT,c(alpha,best.lambda,out.mse))
}
colnames(RESULT)=c("alpha","lambda","MSE")
print(RESULT)

#2.5
RESULT2=as.data.frame(RESULT) %>% filter(rank(MSE)<=4)
head(RESULT2)
RESULT3=NULL
for(k in 1:4){
  fit=glmnet(x=as.matrix(SIM.DATA[,-1]),y=as.matrix(SIM.DATA[,1]),alpha=RESULT2$alpha[k])
  RESULT3=rbind(RESULT3,cbind(k,1:201,as.numeric(coef(fit,s=RESULT2$lambda[k])),c(0,beta)))
}
colnames(RESULT3)=c("Model","Parameter","Estimate","Actual")
RESULT3=as.data.frame(RESULT3)

RESULT3 %>% 
  ggplot() +
  geom_point(aes(x=Parameter,y=Estimate,color=as.factor(Model)),size=2) +
  geom_line(aes(x=Parameter,y=Actual),linetype="dashed",size=1.25,alpha=0.4) +
  theme_minimal() +
  facet_grid(as.factor(Model)~.) +
  guides(color=FALSE)
  

#3.1
DATA=mpg
DATA2=DATA[,c("year","displ","cyl","drv","cty","hwy","fl","class")]
head(DATA2)

y=DATA2$hwy
X=model_matrix(DATA2,hwy~.*.)[,-1]
var.names=names(X)
dim(X)

#3.2
set.seed(216)
cvmod.0=cv.glmnet(y=y,x=as.matrix(X),alpha=0)
set.seed(216)
cvmod.25=cv.glmnet(y=y,x=as.matrix(X),alpha=0.25)
set.seed(216)
cvmod.5=cv.glmnet(y=y,x=as.matrix(X),alpha=0.5)
set.seed(216)
cvmod.75=cv.glmnet(y=y,x=as.matrix(X),alpha=0.75)
set.seed(216)
cvmod.1=cv.glmnet(y=y,x=as.matrix(X),alpha=1)

CV.0.ERROR=cvmod.0$cvm[which(cvmod.0$lambda==cvmod.0$lambda.1se)]
CV.25.ERROR=cvmod.25$cvm[which(cvmod.25$lambda==cvmod.25$lambda.1se)]
CV.5.ERROR=cvmod.5$cvm[which(cvmod.5$lambda==cvmod.5$lambda.1se)]
CV.75.ERROR=cvmod.75$cvm[which(cvmod.75$lambda==cvmod.75$lambda.1se)]
CV.1.ERROR=cvmod.1$cvm[which(cvmod.1$lambda==cvmod.1$lambda.1se)]

MOD.RESULT=tibble(alpha=c(0,0.25,0.5,0.75,1),
                  lambda=c(cvmod.0$lambda.1se,cvmod.25$lambda.1se,
                           cvmod.5$lambda.1se,cvmod.75$lambda.1se,
                           cvmod.1$lambda.1se),
                  CV.Error=c(CV.0.ERROR,CV.25.ERROR,CV.5.ERROR,
                             CV.75.ERROR,CV.1.ERROR))
print(MOD.RESULT)

#3.3
best.alpha=MOD.RESULT$alpha[which.min(MOD.RESULT$CV.Error)]
best.lambda=MOD.RESULT$lambda[which.min(MOD.RESULT$CV.Error)]

best.mod=glmnet(y=y,x=as.matrix(X),nlambda=1,lambda=best.lambda,alpha=best.alpha)
best.coef=as.tibble(as.matrix(coef(best.mod)))
best.coef2=best.coef %>% 
  mutate(Parameter=c("Int",var.names)) %>%
  rename(Estimate=s0) %>%
  select(Parameter,Estimate)
nonzero.best.coef=best.coef2 %>%
  filter(Estimate!=0)
print(nonzero.best.coef,n=1e3)

DATA2$hwy.hat=predict(best.mod,newx=as.matrix(X))

ggplot(DATA2) +
  geom_point(aes(x=hwy,y=hwy.hat),color="lightskyblue2") +
  geom_abline(a=0,b=1,linetype="dashed") +
  theme_minimal() +
  ylab("Predicted Highway MPG") +
  xlab("Actual Highway MPG")

ggplot(DATA2) +
  geom_histogram(aes(x=hwy-hwy.hat),fill="lightskyblue2") +
  theme_minimal() +
  xlab("Residuals") +
  ylab("Frequency")

#3.4
library(Ecdat)
Part=Participation
dim(Part)
head(Part)
Part$lfp=ifelse(Part$lfp=="yes",1,0)
Part$foreign=ifelse(Part$foreign=="yes",1,0)

#3.5
set.seed(216)
cvmod.0=cv.glmnet(y=as.factor(Part$lfp),x=as.matrix(Part[,-1]),alpha=0,
                  family="binomial",type.measure="class")
set.seed(216)
cvmod.25=cv.glmnet(y=as.factor(Part$lfp),x=as.matrix(Part[,-1]),alpha=0.25,
                   family="binomial",type.measure="class")
set.seed(216)
cvmod.5=cv.glmnet(y=as.factor(Part$lfp),x=as.matrix(Part[,-1]),alpha=0.5,
                  family="binomial",type.measure="class")
set.seed(216)
cvmod.75=cv.glmnet(y=as.factor(Part$lfp),x=as.matrix(Part[,-1]),alpha=0.75,
                   family="binomial",type.measure="class")
set.seed(216)
cvmod.1=cv.glmnet(y=as.factor(Part$lfp),x=as.matrix(Part[,-1]),alpha=1,
                  family="binomial",type.measure="class")

CV.0.ERROR=cvmod.0$cvm[which(cvmod.0$lambda==cvmod.0$lambda.1se)]
CV.25.ERROR=cvmod.25$cvm[which(cvmod.25$lambda==cvmod.25$lambda.1se)]
CV.5.ERROR=cvmod.5$cvm[which(cvmod.5$lambda==cvmod.5$lambda.1se)]
CV.75.ERROR=cvmod.75$cvm[which(cvmod.75$lambda==cvmod.75$lambda.1se)]
CV.1.ERROR=cvmod.1$cvm[which(cvmod.1$lambda==cvmod.1$lambda.1se)]

MOD.RESULT=tibble(alpha=c(0,0.25,0.5,0.75,1),
                  lambda=c(cvmod.0$lambda.1se,cvmod.25$lambda.1se,
                           cvmod.5$lambda.1se,cvmod.75$lambda.1se,
                           cvmod.1$lambda.1se),
                  CV.Error=c(CV.0.ERROR,CV.25.ERROR,CV.5.ERROR,
                             CV.75.ERROR,CV.1.ERROR))
print(MOD.RESULT)

#3.6
best.alpha=MOD.RESULT$alpha[which.min(MOD.RESULT$CV.Error)]
best.lambda=MOD.RESULT$lambda[which.min(MOD.RESULT$CV.Error)]

best.mod=glmnet(y=as.factor(Part$lfp),x=as.matrix(Part[,-1]),
                nlambda=1,lambda=best.lambda,alpha=best.alpha,
                family="binomial")
best.coef=as.matrix(coef(best.mod))
head(best.coef)

Part$Predict=predict(best.mod,newx=as.matrix(Part[,-1]),type="class")
Part$lfp=ifelse(Part$lfp==1,"Yes","No")
Part$Predict=ifelse(Part$Predict=="1","Yes","No")

table(Part[,c("lfp","Predict")])
sum(Part$lfp=="Yes")
sum(Part$Predict=="Yes")
