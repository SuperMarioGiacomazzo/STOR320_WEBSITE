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


#4.1
set.seed(216)
EXAMPLE=tibble(
  x=rnorm(10000,mean=0,sd=5),
  y=7+12/(1+exp(4-1*x))
)
ggplot(data=EXAMPLE) +
  geom_point(aes(x=x,y=y)) +
  theme_minimal()

#4.2
logistic.model=function(COEF,DATA){
  pred=COEF[1]+COEF[2]/(1+exp(COEF[3]-COEF[4]*DATA$A))
}
MSE.logistic=function(COEF,DATA){
  error=DATA$W-logistic.model(DATA=DATA,COEF=COEF)
  sq.error=error^2
  mse=mean(sq.error,na.rm=T)
  return(mse)
}

#4.3
logistic.mod=optim(
  par=c(min(TRAIN$W,na.rm=T),
        max(TRAIN$W,na.rm=T)-min(TRAIN$W,na.rm=T),
        median(TRAIN$A,na.rm=T),
        1),           #Smart Starting Values
  fn=MSE.logistic,    #Function to Minimize
  DATA=TRAIN          #Required Argument
)
print(logistic.mod)

#4.4
TRAIN6=TRAIN5 %>% mutate(logpred=logistic.model(COEF=logistic.mod$par,DATA=TRAIN5),
                         logres=W-logpred)
TEST6=TEST5 %>% mutate(logpred=logistic.model(COEF=logistic.mod$par,DATA=TEST5),
                         logres=W-logpred)


#Intermission
save.image("Tutorial11.Rdata")


#5.1
TEST6 %>%
  select(L,A,W,linpred,poly2pred,poly3pred,poly4pred,logpred)%>%
  gather(linpred:logpred,key="Model",value="Pred",factor_key=T) %>%
    ggplot() + 
      geom_point(aes(x=A,y=W),color="gray") + 
      theme_minimal() +
      geom_line(aes(x=A,y=Pred,color=Model),size=2)

#5.2
TEST6 %>%
  select(L,A,W,linpred,poly2pred,poly3pred,poly4pred,logpred)%>%
  gather(linpred:logpred,key="Model",value="Pred",factor_key=T) %>%
    ggplot() + 
    geom_point(aes(x=W,y=Pred,color=Model)) + 
    geom_abline(a=0,b=1,color="black",size=2) +
    xlab("Maximum Water Temperature") +
    ylab("Predicted Water Temperature") +
    theme_minimal()

#5.3
TEST6 %>%
  select(L,A,W,TIME,linres,poly2res,poly3res,poly4res,logres)%>%
  gather(linres:logres,key="Model",value="Res",factor_key=T) %>%
    ggplot() + 
      geom_line(aes(x=TIME,y=Res),color="lightskyblue2") + 
      geom_hline(yintercept=0,color="black",linetype=2,size=1.5) +
      xlab("Time") +
      ylab("Residual") +
      theme_dark() +
      facet_grid(Model~.)

#5.4
TEST6 %>%
  select(L,A,W,TIME,linpred,logpred) %>%
  gather(linpred:logpred,key="Model",value="Pred",factor_key=T) %>%
    ggplot() +
      geom_point(aes(x=A,y=W),alpha=0.2) +
      geom_line(aes(x=A,y=Pred,color=Model),size=2) +
      theme_minimal()+facet_grid(L~.)

TEST6 %>%
  select(L,A,W,TIME,linres,logres) %>%
  gather(linres:logres,key="Model",value="Res",factor_key=T) %>%
  ggplot() +
  geom_point(aes(x=A,y=Res,color=Model)) +
  geom_hline(yintercept=0) +
  theme_minimal()+facet_grid(L~.)

TEST6 %>%
  select(L,A,W,TIME,linres,logres) %>%
  gather(linres:logres,key="Model",value="Res",factor_key=T) %>%
  ggplot() +
  geom_point(aes(x=TIME,y=Res,color=Model)) +
  geom_hline(yintercept=0) +
  theme_minimal()+facet_grid(L~.)


#6.1
bias.func=function(res){
  bias=mean(res,na.rm=T)
  return(bias)
}

mae.func=function(res){
  mae=mean(abs(res),na.rm=T)
  return(mae)
}

rmse.func=function(res){
  mse=mean(res^2,na.rm=T)
  rmse=sqrt(mse)
  return(rmse)
}

#6.2
ex.res=TEST6$linres
c(bias.func(ex.res),mae.func(ex.res),rmse.func(ex.res))

ex.res.mat=TEST6 %>% select(linres,poly2res,poly3res,poly4res,logres)
apply(ex.res.mat,2,bias.func)
apply(ex.res.mat,2,mae.func)
apply(ex.res.mat,2,rmse.func)

#6.3
SUMM1=TEST6 %>%
  select(L,A,W,TIME,linres,poly2res,poly3res,poly4res,logres)%>%
  rename(Linear=linres,`Poly(2)`=poly2res,`Poly(3)`=poly3res,`Poly(4)`=poly4res,Logistic=logres)%>%
  gather(Linear:Logistic,key="Model",value="Residual",factor_key=T)
SUMM2= SUMM1 %>% 
  group_by(Model) %>%
  summarize(MB=bias.func(Residual),
            MAE=mae.func(Residual),
            RMSE=rmse.func(Residual))
print(SUMM2)

#6.4
SUMM3=xtable(SUMM2,digits=4,align=c("l","l","r","r","r"))
print(SUMM3,include.rownames=F,type="html")
