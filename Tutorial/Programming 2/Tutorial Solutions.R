options(scipen=999)
library(tidyverse)    #Essential Functions
library(Ecdat)    #Contains Economic Datasets


#2.4
Cigar=Ecdat::Cigar
head(Cigar)
print(round(cor(Cigar),3))

#2.5
CORR.CIGAR1=matrix(NA,9,9) #Empty Matrix Initialized
rownames(CORR.CIGAR1)=names(Cigar)
colnames(CORR.CIGAR1)=names(Cigar)

seq_along(names(Cigar))

for(j in seq_along(names(Cigar))){
  for(k in seq_along(names(Cigar))){
    CORR.CIGAR1[j,k]=cor(Cigar[,j],Cigar[,k])
  }
}
print(round(CORR.CIGAR1,3))

CORR.CIGAR2=matrix(NA,8,8)
rownames(CORR.CIGAR2)=names(Cigar)[-1]
colnames(CORR.CIGAR2)=names(Cigar)[-1]
for(j in 1:8){
  for(k in 1:8){
    CORR.CIGAR2[j,k]=cor(Cigar[,j+1],Cigar[,k+1])
  }
}
print(round(CORR.CIGAR2,3))


#2.6
HealthInsurance=Ecdat::HI
head(HealthInsurance)
#print(round(cor(HealthInsurance),3)) #Try this Code

#2.7
var.names = names(HealthInsurance)

FiveSum.HI = matrix(NA,length(var.names),6)
colnames(FiveSum.HI) = c("Variable","Min","Q1","Q2","Q3","Max")

for(VAR in seq_along(var.names)){
  if(is.numeric(HealthInsurance[,VAR])){
    MIN=min(HealthInsurance[,VAR])
    Q1=quantile(HealthInsurance[,VAR],0.25)
    Q2=median(HealthInsurance[,VAR],0.5)
    Q3=quantile(HealthInsurance[,VAR],0.75)
    MAX=max(HealthInsurance[,VAR])
    FiveSum.HI[VAR,]=c(names(HealthInsurance)[VAR],MIN,Q1,Q2,Q3,MAX)
  } else {
    cat(str_c("Variable ",var.names[VAR]," is not numeric\n"))
    FiveSum.HI[VAR,]=c(names(HealthInsurance)[VAR],rep(NA,5))
  }
}
print(as.tibble(na.omit(FiveSum.HI)))

FiveSum.HI2 = NULL
Numeric.names = NULL
for(VAR in seq_along(var.names)){
  if(is.numeric(HealthInsurance[,VAR])){
    MIN=min(HealthInsurance[,VAR])
    Q1=quantile(HealthInsurance[,VAR],0.25)
    Q2=median(HealthInsurance[,VAR],0.5)
    Q3=quantile(HealthInsurance[,VAR],0.75)
    MAX=max(HealthInsurance[,VAR])
    FiveSum.HI2=rbind(FiveSum.HI2,c(MIN,Q1,Q2,Q3,MAX))
    Numeric.names=c(Numeric.names,var.names[VAR])
  } 
}
FiveSum.HI3=as.tibble(cbind(Numeric.names,as.tibble(FiveSum.HI2)))
names(FiveSum.HI3) = c("Variable","Min","Q1","Q2","Q3","Max")
print(na.omit(FiveSum.HI3))

#3.1
set.seed(216)
x1=rnorm(BLANK,mean=82,sd=3)
ggplot()+geom_histogram(aes(x1))

set.seed(216)
x2=rbinom(BLANK,size=10,prob=1/6)
ggplot()+geom_bar(aes(x2))

#3.2
prop=rep(NA,1000)
for(k in 1:1000){
  set.seed(k)
  x=sample(c("H","T"),size=k,replace=T,prob=c(0.5,0.5))
  prop[k]=mean(x=="H")
}
ggplot() + 
  geom_line(aes(x=1:1000,y=prop),alpha=0.5) + 
  geom_hline(yintercept=0.5,linetype="dashed",color="red",size=2) +
  theme_minimal()


