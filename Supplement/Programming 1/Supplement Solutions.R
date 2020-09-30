options(scipen=999)
library(tidyverse)    #Essential Functions


#1.1
x = 3
if(x > 0){
  print(log(x))
}
x = -3
if(x > 0){
  print(log(x))
}


#1.2
x = 3
if(x > 0){
  print(log(x))
} else{
  message("Unable to Take Logarithm")
}
x = -3
if(x > 0){
  print(log(x))
} else {
  message("Unable to Take Logarithm")
}


#1.3
x="Hallelujah"
if(x > 0){
  print(log(x))
}

if(x > 0){
  print(log(x))
} else{
  message("Unable to Take Logarithm")
}


#1.4
x="Hallelujah"
if(is.numeric(x)){
  if(x > 0){
    print(log(x))
  } else{
    message("Unable to Take Logarithm")
  }
} else{
  message("No Strings Attached")
}


#1.5
x=c(-1,3,200)
print(log(x))

y1 =  if(x > 0){
  log(x)
} else{
  NA
}
print(y1)

y2 = ifelse(x>0,log(x),NA)
print(y2)


#1.6
x=rnorm(1000,mean=0,sd=1)
y=ifelse(abs(x)<1,"Within 1 SD",ifelse(abs(x)>2,"Far Far Away","Between 1 and 2 SD"))
y.fct=factor(y,levels=c("Within 1 SD","Between 1 and 2 SD","Far Far Away"))
ggplot() +
  geom_bar(aes(x=y.fct),fill="lightskyblue1") +
  theme_minimal()


#2.1
a=BLANK #Any Number
r=BLANK #Any Number Between -1 and 1: abs(r)<1

theoretical.limit=a/(1-r)

START=a

FINISH.1 = START + a*r^1

FINISH.2 = FINISH.1 + a*r^2

FINISH.3 = FINISH.2 + a*r^3

FINISH.10 = a 
for(k in 1:10){
  FINISH.10=FINISH.10+a*r^k
}

FINISH.100 = a 
for(k in 1:100){
  FINISH.100=FINISH.100+a*r^k
}

DATA = tibble(k=c(1,2,3,10,100,"Infinity"),
              SUMMATION=c(FINISH.1,FINISH.2,FINISH.3,
                          FINISH.10,FINISH.100,
                          theoretical.limit))
print(DATA)

ABSOLUTE.ERROR = abs(FINISH.100-theoretical.limit)
print(ABSOLUTE.ERROR)


#2.2
a=10
r=-2

FINISH=a
k=0
while(abs(FINISH-a/(1-r)) > 1e-10) {
  k=k+1
  FINISH = FINISH + a*r^k
  if(k>100) break
}
print(c(k,FINISH))


#2.3
a=10
r=-0.75
theoretical.limit=a/(1-r)

K=20 #How Many Steps Do You Want to Save?

summation=rep(NA,(K+1))
summation[1]=a
for (k in 1:K) {
  summation[k+1]=summation[k] + a*r^k
}

ggplot() +
  geom_line(aes(x=1:(K+1),y=summation)) +
  geom_hline(yintercept=theoretical.limit,
             linetype="dashed")

