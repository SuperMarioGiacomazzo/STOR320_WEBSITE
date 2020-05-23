library(tidyverse)
library(nycflights13)
library(knitr)
library(kableExtra)

names(flights)


#Part 1
flight.summary = 
  
  #Initiate the Data Set Summarized
  flights %>%
          
  #Use mutate() to Create a Measure of Accuracy Based on Delay Variables Measured in Minutes
  mutate(
    dep_min=(dep_time%/%100)*60+dep_time%%100,
    sched_dep_min=(sched_dep_time%/%100)*60+sched_dep_time%%100,
    arr_min=(arr_time%/%100)*60+arr_time%%100,
    sched_arr_min=(sched_arr_time%/%100)*60+sched_arr_time%%100,
    dep_delay_min=dep_min-sched_dep_min,
    arr_delay_min=arr_min-sched_arr_min,
    accuracy=sqrt((dep_delay_min)^2+abs(arr_delay_min)^2)
  ) %>%
  
  #Use filter() to Remove Observations Missing Your Accuracy Measure.
  filter(!is.na(accuracy)) %>%
  
  #Use group_by() for future aggregation for each combination of origin, destination, and carrier
  group_by(origin,dest,carrier) %>%

  #Use summarize() to capture the count, mean of accuracy,variance of accuracy, and mean of distance for each combination of origin, destination, and carrier
  summarize(
    count=n(),
    mean.acc=mean(accuracy),
    var.acc=var(accuracy),
    mean.dist=mean(distance)
  ) %>%
  
  #Use filter() to remove all scenarios where the count variable Created is Less than or Equal to 10
  filter(!(count<=10)) %>%
  
  #Ungroup Future Modifications based on combination of origin, destination, and carrier
  ungroup() %>%
  
  #Use mutate() to convert the count variable into a variable called proportion representing the proportion of flights under each combination of origin, destination, and carrier
  mutate(proportion=count/sum(count))

#Part 2
png(file="graph1.png",width=600,height=400)
ggplot(data=flight.summary) +
  geom_point(aes(x=mean.dist,y=mean.acc,color=origin)) +
  geom_smooth(aes(x=mean.dist,y=mean.acc,color=origin),se=F,method="lm")
dev.off()

png(file="graph2.png",width=600,height=400)
ggplot(data=filter(flight.summary,mean.dist<3000)) +
  geom_point(aes(x=mean.dist,y=mean.acc,color=origin)) +
  geom_smooth(aes(x=mean.dist,y=mean.acc,color=origin),se=F,method="lm")
dev.off()


#Part 3
flight.summary2 = 
  flight.summary %>%
  mutate(rank=min_rank(mean.acc)) %>%
  filter(min_rank(mean.acc)<=5 | min_rank(desc(mean.acc))<=5) %>%
  arrange(rank)


