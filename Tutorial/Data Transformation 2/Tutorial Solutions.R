library(tidyverse)
library(nycflights13)

names(flights)

#Part 1:
f1a=filter(flights,flight==807)

f1b=select(f1a,flight,carrier,origin,dest)

f1c=rename(f1b,destination=dest)

f1d=arrange(f1c,carrier,origin,destination)
head(f1d,5)

f1e=arrange(f1d,desc(carrier),desc(origin),desc(destination))
head(f1e,18)

#Part2
f2a=transmute(flights,
          dep_hr=dep_time%/%100+(dep_time%%100)/60,
          sched_dep_hr=sched_dep_time%/%100+(sched_dep_time%%100)/60,
          arr_hr=arr_time%/%100+(arr_time%%100)/60,
          sched_arr_hr=sched_arr_time%/%100+(sched_arr_time%%100)/60)
names(f2a)
head(f2a)

f2b=mutate(f2a,
           dep_delay_hr=dep_hr-sched_dep_hr,
           arr_delay_hr=arr_hr-sched_arr_hr)

f2c=mutate(f2b,
           gain_hr=arr_delay_hr-dep_delay_hr,
           percent_gain_hr=percent_rank(gain_hr))

f2d=filter(f2c,percent_gain_hr<0.1|percent_gain_hr>0.9)

f2e=arrange(f2d,desc(percent_gain_hr))
head(f2e,5)

f2e.pipedream = flights %>%
  transmute(dep_hr=dep_time%/%100+(dep_time%%100)/60,
            sched_dep_hr=sched_dep_time%/%100+(sched_dep_time%%100)/60,
            arr_hr=arr_time%/%100+(arr_time%%100)/60,
            sched_arr_hr=sched_arr_time%/%100+(sched_arr_time%%100)/60) %>%
  mutate(dep_delay_hr=dep_hr-sched_dep_hr,
         arr_delay_hr=arr_hr-sched_arr_hr) %>%
  mutate(gain_hr=arr_delay_hr-dep_delay_hr,
         percent_gain_hr=percent_rank(gain_hr)) %>%
  filter(percent_gain_hr<0.1|percent_gain_hr>0.9) %>%
  arrange(desc(percent_gain_hr))

identical(f2e,f2e.pipedream)

#Part 3
f.accuracy<-mutate(flights,
                   dep_hr=dep_time%/%100+(dep_time%%100)/60,
                   sched_dep_hr=sched_dep_time%/%100+(sched_dep_time%%100)/60,
                   arr_hr=arr_time%/%100+(arr_time%%100)/60,
                   sched_arr_hr=sched_arr_time%/%100+(sched_arr_time%%100)/60,
                   dep_delay_hr=dep_hr-sched_dep_hr,
                   arr_delay_hr=arr_hr-sched_arr_hr,
                   accuracy=abs(dep_delay)+(arr_delay))
head(f.accuracy,5)

f.accuracy2=select(f.accuracy,carrier,accuracy)

carrier.summary<-f.accuracy2 %>%
                    group_by(carrier) %>%
                    summarize(
                      mean.accuracy=mean(accuracy,na.rm=T),
                      sd.accuracy=sd(accuracy,na.rm=T)
                    )
carrier.summary
