
#For this tutorial, we will use a dataset in the pscl package named presidentialElections. 
#In this data set their are four variables: state, demVote, year, south. 
#For more information, type ?presidentialElections in R Console

library(tidyverse) #Loads the tidyverse package for ggplot
library(pscl) #Loads the pscl package for the dataset
library(gridExtra) #Allows us to combine ggplots into a nice layout

PE=presidentialElections #Assign the dataset to a variable with a smaller name (convenience)

p1<-ggplot(data=PE) +
  geom_point(aes(x=year,y=demVote,color=as.factor(south)),size=2) +
  geom_smooth(aes(x=year,y=demVote,color=as.factor(south))) +
  xlab("") + ylab("% Democratic") + 
  guides(color=guide_legend(title="South")) +
  scale_color_manual(values=c("Blue","Red")) +
  theme_minimal()+
  geom_vline(xintercept=1957,alpha=0.8,linetype=4) +
  ggtitle("USA Change in Democratic Vote")

p1

p2<-ggplot(data=PE[PE$year==1932,]) +
      geom_density(aes(x=demVote,fill=south)) + 
      theme_minimal()+
      guides(color=guide_legend(title="South")) +
      scale_fill_manual(values=c("Blue","Red")) + 
      guides(fill=guide_legend(title="South")) +
      ggtitle("Distribution of Democratic Vote from 1932") +
      xlab("% Democratic") + ylab("Density") +
      geom_vline(xintercept=50)
p2

p3<-ggplot(data=PE[PE$year==2016,]) +
  geom_density(aes(x=demVote,fill=south)) + 
  theme_minimal()+
  guides(color=guide_legend(title="South")) +
  scale_fill_manual(values=c("Blue","Red")) + 
  guides(fill=guide_legend(title="South")) +
  ggtitle("Distribution of Democratic Vote from 2016") +
  xlab("% Democratic") + ylab("Density")+
  geom_vline(xintercept=50)

p3

grid.arrange(p1,p2,p3)
grid.arrange(p1,p2,p3,ncol=2)

matrix(c(1,1,2,3),ncol=2)
LAYOUT=matrix(c(1,1,2,3),ncol=2)

grid.arrange(p1,p2,p3,layout_matrix=LAYOUT)

matrix(c(1,2,1,3),ncol=2)
LAYOUT2=matrix(c(1,2,1,3),ncol=2)
grid.arrange(p1,p2,p3,layout_matrix=LAYOUT2)
