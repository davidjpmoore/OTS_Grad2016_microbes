
#measured 
#aft_cnt mid_cnt mor_cnt

#rep = site (n=3)



#site time location height direction agar
library(dplyr)
library(ggplot2)

#load data from folder called "data"
dummydata = read.csv("./data/La-Selva-Microbe.csv")


#rename columns to avoid confusing with r functions
dummydata01 = dummydata  %>%
  mutate(TimeOfDay=Time, PlateLocation = Location) 


AnalysisANOVA_dummy <- aov(dummyData ~ TimeOfDay  + ## Time effect 
                             PlateLocation + # river vs soil effect
                                Height + # Distance from ground effect
                                Direction + #plate facing up or down 
                                Agar + # type of Agar (testing)
                             
                             TimeOfDay*PlateLocation +
                             TimeOfDay*Height + 
                             TimeOfDay*Direction + 
                             
                             PlateLocation*Height +
                             PlateLocation*Direction +
                             
                             Height*Direction +
                             
                             TimeOfDay*Height*PlateLocation +
                             TimeOfDay*Height*Direction +
                             TimeOfDay*Height*PlateLocation*Direction,
                                     
                              data = dummydata01)

summary(AnalysisANOVA_dummy)




#Plots

dummydata02 = dummydata01 %>%
  filter(dummyData<500)

Dummyplot <- ggplot(dummydata02, aes(x=TimeOfDay, y=dummyData))
# 
Dummyplot + aes(shape = factor(Direction)) + scale_shape(solid = FALSE, name ="Direction") +
  geom_boxplot(lwd=1) +
  geom_point(aes( shape = factor(Direction)), size = 2, position = "jitter") +
  theme_classic() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
 # theme(axis.ticks.length=unit(-0.25, "cm"), axis.ticks.margin=unit(0.5, "cm"))+
  ylab("Dummy Colony Counts")+
  xlab("Time of Day") 

