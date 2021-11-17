

library(tidyverse)
library(githubinstall)
devtools::install_github("katiejolly/nationalparkcolors")
library(nationalparkcolors)
library(codyn)
library(ggplot2)
library(ggpubr)
library(magrittr)

#######################################
####  SPRING 2019 - EXP 1  ###
####### Read-In Datasets ######
LLPdata<-read.csv("LLP_Data_Spring2019.csv",header=T) %>% 
  #Edit weeks so that they are correct based on numbers and week timeline
mutate(Week=as.factor(ifelse(Date=="1/22/2019",2,ifelse(Date=="3/7/2019",8,ifelse(Date=="3/11/2019",9,ifelse(Date=="3/12/2019",9,ifelse(Date=="3/13/2019",9,ifelse(Date=="3/14/2019",9,ifelse(Date=="3/15/2019",9,ifelse(Date=="3/18/2019",10,Week))))))))))
Biomass<-read.csv("Biomass_LLP_Data_Spring2019.csv", header=T)%>%
  group_by(PlantID)%>%
  mutate(AvgAlive=mean(Alive,na.rm=T))%>%
  mutate(AvgDead=mean(Dead,na.rm=T))
LLPBio<-subset(Biomass,!duplicated(subset(Biomass,select=c(PlantID))))
LLPBiomass<-LLPBio%>%
  group_by(PlantID)%>%
  mutate(AvgTotal=sum(AvgAlive,AvgDead,na.rm=T))%>%
  select(-Alive,-Dead)
LLPLA<-read.csv("LA_LLP_Data_Spring2019.csv", header=T)
#####################################################################################
### Average the measurements taken on same plant per week ###
hwMLL<-LLPdata%>%
  group_by(PlantID,Week,Treatment)%>%
  filter(Week=="7")
hwMLL2<-hwMLL%>%
  group_by(Plant_Species,Treatment,PlantID)%>%
  summarise(AvgMLL=mean(Max_Leaf_Length,na.rm=T),avgMPH=mean(Max_Height,na.rm=T),
            avgstems=mean(Stem_Number,na.rm=T))
hwMLL3<-na.omit(hwMLL2)

# Number of individuals for each species - growth measures #
hwMLL3%>% group_by(Plant_Species,Treatment) %>% tally()
# Number of individuals for each species - ANPP #
LLPBiomass%>% group_by(Plant_Species,Treatment) %>% tally()
# Number of individuals for each species - LA #
LLPLA%>% group_by(Plant_Species,Treatment) %>% tally()

theme_set(theme_bw(18))
######################################################################
# STATS TESTS W/ ADJUSTMENTS FOR BIOTIC VARIABLES
############# BIOMASS ###########################
AGBiomass<-subset(LLPBiomass,c(Plant_Species=="Andropogon gerardii"))
ASBiomass<-subset(LLPBiomass,c(Plant_Species=="Aristida stricta"))
SSBiomass<-subset(LLPBiomass,c(Plant_Species=="Schizachyrium scoparium"))
SNBiomass<-subset(LLPBiomass,c(Plant_Species=="Sorghastrum nutans"))

wilcox.test(AGBiomass$AvgTotal~AGBiomass$Treatment)
wilcox.test(ASBiomass$AvgTotal~ASBiomass$Treatment)
wilcox.test(SSBiomass$AvgTotal~SSBiomass$Treatment)
wilcox.test(SNBiomass$AvgTotal~SNBiomass$Treatment)

pval<-c(0.285,0.6951,0.0238,0.07389)
p.adjust(pval,method="BH",n=5)

############## MLL #########################
AGMLL<-subset(hwMLL3,c(Plant_Species=="Andropogon gerardii"))
ASMLL<-subset(hwMLL3,c(Plant_Species=="Aristida stricta"))
SSMLL<-subset(hwMLL3,c(Plant_Species=="Schizachyrium scoparium"))
SNMLL<-subset(hwMLL3,c(Plant_Species=="Sorghastrum nutans"))

wilcox.test(AGMLL$AvgMLL~AGMLL$Treatment)
wilcox.test(ASMLL$AvgMLL~ASMLL$Treatment)
wilcox.test(SSMLL$AvgMLL~SSMLL$Treatment)
wilcox.test(SNMLL$AvgMLL~SNMLL$Treatment)

pval1<-c(0.9249,0.511,0.07664,0.9873)
p.adjust(pval1,method="BH",n=5)

################# LA #####################
AGLA<-subset(LLPLA,c(Plant_Species=="Andropogon gerardii"))
ASLA<-subset(LLPLA,c(Plant_Species=="Aristida stricta"))
SSLA<-subset(LLPLA,c(Plant_Species=="Schizachyrium scoparium"))
SNLA<-subset(LLPLA,c(Plant_Species=="Sorghastrum nutans"))

wilcox.test(AGLA$LA~AGLA$Treatment)
wilcox.test(ASLA$LA~ASLA$Treatment)
wilcox.test(SSLA$LA~SSLA$Treatment)
wilcox.test(SNLA$LA~SNLA$Treatment)

pval2<-c(0.7827,0.02269,0.2938,0.7045)
p.adjust(pval2,method="BH",n=5)

################# MPH #####################
AGMPH<-subset(hwMLL3,c(Plant_Species=="Andropogon gerardii"))
ASMPH<-subset(hwMLL3,c(Plant_Species=="Aristida stricta"))
SSMPH<-subset(hwMLL3,c(Plant_Species=="Schizachyrium scoparium"))
SNMPH<-subset(hwMLL3,c(Plant_Species=="Sorghastrum nutans"))

wilcox.test(AGMPH$avgMPH~AGMPH$Treatment)
wilcox.test(ASMPH$avgMPH~ASMPH$Treatment)
wilcox.test(SSMPH$avgMPH~SSMPH$Treatment)
wilcox.test(SNMPH$avgMPH~SNMPH$Treatment)

pval3<-c(0.2423,0.01829,0.00434,0.7261)
p.adjust(pval3,method="BH",n=5)

################# Stems #####################
AGstems<-subset(hwMLL3,c(Plant_Species=="Andropogon gerardii"))
ASstems<-subset(hwMLL3,c(Plant_Species=="Aristida stricta"))
SSstems<-subset(hwMLL3,c(Plant_Species=="Schizachyrium scoparium"))
SNstems<-subset(hwMLL3,c(Plant_Species=="Sorghastrum nutans"))

wilcox.test(AGstems$avgstems~AGMPH$Treatment)
wilcox.test(ASstems$avgstems~ASMPH$Treatment)
wilcox.test(SSstems$avgstems~SSMPH$Treatment)
wilcox.test(SNstems$avgstems~SNMPH$Treatment)

pval4<-c(0.9697,0.5259,0.573,0.7853)
p.adjust(pval4,method="BH",n=5)
#########################################################

### Tests of Normality for Biotic Variables ###
#MLL# - NOT NORMAL
ggdensity(hwMLL3$AvgMLL)
ggqqplot(hwMLL3$AvgMLL)
shapiro.test(hwMLL3$AvgMLL)
##############################################
#MPH# - NOT NORMAL
ggdensity(hwMLL3$avgMPH)
ggqqplot(hwMLL3$avgMPH)
shapiro.test(hwMLL3$avgMPH)
##############################################
#Stems# - NOT NORMAL
ggdensity(hwMLL3$avgstems)
ggqqplot(hwMLL3$avgstems)
shapiro.test(hwMLL3$avgstems)
#################################################
#ANPP# - NOT NORMAL
ggdensity(LLPBiomass$AvgTotal)
ggqqplot(LLPBiomass$AvgTotal)
shapiro.test(LLPBiomass$AvgTotal)
#####################################################
#LA# - NOT NORMAL
ggdensity(LLPLA$LA)
ggqqplot(LLPLA$LA)
shapiro.test(LLPLA$LA)
###################################################################################

### Manuscript Figures ###
pal<-park_palette("Saguaro")
Totalbox<-ggplot(LLPBiomass,aes(x=Plant_Species,y=AvgTotal,fill=Treatment))+
  geom_boxplot()+xlab("Treatment")+ylab("Aboveground Production (g/plug)")+
  theme(panel.background = element_rect(fill="transparent",colour=NA), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill="transparent",colour=NA))+
  scale_y_continuous(limits=c(0,2.5))+
  scale_fill_manual(values=pal,name="Treatment")
Totalbox
ggsave("Sp19Prod.png", Totalbox, bg = "transparent")

MLLbox<-ggplot(hwMLL3,aes(x=Plant_Species,y=AvgMLL,fill=Treatment))+
  geom_boxplot()+xlab("Treatment")+ylab("Maximum Leaf Length (mm)")+
  theme(panel.background = element_rect(fill="transparent",colour=NA), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill="transparent",colour=NA))+
  scale_y_continuous(limits=c(0,600))+
  scale_fill_manual(values=pal)
MLLbox
ggsave("Sp19MLL.png",MLLbox, bg = "transparent")

LAbox<-ggplot(LLPLA,aes(x=Plant_Species,y=LA,fill=Treatment))+
  geom_boxplot()+xlab("Treatment")+ylab("Leaf Area (cm^2)")+
  theme(panel.background = element_rect(fill="transparent",colour=NA), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill="transparent",colour=NA))+
  scale_y_continuous(limits=c(0,30))+
  scale_fill_manual(values=pal,name="Treatment")
LAbox
ggsave("Sp19LA.png",LAbox, bg = "transparent")

Stembox<-ggplot(hwMLL3,aes(x=Plant_Species,y=avgstems,fill=Treatment))+
  geom_boxplot()+xlab("Treatment")+ylab("Stem Number")+
  theme(panel.background = element_rect(fill="transparent",colour=NA), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill="transparent",colour=NA))+
  scale_y_continuous(limits=c(0,40))+
  scale_fill_manual("Treatment",values=pal)
Stembox
ggsave("Sp19Stems.png",Stembox, bg = "transparent")

MPHbox<-ggplot(hwMLL3,aes(x=Plant_Species,y=avgMPH,fill=Treatment))+
  geom_boxplot()+xlab("Treatment")+ylab("Maximum Plant Height")+
  theme(panel.background = element_rect(fill="transparent",colour=NA), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill="transparent",colour=NA))+
  scale_y_continuous(limits=c(0,400))+
  scale_fill_manual("Treatment",values=pal)
MPHbox
ggsave("Sp19MPH.png",MPHbox, bg = "transparent")

######### ABIOTIC VARIABLES ######
pal<-park_palette("Saguaro")
pals<-park_palette("Badlands")

Temp_Humidity<-LLPdata%>%
  select(Week,Humidity,Air_Temp,Treatment,Light,Soil_Moisture,Section,Group) %>% 
  #Filter out temperature irregularities
  filter(Air_Temp!=3.2) %>% 
  #change air temps that are seemingly in Fahrenheit not Celsius
  mutate(Air_Temp=ifelse(Air_Temp==79,26.1,ifelse(Air_Temp==75.6,24.22,ifelse(Air_Temp==68.4,20.2,ifelse(Air_Temp==67.8,19.9,Air_Temp))))) %>% 
  group_by(Treatment,Week,Section,Group) %>%
  summarize(Section_Humidity=mean(Humidity),Section_Air_Temp=mean(Air_Temp),Section_Soil_Moisture=mean(Soil_Moisture),Section_Light=mean(Light)) 


Humidity<-Temp_Humidity %>% 
  select(Treatment,Week,Section,Group,Section_Humidity) %>% 
  na.omit() %>% 
  group_by(Treatment,Week) %>% 
  summarize(Humidity_std=sd(Section_Humidity),Humidity_Mean=mean(Section_Humidity),Humidity_n=length(Section_Humidity))%>%
  mutate(Humidity_St_Error=Humidity_std/sqrt(Humidity_n)) %>% 
  ungroup()

Air_Temp<-Temp_Humidity %>% 
  select(Treatment,Week,Section,Group,Section_Air_Temp) %>% 
  na.omit() %>% 
  group_by(Treatment,Week)%>% 
  summarize(Air_Temp_std=sd(Section_Air_Temp),Air_Temp_Mean=mean(Section_Air_Temp),Air_Temp_n=length(Section_Air_Temp))%>%
  mutate(Air_Temp_St_Error=Air_Temp_std/sqrt(Air_Temp_n)) %>% 
  ungroup()

Soil_Moisture<-Temp_Humidity %>% 
  select(Treatment,Week,Section,Group,Section_Soil_Moisture) %>%
  na.omit() %>% 
  group_by(Treatment,Week) %>% 
  summarize(Soil_Moisture_std=sd(Section_Soil_Moisture),Soil_Moisture_Mean=mean(Section_Soil_Moisture),Soil_Moisture_n=length(Section_Soil_Moisture))%>%
  mutate(Soil_Moisture_St_Error=Soil_Moisture_std/sqrt(Soil_Moisture_n)) %>% 
  ungroup()

Light<-Temp_Humidity %>% 
  select(Treatment,Week,Section,Group,Section_Light) %>% 
  na.omit() %>% 
  group_by(Treatment,Week) %>% 
  ungroup() %>% 
  group_by(Week,Treatment) %>% 
  summarize(Light_std=sd(Section_Light),Light_Mean=mean(Section_Light),Light_n=length(Section_Light))%>%
  mutate(Light_St_Error=Light_std/sqrt(Light_n)) %>% 
  ungroup()

colour_palette<-park_palette("Saguaro",2)


#### Graph Humidity
sp2019_Humidity<-ggplot(Humidity,aes(x=Week, y=Humidity_Mean,group=Treatment))+
  geom_point(aes(color=Treatment,shape=Treatment),size=5)+
  geom_line(aes(color=Treatment,linetype=Treatment),size=2)+
  scale_linetype_manual(values=c("solid","dashed"))+
  scale_color_manual(values = colour_palette)+
  scale_shape_manual(values=c(1,16))+
  geom_errorbar(aes(ymin=Humidity_Mean-Humidity_St_Error,ymax=Humidity_Mean+Humidity_St_Error),width=0.2)+
  #Label the x-axis "Treatment"
  xlab(element_blank())+
  #Label the y-axis "Species Richness"
  ylab("Average Humidity (%)")+
  expand_limits(y=c(0,90))+
  #put the lines slightly on either side of week 9 and 10 so we can still see the points
  geom_vline(xintercept = 4.9, colour = "blue",size=1)+
  geom_vline(xintercept = 6.9, colour = "blue",size=1)+
  theme_bw()+
  theme(legend.position = "none",legend.title=element_blank(),legend.background=element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),text=element_text(size=30),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(color="black"))+
  annotate("text",x=0.9,y=90,label="d)",size=10)

#### Graph Temperature
sp2019_Temp<-ggplot(Air_Temp,aes(x=Week, y=Air_Temp_Mean,group=Treatment))+
  geom_point(aes(color=Treatment,shape=Treatment),size=5)+
  geom_line(aes(color=Treatment,linetype=Treatment),size=2)+
  scale_linetype_manual(values=c("solid","dashed"))+
  scale_color_manual(values = colour_palette)+
  scale_shape_manual(values=c(1,16))+
  geom_errorbar(aes(ymin=Air_Temp_Mean-Air_Temp_St_Error,ymax=Air_Temp_Mean+Air_Temp_St_Error),width=0.2)+
  #Label the x-axis "Treatment"
  xlab(element_blank())+
  #Label the y-axis "Species Richness"
  ylab("Average Temperature (C)")+
  expand_limits(y=c(0,40))+
  geom_vline(xintercept = 4.9, colour = "blue",size=1)+
  geom_vline(xintercept = 6.9, colour = "blue",size=1)+
  theme_bw()+
  theme(legend.position = "none",legend.title=element_blank(),legend.background=element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),text=element_text(size=30),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(color="black"))+
  annotate("text",x=0.9,y=40,label="a)",size=10)

#### Graph Soil Moisture
sp2019_Soil_Moisture<-ggplot(Soil_Moisture,aes(x=Week, y=Soil_Moisture_Mean,group=Treatment))+
  geom_point(aes(color=Treatment,shape=Treatment),size=5)+
  geom_line(aes(color=Treatment,linetype=Treatment),size=2)+
  scale_linetype_manual(values=c("solid","dashed"))+
  scale_color_manual(values = colour_palette)+
  scale_shape_manual(values=c(1,16))+
  geom_errorbar(aes(ymin=Soil_Moisture_Mean-Soil_Moisture_St_Error,ymax=Soil_Moisture_Mean+Soil_Moisture_St_Error),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Week")+
  #Label the y-axis "Species Richness"
  ylab("Soil Moisture")+
  expand_limits(y=c(0,40))+
  geom_vline(xintercept = 4.9, colour = "blue",size=1)+
  geom_vline(xintercept = 6.9, colour = "blue",size=1)+
  theme_bw()+
  theme(legend.position = "none",legend.title=element_blank(),legend.background=element_blank(),text=element_text(size=30),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(color="black"))+
  annotate("text",x=0.9,y=39,label="g)",size=10)

#### Light
ggplot(Light,aes(x=Week, y=Light_Mean,group=Treatment))+
  geom_point(aes(color=Treatment,shape=Treatment),size=5)+
  geom_line(aes(color=Treatment,linetype=Treatment),size=2)+
  scale_linetype_manual(values=c("solid","dashed"))+
  scale_color_manual(values = colour_palette)+
  scale_shape_manual(values=c(1,16))+
  geom_errorbar(aes(ymin=Light_Mean-Light_St_Error,ymax=Light_Mean+Light_St_Error),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Week")+
  #Label the y-axis "Species Richness"
  ylab("Light")+
  expand_limits(y=c(0,10000))+
  geom_vline(xintercept = 4.9, colour = "blue",size=1)+
  geom_vline(xintercept = 6.9, colour = "blue",size=1)+
  theme_bw()+
  theme(legend.position = c(0.9,0.95),legend.title=element_blank(),legend.background=element_blank(),text=element_text(size=30),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



#Look at Histogram to determine if data is normally distrubuted
hist(Temp_Humidity$Section_Humidity,main="Sp 2019 Humidity") 
#testing for normality - H0 is that the data is normally distributed, therefore significant P-value means data is not normal OR the sample size is too small
shapiro.test(Temp_Humidity$Section_Humidity)

hist(Temp_Humidity$Section_Air_Temp,main="Sp 2019 Air Temp")
shapiro.test(Temp_Humidity$Section_Air_Temp)

hist(Temp_Humidity$Section_Soil_Moisture,main="Sp 2019 Soil Moisture") 
shapiro.test(Temp_Humidity$Section_Soil_Moisture)

hist(Temp_Humidity$Section_Light,main="Sp 2019 Light") 
shapiro.test(Temp_Humidity$Section_Soil_Moisture)




#### T Tests

#Week 1
Temp_Humidity_TTests_Week1<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==1) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week1$Section_Humidity~Temp_Humidity_TTests_Week1$Treatment)
wilcox.test(Temp_Humidity_TTests_Week1$Section_Air_Temp~Temp_Humidity_TTests_Week1$Treatment)
wilcox.test(Temp_Humidity_TTests_Week1$Section_Soil_Moisture~Temp_Humidity_TTests_Week1$Treatment)
wilcox.test(Temp_Humidity_TTests_Week1$Section_Light~Temp_Humidity_TTests_Week1$Treatment)

#Week 2
Temp_Humidity_TTests_Week2<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==2) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week2$Section_Humidity~Temp_Humidity_TTests_Week2$Treatment)
wilcox.test(Temp_Humidity_TTests_Week2$Section_Air_Temp~Temp_Humidity_TTests_Week2$Treatment)
wilcox.test(Temp_Humidity_TTests_Week2$Section_Soil_Moisture~Temp_Humidity_TTests_Week2$Treatment)
wilcox.test(Temp_Humidity_TTests_Week2$Section_Light~Temp_Humidity_TTests_Week2$Treatment)

#Week 3
Temp_Humidity_TTests_Week3<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==3) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week3$Section_Humidity~Temp_Humidity_TTests_Week3$Treatment)
wilcox.test(Temp_Humidity_TTests_Week3$Section_Air_Temp~Temp_Humidity_TTests_Week3$Treatment)
wilcox.test(Temp_Humidity_TTests_Week3$Section_Soil_Moisture~Temp_Humidity_TTests_Week3$Treatment)
wilcox.test(Temp_Humidity_TTests_Week3$Section_Light~Temp_Humidity_TTests_Week3$Treatment)

#Week 4
Temp_Humidity_TTests_Week4<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==4) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week4$Section_Humidity~Temp_Humidity_TTests_Week4$Treatment)
wilcox.test(Temp_Humidity_TTests_Week4$Section_Air_Temp~Temp_Humidity_TTests_Week4$Treatment)
wilcox.test(Temp_Humidity_TTests_Week4$Section_Soil_Moisture~Temp_Humidity_TTests_Week4$Treatment)
wilcox.test(Temp_Humidity_TTests_Week4$Section_Light~Temp_Humidity_TTests_Week4$Treatment)

#Week 5
Temp_Humidity_TTests_Week5<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==5) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week5$Section_Humidity~Temp_Humidity_TTests_Week5$Treatment)
wilcox.test(Temp_Humidity_TTests_Week5$Section_Air_Temp~Temp_Humidity_TTests_Week5$Treatment)
wilcox.test(Temp_Humidity_TTests_Week5$Section_Soil_Moisture~Temp_Humidity_TTests_Week5$Treatment)
wilcox.test(Temp_Humidity_TTests_Week5$Section_Light~Temp_Humidity_TTests_Week5$Treatment)

#Week 6
Temp_Humidity_TTests_Week6<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==6) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week6$Section_Humidity~Temp_Humidity_TTests_Week6$Treatment)
wilcox.test(Temp_Humidity_TTests_Week6$Section_Air_Temp~Temp_Humidity_TTests_Week6$Treatment)
wilcox.test(Temp_Humidity_TTests_Week6$Section_Soil_Moisture~Temp_Humidity_TTests_Week6$Treatment)
wilcox.test(Temp_Humidity_TTests_Week6$Section_Light~Temp_Humidity_TTests_Week6$Treatment)

#Week 7
Temp_Humidity_TTests_Week7<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==7) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week7$Section_Humidity~Temp_Humidity_TTests_Week7$Treatment)
wilcox.test(Temp_Humidity_TTests_Week7$Section_Air_Temp~Temp_Humidity_TTests_Week7$Treatment)
wilcox.test(Temp_Humidity_TTests_Week7$Section_Soil_Moisture~Temp_Humidity_TTests_Week7$Treatment)
wilcox.test(Temp_Humidity_TTests_Week7$Section_Light~Temp_Humidity_TTests_Week7$Treatment)

# Week 8
Temp_Humidity_TTests_Week8<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==8) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week8$Section_Humidity~Temp_Humidity_TTests_Week8$Treatment)
wilcox.test(Temp_Humidity_TTests_Week8$Section_Air_Temp~Temp_Humidity_TTests_Week8$Treatment)
wilcox.test(Temp_Humidity_TTests_Week8$Section_Soil_Moisture~Temp_Humidity_TTests_Week8$Treatment)
wilcox.test(Temp_Humidity_TTests_Week8$Section_Light~Temp_Humidity_TTests_Week8$Treatment)

#### Week 9
Temp_Humidity_TTests_Week9<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==9) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week9$Section_Humidity~Temp_Humidity_TTests_Week9$Treatment)
wilcox.test(Temp_Humidity_TTests_Week9$Section_Air_Temp~Temp_Humidity_TTests_Week9$Treatment)
wilcox.test(Temp_Humidity_TTests_Week9$Section_Soil_Moisture~Temp_Humidity_TTests_Week9$Treatment)
wilcox.test(Temp_Humidity_TTests_Week9$Section_Light~Temp_Humidity_TTests_Week9$Treatment)

#### Week 10
Temp_Humidity_TTests_Week10<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==10) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week10$Section_Humidity~Temp_Humidity_TTests_Week10$Treatment)
wilcox.test(Temp_Humidity_TTests_Week10$Section_Air_Temp~Temp_Humidity_TTests_Week10$Treatment)
wilcox.test(Temp_Humidity_TTests_Week10$Section_Soil_Moisture~Temp_Humidity_TTests_Week10$Treatment)
wilcox.test(Temp_Humidity_TTests_Week10$Section_Light~Temp_Humidity_TTests_Week10$Treatment)

#Make a list of all p-values and use p.adjust to correct all p-values at once 
####Need to be properly adjusted based on new p-values after correcting for week ####

#Make lists of all p-values for each abiotic measurement - ordered by week
#only 8 weeks because week 8 and week 10 do not have enough data to compare
pval_H_Sp19<-c(0.9526,0.6616,1.0,1.0,0.007723,0.0007884,0.9667,0.9879)
pval_AT_Sp19<-c(0.9506,0.7732,0.9764,1.0,0.002299,0.3427,0.8947,1.0,1.0)
pval_SM_Sp19<-c(0.8923,0.7374,0.8941,0.8072,0.003079,4.048e-11,6.271e-09,1.0,0.01879,0.10)
pval_L_Sp19<-c(0.9148,0.8231,0.7499,0.7945,0.01989,0.03449,1.0,0.977,0.7)

#adjust p-value using padjust for each abiotic measurement
#ran with n=8 because week 8 and 10 did not have enough data for stats
p.adjust(pval_H_Sp19, method = "BH", n=8)
#ran with n=9 becuase week 8 did not have enough data for stats
p.adjust(pval_AT_Sp19, method = "BH", n=9)
p.adjust(pval_SM_Sp19, method = "BH", n=10)
#ran with n=9 because week 8 did not have enough data for stats
p.adjust(pval_L_Sp19, method = "BH", n=9)

#### Make graph with all temp, humidity, and soil moisture graphs
plot_grid(sp2019_Temp,fa2019_Temp,sp2020_Temp,sp2019_Humidity,fa2019_Humidity,sp2020_Humidity,sp2019_Soil_Moisture,fa2019_Soil_Moisture,sp2020_Soil_Moisture)

####################################################################################
##########################################################################
####  FALL 2019 - EXP 2  ###
####### Read-In Datasets #######
data<-read_csv("LLP_Data_Fall2019.csv",col_types=cols(Plant_Stress=col_character(),Week=col_factor(levels=c("1","2","3","4","5","6","7","8","9","10","11")))) %>% 
  #Edit weeks so that they are correct based on numbers and week timeline
mutate(Week=as.factor(ifelse(Date=="10.22.2019",9,ifelse(Date=="10.23.2019",9,ifelse(Date=="10.24.2019",9,ifelse(Date=="10.28.2019",10,ifelse(Date=="10.30.2019",10,ifelse(Date=="10.31.2019",10,ifelse(Date=="10.29.2019",10,Week)))))))))
LLPBiomass<-read.csv("Biomass_LLP_Data_Fall2019.csv", header=T)%>%
  group_by(PlantID)%>%
  mutate(TotalBiomass=sum(Alive,Dead,na.rm=T))
RootBiomass<-read.csv("RootBiomass_LLP_Fall2019.csv", header=T)%>%
  select(-Initials,-Notes)
LLPLA<-read.csv("LA_LLP_Data_Fall2019.csv", header=T)
#################################################################
### Average the measurements taken on same plant per week ###
MLLgraphs<-data%>%
  group_by(PlantID,Week,Treatment)%>%
  filter(Week=="9")
MLLgraphs2.1<-MLLgraphs%>%
  group_by(Treatment,PlantID)%>%
  summarise(avgMLL=mean(Max_Leaf_Length,na.rm=T),avgMPH=mean(Max_Height,na.rm=T),
            avgstems=mean(Stem_Number,na.rm=T))
MLLgraphs3<-na.omit(MLLgraphs2.1)
#####################################

# Number of individuals for each Treatment - growth measures #
MLLgraphs3%>% group_by(Treatment) %>% tally()
# Number of individuals for each Treatment - ANPP #
LLPBiomass%>% group_by(Treatment) %>% tally()
# Number of individuals for each Treatment - BNPP #
RootBiomass%>% group_by(Treatment) %>% tally()
# Number of individuals for each Treatment - LA #
LLPLA%>% group_by(Treatment) %>% tally()

######################################################################
### Tests of Normality for Biotic Variables ###
#MLL# - NOT NORMAL
ggdensity(MLLgraphs3$avgMLL)
ggqqplot(MLLgraphs3$avgMLL)
shapiro.test(MLLgraphs3$avgMLL)
####################################################
#MPH# - NOT NORMAL
ggdensity(MLLgraphs3$avgMPH)
ggqqplot(MLLgraphs3$avgMPH)
shapiro.test(MLLgraphs3$avgMPH)
##################################################
#Stems# - NOT NORMAL
ggdensity(MLLgraphs3$avgstems)
ggqqplot(MLLgraphs3$avgstems)
shapiro.test(MLLgraphs3$avgstems)
###################################################
#ANPP# - NORMAL
ggdensity(LLPBiomass$TotalBiomass)
ggqqplot(LLPBiomass$TotalBiomass)
shapiro.test(LLPBiomass$TotalBiomass)
##################################################
#BNPP - NOT NORMAL
ggdensity(RootBiomass$RootBiomass)
ggqqplot(RootBiomass$RootBiomass)
shapiro.test(RootBiomass$RootBiomass)
###############################################
#LA# - NOT NORMAL
ggdensity(LLPLA$LA)
ggqqplot(LLPLA$LA)
shapiro.test(LLPLA$LA)
###############################################################

# STATS TESTS W/ ADJUSTMENTS FOR BIOTIC VARIABLES #
wilcox.test(MLLgraphs3$avgMLL~MLLgraphs3$Treatment)
wilcox.test(MLLgraphs3$avgMPH~MLLgraphs3$Treatment)
wilcox.test(MLLgraphs3$avgstems~MLLgraphs3$Treatment)
wilcox.test(LLPBiomass$TotalBiomass~LLPBiomass$Treatment)
wilcox.test(RootBiomass$RootBiomass~RootBiomass$Treatment)
wilcox.test(LLPLA$LA~LLPLA$Treatment)

pval5<-c(0.1096,2.661e-09,2.434e-05,0.005233,4.948e-05,7.485e-13)
p.adjust(pval5,method="BH",n=6)

##############################################################################
Rootbox<-ggplot(RootBiomass,aes(x=Treatment,y=RootBiomass,fill=Treatment))+
  geom_boxplot()+xlab("Treatment")+ylab("Belowground Biomass (g/plug)")+
  theme(panel.background = element_rect(fill="transparent",colour=NA), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill="transparent",colour=NA))+
  scale_y_continuous(limits=c(0,3.0))+
  scale_fill_manual("Treatment",values=pal)
Rootbox
ggsave("F19Root.png",Rootbox, bg = "transparent")

Ratiobox<-ggplot(ABratio,aes(x=Treatment,y=ratio,fill=Treatment))+
  geom_boxplot()+xlab("Treatment")+ylab("Ratio of ANPP/BNPP")+
  theme(panel.background = element_rect(fill="transparent",colour=NA), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill="transparent",colour=NA))+
  scale_y_continuous(limits=c(0,3.0))+
  scale_fill_manual("Treatment",values=pal)
Ratiobox
ggsave("F19Ratio.png",Ratiobox, bg = "transparent")

Totalbox<-ggplot(LLPBiomass,aes(x=Treatment,y=TotalBiomass,fill=Treatment))+
  geom_boxplot()+xlab("Treatment")+ylab("Aboveground Production (g/plug)")+
  theme(panel.background = element_rect(fill="transparent",colour=NA), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill="transparent",colour=NA))+
  scale_y_continuous(limits=c(0,3.0))+
  scale_fill_manual("Treatment",values=pal)
Totalbox
ggsave("F19Prod.png",Totalbox, bg = "transparent")


theme_set(theme_bw(16))

MLLbox<-ggplot(MLLgraphs3,aes(x=Treatment,y=avgMLL,fill=Treatment))+
  geom_boxplot()+xlab("Treatment")+ylab("Maximum Leaf Length (mm)")+
  theme(panel.background = element_rect(fill="transparent",colour=NA), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill="transparent",colour=NA))+
  scale_y_continuous(limits=c(0,850))+
  scale_fill_manual("Treatment",values=pal)
MLLbox
ggsave("F19MLL.png",MLLbox, bg = "transparent")

LAbox<-ggplot(LLPLA,aes(x=Treatment,y=LA,fill=Treatment))+
  geom_boxplot()+xlab("Treatment")+ylab("Leaf Area (cm^2)")+
  theme(panel.background = element_rect(fill="transparent",colour=NA), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill="transparent",colour=NA))+
  scale_y_continuous(limits=c(0,60))+
  scale_fill_manual("Treatment",values=pal)
LAbox
ggsave("F19LA.png",LAIbox, bg = "transparent")

Stembox<-ggplot(MLLgraphs3,aes(x=Treatment,y=avgstems,fill=Treatment))+
  geom_boxplot()+xlab("Treatment")+ylab("Stem Number")+
  theme(panel.background = element_rect(fill="transparent",colour=NA), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill="transparent",colour=NA))+
  scale_y_continuous(limits=c(0,40))+
  scale_fill_manual("Treatment",values=pal)
Stembox
ggsave("F19Stem.png",Stembox, bg = "transparent")

MPHbox<-ggplot(MLLgraphs3,aes(x=Treatment,y=avgMPH,fill=Treatment))+
  geom_boxplot()+xlab("Treatment")+ylab("Maximum Plant Height")+
  theme(panel.background = element_rect(fill="transparent",colour=NA), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill="transparent",colour=NA))+
  scale_y_continuous(limits=c(0,850))+
  scale_fill_manual("Treatment",values=pal)
MPHbox
ggsave("F19MPH.png",MPHbox, bg = "transparent")

######### ABIOTIC VARIABLES ######
colour_palette<-park_palette("Saguaro",2)

Temp_Humidity<-data %>% 
  select(Week,Humidity,Air_Temp,Treatment,Light,Soil_Moisture,Section,Group) %>% 
  group_by(Treatment,Week,Section,Group) %>%
  summarize(Section_Humidity=mean(Humidity),Section_Air_Temp=mean(Air_Temp),Section_Soil_Moisture=mean(Soil_Moisture),Section_Light=mean(Light)) 

Humidity<-Temp_Humidity %>% 
  select(Treatment,Week,Section,Group,Section_Humidity) %>% 
  group_by(Treatment,Week) %>% 
  summarize(Humidity_std=sd(Section_Humidity),Humidity_Mean=mean(Section_Humidity),Humidity_n=length(Section_Humidity))%>%
  mutate(Humidity_St_Error=Humidity_std/sqrt(Humidity_n)) %>% 
  ungroup()

Air_Temp<-Temp_Humidity %>% 
  select(Treatment,Week,Section,Group,Section_Air_Temp) %>% 
  group_by(Treatment,Week)%>% 
  summarize(Air_Temp_std=sd(Section_Air_Temp),Air_Temp_Mean=mean(Section_Air_Temp),Air_Temp_n=length(Section_Air_Temp))%>%
  mutate(Air_Temp_St_Error=Air_Temp_std/sqrt(Air_Temp_n))

Soil_Moisture<-Temp_Humidity %>% 
  select(Treatment,Week,Section,Group,Section_Soil_Moisture) %>% 
  group_by(Treatment,Week) %>% 
  summarize(Soil_Moisture_std=sd(Section_Soil_Moisture),Soil_Moisture_Mean=mean(Section_Soil_Moisture),Soil_Moisture_n=length(Section_Soil_Moisture))%>%
  mutate(Soil_Moisture_St_Error=Soil_Moisture_std/sqrt(Soil_Moisture_n))

Light<-Temp_Humidity %>% 
  select(Treatment,Week,Section,Group,Section_Light) %>% 
  group_by(Treatment,Week) %>% 
  ungroup() %>% 
  group_by(Week,Treatment) %>% 
  summarize(Light_std=sd(Section_Light),Light_Mean=mean(Section_Light),Light_n=length(Section_Light))%>%
  mutate(Light_St_Error=Light_std/sqrt(Light_n))

#### Graph Humidity
fa2019_Humidity<-ggplot(Humidity,aes(x=Week, y=Humidity_Mean,group=Treatment))+
  geom_point(aes(color=Treatment,shape=Treatment),size=5)+
  geom_line(aes(color=Treatment,linetype=Treatment),size=2)+
  scale_linetype_manual(values=c("solid","dashed"))+
  scale_color_manual(values = colour_palette)+
  scale_shape_manual(values=c(1,16))+
  geom_errorbar(aes(ymin=Humidity_Mean-Humidity_St_Error,ymax=Humidity_Mean+Humidity_St_Error),width=0.2)+
  #Label the x-axis "Treatment"
  xlab(element_blank())+
  #Label the y-axis "Species Richness"
  ylab(element_blank())+
  expand_limits(y=c(0,90))+
  #put the lines slightly on either side of week 9 and 10 so we can still see the points
  geom_vline(xintercept = 7.9, colour = "blue",size=1)+
  geom_vline(xintercept = 9.1, colour = "blue",size=1)+
  theme_bw()+
  theme(legend.position = "none",legend.title=element_blank(),legend.background=element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),text=element_text(size=30),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(color="black"))+
  annotate("text",x=0.9,y=90,label="e)",size=10)


#### Graph Temperature
fa2019_Temp<-ggplot(Air_Temp,aes(x=Week, y=Air_Temp_Mean,group=Treatment))+
  geom_point(aes(color=Treatment,shape=Treatment),size=5)+
  geom_line(aes(color=Treatment,linetype=Treatment),size=2)+
  scale_linetype_manual(values=c("solid","dashed"))+
  scale_color_manual(values = colour_palette)+
  scale_shape_manual(values=c(1,16))+
  geom_errorbar(aes(ymin=Air_Temp_Mean-Air_Temp_St_Error,ymax=Air_Temp_Mean+Air_Temp_St_Error),width=0.2)+
  #Label the x-axis "Treatment"
  #Label the x-axis "Treatment"
  xlab(element_blank())+
  #Label the y-axis "Species Richness"
  ylab(element_blank())+
  expand_limits(y=c(0,40))+
  geom_vline(xintercept = 7.9, colour = "blue",size=1)+
  geom_vline(xintercept = 9.1, colour = "blue",size=1)+
  theme_bw()+
  theme(legend.position = "none",legend.title=element_blank(),legend.background=element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),text=element_text(size=30),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(color="black"))+
  annotate("text",x=0.9,y=40,label="b)",size=10)

#### Graph Soil Moisture
fa2019_Soil_Moisture<-ggplot(Soil_Moisture,aes(x=Week, y=Soil_Moisture_Mean,group=Treatment))+
  geom_point(aes(color=Treatment,shape=Treatment),size=5)+
  geom_line(aes(color=Treatment,linetype=Treatment),size=2)+
  scale_linetype_manual(values=c("solid","dashed"))+
  scale_color_manual(values = colour_palette)+
  scale_shape_manual(values=c(1,16))+
  geom_errorbar(aes(ymin=Soil_Moisture_Mean-Soil_Moisture_St_Error,ymax=Soil_Moisture_Mean+Soil_Moisture_St_Error),width=0.2)+
  #Label the x-axis "Treatment"
  #Label the x-axis "Treatment"
  xlab("Week")+
  #Label the y-axis "Species Richness"
  ylab(element_blank())+
  expand_limits(y=c(0,40))+
  geom_vline(xintercept = 7.9, colour = "blue",size=1)+
  geom_vline(xintercept = 9.1, colour = "blue",size=1)+
  theme_bw()+
  theme(legend.position = "none",legend.title=element_blank(),legend.background=element_blank(),text=element_text(size=30),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(color="black"))+
  annotate("text",x=0.9,y=39,label="h)",size=10)


#### Graph Light
ggplot(Light,aes(x=Week, y=Light_Mean,group=Treatment))+
  geom_point(aes(color=Treatment,shape=Treatment),size=5)+
  geom_line(aes(color=Treatment,linetype=Treatment),size=2)+
  scale_linetype_manual(values=c("solid","dashed"))+
  scale_color_manual(values = colour_palette)+
  scale_shape_manual(values=c(1,16))+
  geom_errorbar(aes(ymin=Light_Mean-Light_St_Error,ymax=Light_Mean+Light_St_Error),width=0.2)+
  #Label the x-axis "Treatment"
  #Label the x-axis "Treatment"
  xlab("Week")+
  #Label the y-axis "Species Richness"
  ylab("Light")+
  expand_limits(y=c(0,15000))+
  geom_vline(xintercept = 7.9, colour = "blue",size=1)+
  geom_vline(xintercept = 9.1, colour = "blue",size=1)+
  theme_bw()+
  theme(legend.position = "none",legend.title=element_blank(),legend.background=element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),text=element_text(size=30),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


#Look at Histogram to determine if data is normally distrubuted
hist(Temp_Humidity$Section_Humidity,main="Fa 2019 Humidity") 
#testing for normality - H0 is that the data is normally distributed, therefore significant P-value means data is not normal OR the sample size is too small
shapiro.test(Temp_Humidity$Section_Humidity)

hist(Temp_Humidity$Section_Air_Temp,main="Fa 2019 Air Temp")
shapiro.test(Temp_Humidity$Section_Air_Temp)

hist(Temp_Humidity$Section_Soil_Moisture,main="Fa 2019 Soil Moisture") 
shapiro.test(Temp_Humidity$Section_Soil_Moisture)

#### T Tests

#Week 1
Temp_Humidity_TTests_Week1<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==1) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week1$Section_Humidity~Temp_Humidity_TTests_Week1$Treatment)
wilcox.test(Temp_Humidity_TTests_Week1$Section_Air_Temp~Temp_Humidity_TTests_Week1$Treatment)
wilcox.test(Temp_Humidity_TTests_Week1$Section_Soil_Moisture~Temp_Humidity_TTests_Week1$Treatment)
wilcox.test(Temp_Humidity_TTests_Week1$Section_Light~Temp_Humidity_TTests_Week1$Treatment)



#Week 2
Temp_Humidity_TTests_Week2<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==2) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week2$Section_Humidity~Temp_Humidity_TTests_Week2$Treatment)
wilcox.test(Temp_Humidity_TTests_Week2$Section_Air_Temp~Temp_Humidity_TTests_Week2$Treatment)
wilcox.test(Temp_Humidity_TTests_Week2$Section_Soil_Moisture~Temp_Humidity_TTests_Week2$Treatment)
wilcox.test(Temp_Humidity_TTests_Week2$Section_Light~Temp_Humidity_TTests_Week2$Treatment)

#Week 3
Temp_Humidity_TTests_Week3<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==3) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week3$Section_Humidity~Temp_Humidity_TTests_Week3$Treatment)
wilcox.test(Temp_Humidity_TTests_Week3$Section_Air_Temp~Temp_Humidity_TTests_Week3$Treatment)
wilcox.test(Temp_Humidity_TTests_Week3$Section_Soil_Moisture~Temp_Humidity_TTests_Week3$Treatment)
wilcox.test(Temp_Humidity_TTests_Week3$Section_Light~Temp_Humidity_TTests_Week3$Treatment)

#Week 4
Temp_Humidity_TTests_Week4<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==4) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week4$Section_Humidity~Temp_Humidity_TTests_Week4$Treatment)
wilcox.test(Temp_Humidity_TTests_Week4$Section_Air_Temp~Temp_Humidity_TTests_Week4$Treatment)
wilcox.test(Temp_Humidity_TTests_Week4$Section_Soil_Moisture~Temp_Humidity_TTests_Week4$Treatment)
wilcox.test(Temp_Humidity_TTests_Week4$Section_Light~Temp_Humidity_TTests_Week4$Treatment)


#Week 5
Temp_Humidity_TTests_Week5<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==5) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week5$Section_Humidity~Temp_Humidity_TTests_Week5$Treatment)
wilcox.test(Temp_Humidity_TTests_Week5$Section_Air_Temp~Temp_Humidity_TTests_Week5$Treatment)
wilcox.test(Temp_Humidity_TTests_Week5$Section_Soil_Moisture~Temp_Humidity_TTests_Week5$Treatment)
wilcox.test(Temp_Humidity_TTests_Week5$Section_Light~Temp_Humidity_TTests_Week5$Treatment)

#Week 6
Temp_Humidity_TTests_Week6<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==6) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week6$Section_Humidity~Temp_Humidity_TTests_Week6$Treatment)
wilcox.test(Temp_Humidity_TTests_Week6$Section_Air_Temp~Temp_Humidity_TTests_Week6$Treatment)
wilcox.test(Temp_Humidity_TTests_Week6$Section_Soil_Moisture~Temp_Humidity_TTests_Week6$Treatment)
wilcox.test(Temp_Humidity_TTests_Week6$Section_Light~Temp_Humidity_TTests_Week6$Treatment)


#Week 7
Temp_Humidity_TTests_Week7<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==7) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week7$Section_Humidity~Temp_Humidity_TTests_Week7$Treatment)
wilcox.test(Temp_Humidity_TTests_Week7$Section_Air_Temp~Temp_Humidity_TTests_Week7$Treatment)
wilcox.test(Temp_Humidity_TTests_Week7$Section_Soil_Moisture~Temp_Humidity_TTests_Week7$Treatment)
wilcox.test(Temp_Humidity_TTests_Week7$Section_Light~Temp_Humidity_TTests_Week7$Treatment)

# Week 8
Temp_Humidity_TTests_Week8<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==8) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week8$Section_Humidity~Temp_Humidity_TTests_Week8$Treatment)
wilcox.test(Temp_Humidity_TTests_Week8$Section_Air_Temp~Temp_Humidity_TTests_Week8$Treatment)
wilcox.test(Temp_Humidity_TTests_Week8$Section_Soil_Moisture~Temp_Humidity_TTests_Week8$Treatment)
wilcox.test(Temp_Humidity_TTests_Week8$Section_Light~Temp_Humidity_TTests_Week8$Treatment)


#### Week 9
Temp_Humidity_TTests_Week9<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==9) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week9$Section_Humidity~Temp_Humidity_TTests_Week9$Treatment)
wilcox.test(Temp_Humidity_TTests_Week9$Section_Air_Temp~Temp_Humidity_TTests_Week9$Treatment)
wilcox.test(Temp_Humidity_TTests_Week9$Section_Soil_Moisture~Temp_Humidity_TTests_Week9$Treatment)
wilcox.test(Temp_Humidity_TTests_Week9$Section_Light~Temp_Humidity_TTests_Week9$Treatment)


#### Week 10
Temp_Humidity_TTests_Week10<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==10) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week10$Section_Humidity~Temp_Humidity_TTests_Week10$Treatment)
wilcox.test(Temp_Humidity_TTests_Week10$Section_Air_Temp~Temp_Humidity_TTests_Week10$Treatment)
wilcox.test(Temp_Humidity_TTests_Week10$Section_Soil_Moisture~Temp_Humidity_TTests_Week10$Treatment)
wilcox.test(Temp_Humidity_TTests_Week10$Section_Light~Temp_Humidity_TTests_Week10$Treatment)


#Make a list of all p-values and use p.adjust to correct all p-values at once 

#Make lists of all p-values for each abiotic measurement - ordered by week
pval_H<-c(1.0,0.7951,0.8974,1.0,0.9795,1.0,0.9456,1.482e-12,0.003518,1.0)
pval_AT<-c(1.0,0.7851,1.0,1.0,0.82,0.9836,0.7976,1.986e-05,0.2962,1.0)
pval_SM<-c(1.0,0.7867,0.7827,0.5027,0.9285,0.9572,0.7591,2.161e-11,2.122e-06,0.434)
pval_L<-c(1.0,0.881,0.8424,0.9059,0.7388,0.8299,0.7825,0.7883,0.5397,0.954)

#adjust p-value using padjust for each abiotic measurement
p.adjust(pval_H, method = "BH", n=10)
p.adjust(pval_AT, method = "BH", n=10)
p.adjust(pval_SM, method = "BH", n=10)
p.adjust(pval_L, method = "BH", n=10)


################################################################################################
###  SPRING 2020 - EXP 3 ###
####### Read-In Datasets ####### 
data<-read_csv("LLP_Data_Spring2020.csv")
LLPBiomass<-read.csv("Biomass_LLP_Spring2020.csv", header=T)%>%
  group_by(PlantID)%>%
  mutate(TotalBiomass=sum(Alive,Dead))%>%
  mutate(LeafSenescence=(Dead/TotalBiomass))
LLPLA<-read.csv("LA_LLP_Spring2020.csv", header=T)
#####################################################################################
### Average the measurements taken on same plant per week ###
LLPgraphs<-subset(data,c(Week=="10"))
########################################################################
# Number of individuals for each Treatment - growth measures #
LLPgraphs%>% group_by(Treatment) %>% tally()
# Number of individuals for each Treatment - ANPP #
LLPBiomass%>% group_by(Treatment) %>% tally()
# Number of individuals for each Treatment - LA #
LLPLA%>% group_by(Treatment) %>% tally()
######################################################################

### Tests of Normality for Biotic Variables ###
#MLL# - NOT NORMAL
ggdensity(LLPgraphs$Max_Leaf_Length)
ggqqplot(LLPgraphs$Max_Leaf_Length)
shapiro.test(LLPgraphs$Max_Leaf_Length)
########################################################
#MPH# - NORMAL
ggdensity(LLPgraphs$Max_Height)
ggqqplot(LLPgraphs$Max_Height)
shapiro.test(LLPgraphs$Max_Height)
########################################################
#Stems# - NORMAL
ggdensity(LLPgraphs$Stem_Number)
ggqqplot(LLPgraphs$Stem_Number)
shapiro.test(LLPgraphs$Stem_Number)
##########################################################
#ANPP# - NORMAL
ggdensity(LLPBiomass$TotalBiomass)
ggqqplot(LLPBiomass$TotalBiomass)
shapiro.test(LLPBiomass$TotalBiomass)
########################################################
#LA# - NOT NORMAL
ggdensity(LLPLA$LA)
ggqqplot(LLPLA$LA)
shapiro.test(LLPLA$LA)
############################################################

# STATS TESTS W/ ADJUSTMENTS FOR BIOTIC VARIABLES #
wilcox.test(LLPgraphs$Max_Leaf_Length~LLPgraphs$Treatment)
wilcox.test(LLPgraphs$Max_Height~LLPgraphs$Treatment)
wilcox.test(LLPgraphs$Stem_Number~LLPgraphs$Treatment)
wilcox.test(LLPBiomass$TotalBiomass~LLPBiomass$Treatment)
wilcox.test(LLPLA$LA~LLPLA$Treatment)

pval6<-c(0.1958,0.005271,0.5379,0.006233,2.066e-06)
p.adjust(pval6,method="BH",n=5)

###################################################################################
Totalbox<-ggplot(LLPBiomass,aes(x=Treatment,y=TotalBiomass,fill=Treatment))+
  geom_boxplot()+xlab("Treatment")+ylab("Aboveground Production (g/plug)")+
  theme(panel.background = element_rect(fill="transparent",colour=NA), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill="transparent",colour=NA))+
  scale_y_continuous(limits=c(0,2.0))+
  scale_fill_manual("Treatment",values=pal)
Totalbox
ggsave("Sp20Prod.png",Totalbox, bg = "transparent")

MLLbox<-ggplot(LLPgraphs,aes(x=Treatment,y=Max_Leaf_Length,fill=Treatment))+
  geom_boxplot()+xlab("Treatment")+ylab("Maximum Leaf Length (mm)")+
  theme(panel.background = element_rect(fill="transparent",colour=NA), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill="transparent",colour=NA))+
  scale_y_continuous(limits=c(0,600))+
  scale_fill_manual("Treatment",values=pal)
MLLbox
ggsave("Sp20MLL.png",MLLbox, bg = "transparent")

LAbox<-ggplot(LLPLA,aes(x=Treatment,y=LA,fill=Treatment))+
  geom_boxplot()+xlab("Treatment")+ylab("Leaf Area (cm^2)")+
  theme(panel.background = element_rect(fill="transparent",colour=NA), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill="transparent",colour=NA))+
  scale_y_continuous(limits=c(0,6))+
  scale_fill_manual("Treatment",values=pal)
LAbox
ggsave("Sp20LA.png",LAbox, bg = "transparent")

Stembox<-ggplot(LLPgraphs,aes(x=Treatment,y=Stem_Number,fill=Treatment))+
  geom_boxplot()+xlab("Treatment")+ylab("Stem Number")+
  theme(panel.background = element_rect(fill="transparent",colour=NA), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill="transparent",colour=NA))+
  scale_y_continuous(limits=c(0,60))+
  scale_fill_manual("Treatment",values=pal)
Stembox
ggsave("Sp20Stems.png",Stembox, bg = "transparent")

MPHbox<-ggplot(LLPgraphs,aes(x=Treatment,y=Max_Height,fill=Treatment))+
  geom_boxplot()+xlab("Treatment")+ylab("Maximum Plant Height")+
  theme(panel.background = element_rect(fill="transparent",colour=NA), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill="transparent",colour=NA))+
  scale_y_continuous(limits=c(0,400))+
  scale_fill_manual("Treatment",values=pal)
MPHbox
ggsave("Sp20MPH.png",MPHbox, bg = "transparent")
##############################################################
########
colour_palette<-park_palette("Saguaro",2)

Temp_Humidity<-data%>%
  select(Week,Humidity,Air_Temp,Treatment,Light,Soil_Moisture,Section,Group) %>% 
  #Filter out temperature irregularities
  filter(Air_Temp!=8) %>% 
  #change air temps that are seemingly in Fahrenheit not Celsius
  mutate(Air_Temp=ifelse(Air_Temp==68,20,ifelse(Air_Temp==53,11.7,Air_Temp))) %>% 
  group_by(Treatment,Week,Section,Group) %>%
  summarize(Section_Humidity=mean(Humidity),Section_Air_Temp=mean(Air_Temp),Section_Soil_Moisture=mean(Soil_Moisture),Section_Light=mean(Light)) 

Humidity<-Temp_Humidity %>% 
  select(Treatment,Week,Section,Group,Section_Humidity) %>% 
  group_by(Treatment,Week) %>% 
  summarize(Humidity_std=sd(Section_Humidity),Humidity_Mean=mean(Section_Humidity),Humidity_n=length(Section_Humidity))%>%
  mutate(Humidity_St_Error=Humidity_std/sqrt(Humidity_n)) %>% 
  ungroup()

Air_Temp<-Temp_Humidity %>% 
  select(Treatment,Week,Section,Group,Section_Air_Temp) %>% 
  group_by(Treatment,Week)%>% 
  summarize(Air_Temp_std=sd(Section_Air_Temp),Air_Temp_Mean=mean(Section_Air_Temp),Air_Temp_n=length(Section_Air_Temp))%>%
  mutate(Air_Temp_St_Error=Air_Temp_std/sqrt(Air_Temp_n))

Soil_Moisture<-Temp_Humidity %>% 
  select(Treatment,Week,Section,Group,Section_Soil_Moisture) %>% 
  group_by(Treatment,Week) %>% 
  summarize(Soil_Moisture_std=sd(Section_Soil_Moisture),Soil_Moisture_Mean=mean(Section_Soil_Moisture),Soil_Moisture_n=length(Section_Soil_Moisture))%>%
  mutate(Soil_Moisture_St_Error=Soil_Moisture_std/sqrt(Soil_Moisture_n))

Light<-Temp_Humidity %>% 
  select(Treatment,Week,Section,Group,Section_Light) %>% 
  group_by(Treatment,Week) %>% 
  ungroup() %>% 
  group_by(Week,Treatment) %>% 
  summarize(Light_std=sd(Section_Light),Light_Mean=mean(Section_Light),Light_n=length(Section_Light))%>%
  mutate(Light_St_Error=Light_std/sqrt(Light_n))

#### Graph Humidity
sp2020_Humidity<-ggplot(Humidity,aes(x=Week, y=Humidity_Mean,group=Treatment))+
  geom_point(aes(color=Treatment,shape=Treatment),size=5)+
  geom_line(aes(color=Treatment,linetype=Treatment),size=2)+
  scale_linetype_manual(values=c("solid","dashed"))+
  scale_color_manual(values = colour_palette)+
  scale_shape_manual(values=c(1,16))+
  geom_errorbar(aes(ymin=Humidity_Mean-Humidity_St_Error,ymax=Humidity_Mean+Humidity_St_Error),width=0.2)+
  #Label the x-axis "Treatment"
  xlab(element_blank())+
  #Label the y-axis "Species Richness"
  ylab(element_blank())+
  expand_limits(y=c(0,90))+
  #put the lines slightly on either side of week 9 and 10 so we can still see the points
  geom_vline(xintercept = 8.9, colour = "blue",size=1)+
  geom_vline(xintercept = 10.1, colour = "blue",size=1)+
  theme_bw()+
  theme(legend.position = "none",legend.title=element_blank(),legend.background=element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),text=element_text(size=30),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(color="black"))+
  annotate("text",x=0.65,y=90,label="f)",size=10)

#### Graph Temperature
sp2020_Temp<-ggplot(Air_Temp,aes(x=Week, y=Air_Temp_Mean,group=Treatment))+
  geom_point(aes(color=Treatment,shape=Treatment),size=5)+
  geom_line(aes(color=Treatment,linetype=Treatment),size=2)+
  scale_linetype_manual(values=c("solid","dashed"))+
  scale_color_manual(values = colour_palette)+
  scale_shape_manual(values=c(1,16))+
  geom_errorbar(aes(ymin=Air_Temp_Mean-Air_Temp_St_Error,ymax=Air_Temp_Mean+Air_Temp_St_Error),width=0.2)+
  #Label the x-axis "Treatment"
  xlab(element_blank())+
  #Label the y-axis "Species Richness"
  ylab(element_blank())+
  expand_limits(y=c(0,40))+
  geom_vline(xintercept = 8.9, colour = "blue",size=1)+
  geom_vline(xintercept = 10.1, colour = "blue",size=1)+
  theme_bw()+
  theme(legend.position = c(0.25,0.25),legend.title=element_blank(),legend.background=element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),text=element_text(size=30),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(color="black"))+
  annotate("text",x=0.65,y=40,label="c)",size=10)

#### Graph Soil Moisture
sp2020_Soil_Moisture<-ggplot(Soil_Moisture,aes(x=Week, y=Soil_Moisture_Mean,group=Treatment))+
  geom_point(aes(color=Treatment,shape=Treatment),size=5)+
  geom_line(aes(color=Treatment,linetype=Treatment),size=2)+
  scale_linetype_manual(values=c("solid","dashed"))+
  scale_color_manual(values = colour_palette)+
  scale_shape_manual(values=c(1,16))+
  geom_errorbar(aes(ymin=Soil_Moisture_Mean-Soil_Moisture_St_Error,ymax=Soil_Moisture_Mean+Soil_Moisture_St_Error),width=0.2)+
  scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10"))+
  #Label the x-axis "Treatment"
  xlab("Week")+
  #Label the y-axis "Species Richness"
  ylab(element_blank())+
  expand_limits(y=c(0,40))+
  expand_limits(y=c(0,40))+
  geom_vline(xintercept = 8.9, colour = "blue",size=1)+
  geom_vline(xintercept = 10.1, colour = "blue",size=1)+
  theme_bw()+
  theme(legend.position = "none",legend.title=element_blank(),legend.background=element_blank(),text=element_text(size=30),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text = element_text(color="black"))+
  annotate("text",x=0.7,y=39,label="i)",size=10)

#### Graph Light
ggplot(Light,aes(x=Week, y=Light_Mean,group=Treatment))+
  geom_point(aes(color=Treatment,shape=Treatment),size=5)+
  geom_line(aes(color=Treatment,linetype=Treatment),size=2)+
  scale_linetype_manual(values=c("solid","dashed"))+
  scale_color_manual(values = colour_palette)+
  scale_shape_manual(values=c(1,16))+
  geom_errorbar(aes(ymin=Light_Mean-Light_St_Error,ymax=Light_Mean+Light_St_Error),width=0.2)+
  #Label the x-axis "Treatment"
  xlab("Week")+
  #Label the y-axis "Species Richness"
  ylab(element_blank())+
  expand_limits(y=c(0,40))+
  geom_vline(xintercept = 8.9, colour = "blue",size=1)+
  geom_vline(xintercept = 10.1, colour = "blue",size=1)+
  theme_bw()+
  theme(legend.position = "none",legend.title=element_blank(),legend.background=element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),text=element_text(size=30),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#Look at Histogram to determine if data is normally distrubuted
hist(Temp_Humidity$Section_Humidity,main="Sp 2020 Humidity") 
#testing for normality - H0 is that the data is normally distributed, therefore significant P-value means data is not normal OR the sample size is too small
shapiro.test(Temp_Humidity$Section_Humidity)

hist(Temp_Humidity$Section_Air_Temp,main="Sp 2020 Air Temp")
shapiro.test(Temp_Humidity$Section_Air_Temp)

hist(Temp_Humidity$Section_Soil_Moisture,main="Sp 2020 Soil Moisture") 
shapiro.test(Temp_Humidity$Section_Soil_Moisture)

# STATS TESTS W/ ADJUSTMENTS FOR ABIOTIC VARIABLES #
#Week 1
Temp_Humidity_TTests_Week1<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==1) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week1$Section_Humidity~Temp_Humidity_TTests_Week1$Treatment)
wilcox.test(Temp_Humidity_TTests_Week1$Section_Air_Temp~Temp_Humidity_TTests_Week1$Treatment)
wilcox.test(Temp_Humidity_TTests_Week1$Section_Soil_Moisture~Temp_Humidity_TTests_Week1$Treatment)
wilcox.test(Temp_Humidity_TTests_Week1$Section_Light~Temp_Humidity_TTests_Week1$Treatment)

#Week 2
Temp_Humidity_TTests_Week2<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==2) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week2$Section_Humidity~Temp_Humidity_TTests_Week2$Treatment)
wilcox.test(Temp_Humidity_TTests_Week2$Section_Air_Temp~Temp_Humidity_TTests_Week2$Treatment)
wilcox.test(Temp_Humidity_TTests_Week2$Section_Soil_Moisture~Temp_Humidity_TTests_Week2$Treatment)
wilcox.test(Temp_Humidity_TTests_Week2$Section_Light~Temp_Humidity_TTests_Week2$Treatment)

#Week 3
Temp_Humidity_TTests_Week3<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==3) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week3$Section_Humidity~Temp_Humidity_TTests_Week3$Treatment)
wilcox.test(Temp_Humidity_TTests_Week3$Section_Air_Temp~Temp_Humidity_TTests_Week3$Treatment)
wilcox.test(Temp_Humidity_TTests_Week3$Section_Soil_Moisture~Temp_Humidity_TTests_Week3$Treatment)
wilcox.test(Temp_Humidity_TTests_Week3$Section_Light~Temp_Humidity_TTests_Week3$Treatment)

#Week 4
Temp_Humidity_TTests_Week4<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==4) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week4$Section_Humidity~Temp_Humidity_TTests_Week4$Treatment)
wilcox.test(Temp_Humidity_TTests_Week4$Section_Air_Temp~Temp_Humidity_TTests_Week4$Treatment)
wilcox.test(Temp_Humidity_TTests_Week4$Section_Soil_Moisture~Temp_Humidity_TTests_Week4$Treatment)
wilcox.test(Temp_Humidity_TTests_Week4$Section_Light~Temp_Humidity_TTests_Week4$Treatment)

#Week 5
Temp_Humidity_TTests_Week5<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==5) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week5$Section_Humidity~Temp_Humidity_TTests_Week5$Treatment)
wilcox.test(Temp_Humidity_TTests_Week5$Section_Air_Temp~Temp_Humidity_TTests_Week5$Treatment)
wilcox.test(Temp_Humidity_TTests_Week5$Section_Soil_Moisture~Temp_Humidity_TTests_Week5$Treatment)
wilcox.test(Temp_Humidity_TTests_Week5$Section_Light~Temp_Humidity_TTests_Week5$Treatment)

#Week 6
Temp_Humidity_TTests_Week6<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==6) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week6$Section_Humidity~Temp_Humidity_TTests_Week6$Treatment)
wilcox.test(Temp_Humidity_TTests_Week6$Section_Air_Temp~Temp_Humidity_TTests_Week6$Treatment)
wilcox.test(Temp_Humidity_TTests_Week6$Section_Soil_Moisture~Temp_Humidity_TTests_Week6$Treatment)
wilcox.test(Temp_Humidity_TTests_Week6$Section_Light~Temp_Humidity_TTests_Week6$Treatment)

#Week 7
Temp_Humidity_TTests_Week7<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==7) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week7$Section_Humidity~Temp_Humidity_TTests_Week7$Treatment)
wilcox.test(Temp_Humidity_TTests_Week7$Section_Air_Temp~Temp_Humidity_TTests_Week7$Treatment)
wilcox.test(Temp_Humidity_TTests_Week7$Section_Soil_Moisture~Temp_Humidity_TTests_Week7$Treatment)
wilcox.test(Temp_Humidity_TTests_Week7$Section_Light~Temp_Humidity_TTests_Week7$Treatment)

# Week 8 - NO DATA
#Temp_Humidity_TTests_Week8<-Temp_Humidity%>%
#select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
#group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
#filter(Week==8) %>% 
#ungroup()

#wilcox.test(Temp_Humidity_TTests_Week8$Section_Humidity~Temp_Humidity_TTests_Week8$Treatment)
#wilcox.test(Temp_Humidity_TTests_Week8$Section_Air_Temp~Temp_Humidity_TTests_Week8$Treatment)
#wilcox.test(Temp_Humidity_TTests_Week8$Section_Soil_Moisture~Temp_Humidity_TTests_Week8$Treatment)
#wilcox.test(Temp_Humidity_TTests_Week8$Section_Light~Temp_Humidity_TTests_Week8$Treatment)

#### Week 9
Temp_Humidity_TTests_Week9<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==9) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week9$Section_Humidity~Temp_Humidity_TTests_Week9$Treatment)
wilcox.test(Temp_Humidity_TTests_Week9$Section_Air_Temp~Temp_Humidity_TTests_Week9$Treatment)
wilcox.test(Temp_Humidity_TTests_Week9$Section_Soil_Moisture~Temp_Humidity_TTests_Week9$Treatment)
wilcox.test(Temp_Humidity_TTests_Week9$Section_Light~Temp_Humidity_TTests_Week9$Treatment)

#### Week 10
Temp_Humidity_TTests_Week10<-Temp_Humidity%>%
  select(Week,Section_Humidity,Section_Air_Temp,Treatment,Section_Soil_Moisture,Section_Light) %>% 
  group_by(Week,Treatment,Section_Humidity,Section_Air_Temp,Section_Soil_Moisture,Section_Light)%>%
  filter(Week==10) %>% 
  ungroup()

wilcox.test(Temp_Humidity_TTests_Week10$Section_Humidity~Temp_Humidity_TTests_Week10$Treatment)
wilcox.test(Temp_Humidity_TTests_Week10$Section_Air_Temp~Temp_Humidity_TTests_Week10$Treatment)
wilcox.test(Temp_Humidity_TTests_Week10$Section_Soil_Moisture~Temp_Humidity_TTests_Week10$Treatment)
wilcox.test(Temp_Humidity_TTests_Week10$Section_Light~Temp_Humidity_TTests_Week10$Treatment)

#Make lists of all p-values for each abiotic measurement - ordered by week
pval_H_Sp20<-c(1,1,0.9853,0.9657,1,0.9273,0.9898,2.199e-11,2.579e-09)
pval_AT_Sp20<-c(1,1,1,1,1,0.8967,1,2.54e-11,2.078e-09)
pval_SM_Sp20<-c(0.8128,0.8825,0.7129,0.5117,0.559,0.6934,0.5638,1.245e-10,3049e-09)
pval_L_Sp20<-c(0.9536,0.9327,0.883,0.7129,0.8437,0.8795,0.9693,6.944e-06,0.0007479)

#adjust p-value using padjust for each abiotic measurement
p.adjust(pval_H_Sp20, method = "BH", n=9)
p.adjust(pval_AT_Sp20, method = "BH", n=9)
p.adjust(pval_SM_Sp20, method = "BH", n=9)
p.adjust(pval_L_Sp20, method = "BH", n=9)
