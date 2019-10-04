#This line of code will clear any presaved settings that might interfere with how my code runs
rm(list=ls(all=T))
#This line of code will clear any saved plots so you start nice and fresh
dev.off()

#in order to run this code, you need to set your working directory first. To do this
#you need to add in the pathway to find the location you have the excel sheet stored
#For me, I have the datasheet saved in a folder in my documents so my pathway is:
#setwd("~/Dropbox/Documents/Research/SASC/BRUV_Paper/SASC_BRUV_BaitChoice")

setwd("PUT YOUR PATHWAY HERE WITHIN THE QUOTATIONS")

###### Installing Packages ######
#If this is your first time running my code, you need to install all my extra packages
#You only need to do this the first time, you can ignore this every other time

install.packages("tidyverse")
install.packages("readxl")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("vegan")
install.packages("devtools")
install.packages("plotrix")
install.packages("reshape")
install.packages("unmarked")
install.packages("AICcmodavg")
install.packages("stats")
install.packages("MASS")
install.packages("data.table")
install.packages("lme4")
install.packages("emmeans")
install.packages("tidyr")
install.packages("car")

###### Packages and Data ######
#now you have to run all the packages so they will be active during this R session
library(tidyverse)
library(readxl)
library(ggplot2)
library(ggpubr)
library(vegan)
library(devtools)
library(plotrix)
library(reshape)
library(unmarked)
library(AICcmodavg)
library(stats)
library(MASS)
library(data.table)
library(lme4)
library(emmeans)
library(tidyr)
library(car)

#This will load the data that will be used for the entire data analysis
#Yes Aaron i am only using one data sheet
bruvdata <- read_excel("BRUV_master.xlsx", sheet = "Compiled Data", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text",  "text", "text", "text", "numeric","text", "numeric", "text", "text","numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "text", "numeric", "numeric", "text"))

###### SHANNON ######
#First i ran a shannon index on each site to take into account abundance and diversity
ShannonData <- bruvdata
ShannonData[c( 3, 4, 5, 6, 7, 9, 11, 12, 13,14, 16, 17 ,18, 19, 20,21,22,23,24,25,26,27,28,29,30 )] <- list(NULL)
ShannonData$group=paste(ShannonData$Site, ShannonData$Date)
ShanLength <- length(ShannonData$Site)
as.factor(ShannonData$group)
ShannonData$Shan <- 0

for(i in 1:ShanLength){
  
  xx <- subset(ShannonData, group==ShannonData$group[i])
  cc <- diversity(xx$MaxN, index = "shannon", MARGIN = 1, base = exp(1))
  ShannonData$Shan [i] <- cc
  
}

ShanVector <- factor(ShannonData$group)
aa<-levels(ShanVector)
bb <- length(aa)

ShanSumm <- data.frame(matrix(NA, nrow = bb, ncol = 3))
colnames(ShanSumm) <- c("GroupID", "ShanIndex", "Bait")
ShanSumm$GroupID <- aa
as.numeric(ShannonData$Shan)
as.factor(ShannonData$Bait)

for(i in 1:bb){
  
  xx<-subset(ShannonData, group==ShanSumm$GroupID[i])
  ShanSumm$ShanIndex[i] <- xx[1,7]
  ShanSumm$Bait[i] <- xx[1,3]
  
}

as.data.frame(ShanSumm)
ShanSumm$ShanIndex <- unlist(ShanSumm$ShanIndex)
ShanSumm$Bait <-unlist(ShanSumm$Bait)

qqnorm(ShanSumm$ShanIndex)
qqline(ShanSumm$ShanIndex)
#if the data is not normal: doesnt represent a fairly straight line, contact me, we will need 
#to switch up the test

shanttest<-t.test(ShanSumm$ShanIndex~ShanSumm$Bait) 
shanttest
#t = 0.93914, df = 11.807, p-value = 0.3665
#mean in group Sardines mean in group Squid_pike 
#1.432893                 1.089881 
#This test was not significant for my data, how is if for yours?
#it is significant if the p-value is <0.05

Sar<-subset(ShanSumm, Bait=="Sardines")
SqP<-subset(ShanSumm, Bait=="Squid_pike")

SarSEM<- sd(Sar$ShanIndex)/sqrt(length(Sar$ShanIndex))
SqPSEM<- sd(SqP$ShanIndex)/sqrt(length(SqP$ShanIndex))

ShanPlotDF <- data.frame(matrix(NA, nrow = 2, ncol = 4))
colnames(ShanPlotDF) <- c("Mean", "SEM", "poslim", "neglim")
rownames(ShanPlotDF) <- c("Sardines", "Squid and Pike")
ShanPlotDF$Mean <- shanttest$estimate
ShanPlotDF$SEM <- c(SarSEM, SqPSEM)
ShanPlotDF$poslim <- (ShanPlotDF$Mean + ShanPlotDF$SEM)
ShanPlotDF$neglim <- (ShanPlotDF$Mean - ShanPlotDF$SEM)
ShanPlotDF$Bait <- c("Sardines", "Squid and Pike")

ShanPlot <- ggplot(ShanPlotDF, aes(x = Bait, y = Mean)) +geom_point()+
  labs(x="Bait type", y="Mean Shannon Diversity Index", title="", colour="", size=3) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_errorbar(aes(ymax=poslim,ymin=neglim),position=position_dodge(0.9), width=.1, data=ShanPlotDF)
ShanPlot


###### AIC ######
#AIC MODEL SELECTION NOT TO BE USED, ONLY FOR REFERENCE
#We can maybe run later but we need a llloooootttt more data first
#Abundance <- taxdata
#Abundance$group=paste(Abundance$Site, Abundance$Date)

#AOV1 <- aov(Abundance$MaxN_total ~ Abundance$Bait)
#AOV2 <- aov(Abundance$MaxN_total ~ Abundance$Bait*Abundance$Depth)
#AOV3 <- aov(Abundance$MaxN_total ~ Abundance$Bait*Abundance$Water_temp)
#AOV4 <- aov(Abundance$MaxN_total ~ Abundance$Bait*Abundance$Site)
#AOV5 <- aov(Abundance$MaxN_total ~ Abundance$Bait*Abundance$Vis)
#AOV6 <- aov(Abundance$MaxN_total ~ Abundance$Bait*Abundance$Habitat)
#AOV7 <- aov(Abundance$MaxN_total ~ Abundance$Depth*Abundance$Water_temp)
#AOV8 <- aov(Abundance$MaxN_total ~ Abundance$Depth*Abundance$Water_temp*Abundance$Site)
#AOV9 <- aov(Abundance$MaxN_total ~ Abundance$Depth*Abundance$Water_temp*Abundance$Site*Abundance$Bait)
#GLOBAL <- aov(Abundance$MaxN_total ~ Abundance$Depth*Abundance$Water_temp*Abundance$Site*Abundance$Bait*Abundance$Habitat*Abundance$Vis)

#Cand.modsAbundance <- list(AOV1, AOV2, AOV3, AOV4, AOV5, AOV6, AOV7, AOV8, AOV9, GLOBAL)
#ModnamesAbundance <- c("1", "2", "3", "4","5", "6","7", "8","9", "Global")
#AIC_tableAbundance <- aictab(cand.set = Cand.modsAbundance, modnames = ModnamesAbundance, second.ord = TRUE)
#AIC_tableAbundance

###### Total Abundance ######
#then i analyzed the total abundance at each site
Abundance <- bruvdata
Abundance$group=paste(Abundance$Site, Abundance$Date)
Abundance$MaxN_total<- 0
Abundance[c( 3, 4, 5, 6, 7, 12, 13,14, 16, 17 , 19, 20,21,22,23,24,25,26,27,30 )] <- list(NULL)
AbundanceLength <- length(Abundance$Site)

for(i in 1:AbundanceLength){

  xx <- subset(Abundance, group==Abundance$group[i])
  cc <- sum(xx$MaxN)
  Abundance$MaxN_total [i] <- cc
  
}

AbundVector <- factor(Abundance$group)
dd<-levels(AbundVector)
ee <- length(dd)

AbundSumm <- data.frame(matrix(NA, nrow = bb, ncol = 3))
colnames(AbundSumm) <- c("GroupID", "MaxN", "Bait")
AbundSumm$GroupID <- dd
AbundSumm$Temp <- NA
AbundSumm$Depth <- NA
AbundSumm$Vis <-NA

for(i in 1:ee){

  xx<-subset(Abundance, group==AbundSumm$GroupID[i])
  AbundSumm$MaxN[i] <- xx[1,12]
  AbundSumm$Bait[i] <- xx[1,3]
  AbundSumm$Temp[i] <- xx[1,8]
  AbundSumm$Depth[i] <- xx[1,9]
  AbundSumm$Vis[i] <- xx[1,10]
  
}

as.data.frame(AbundSumm)
AbundSumm$MaxN <- unlist(AbundSumm$MaxN)
AbundSumm$Bait <-unlist(AbundSumm$Bait)
AbundSumm$Depth <-unlist(AbundSumm$Depth)
AbundSumm$Temp <-unlist(AbundSumm$Temp)
AbundSumm$Vis <-unlist(AbundSumm$Vis)


#because data is discrete and non-negative, it most likely will not fit a normal distribution
#therefore we will check a negative binomial and a poisson distribution
mod1 <- glm(AbundSumm$MaxN~AbundSumm$Bait, poisson)
mod2 <- glm.nb(AbundSumm$MaxN~AbundSumm$Bait)

#The model with the higher value is the best fit
#my data fits the negative binomial distribution better
1-pchisq(mod1$deviance, mod1$df.residual)
#0
1-pchisq(mod2$deviance,mod2$df.residual)
#0.2179228 although this is very low and not a great fit, if this is still low with more data we may need
#to switch to a nonparametric test - contact me if it is not above 0.5

Anova(mod2, type=3)
#               LR Chisq Df Pr(>Chisq)
#AbundSumm$Bait   2.3278  1     0.1271
#no significant things

SarAbundance<-subset(AbundSumm, Bait=="Sardines")
SqPAbundance<-subset(AbundSumm, Bait=="Squid_pike")

SarAbundanceSEM<- sd(SarAbundance$MaxN)/sqrt(length(SarAbundance$MaxN))
SqPAbundanceSEM<- sd(SqPAbundance$MaxN)/sqrt(length(SqPAbundance$MaxN))

AbundancePlotDF <- data.frame(matrix(NA, nrow = 2, ncol = 4))
colnames(AbundancePlotDF) <- c("Mean", "SEM", "poslim", "neglim")
rownames(AbundancePlotDF) <- c("Sardines", "Squid and Pike")
AbundancePlotDF$Mean <- abundancettest$estimate
AbundancePlotDF$SEM <- c(SarAbundanceSEM, SqPAbundanceSEM)
AbundancePlotDF$poslim <- (AbundancePlotDF$Mean + AbundancePlotDF$SEM)
AbundancePlotDF$neglim <- (AbundancePlotDF$Mean - AbundancePlotDF$SEM)
AbundancePlotDF$Bait <- c("Sardines", "Squid and Pike")

AbundancePlot <- ggplot(AbundancePlotDF, aes(x = Bait, y = Mean)) +geom_point()+
  labs(x="Bait type", y="Mean total abundance (# of organisms)", title="", colour="", size=3) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_errorbar(aes(ymax=poslim,ymin=neglim),position=position_dodge(0.9), width=.1, data=AbundancePlotDF)
AbundancePlot

###### CLASS ######
#then i split the data into classes and looked at how abundant each class was
#these were the classes our data contained, if you have more, you need to add these to the data
#I will show you how to add them
Actinopterygii <- c("Ariidae","Blenniidae","Cheilodactylidae", "Clinidae", "Mugilidae", "Sparidae")
Malacostraca <- c("Cymothoidae", "Palinuridae", "Plagusiidae")
Chondrichthyes<- c("Dasyatidae", "Scyliorhinidae", "Triakidae")
Cephalopoda <- c("Loliginidae", "Octopodidae")
Myxini <- c("Myxinidae")
Mammalia <- c("Otariidae")

ClassAbund <- bruvdata

CLASSES <- factor(ClassAbund$Family_name)
levels(CLASSES)
#[1] "Ariidae"          "Blenniidae"       "Cheilodactylidae" "Clinidae"         "Cymothoidae"      "Dasyatidae"      
#[7] "Loliginidae"      "Mugilidae"        "Myxinidae"        "Octopodidae"      "Otariidae"        "Palinuridae"     
#[13] "Plagusiidae"      "Scyliorhinidae"   "Sparidae"         "Triakidae"   
#When you run levels(CLASSES), if you get more than what i have listed above, you will need to add them to the data
#first figure out what class each family is in, if it is a class i dont have listed: Contact me
#if it is in one of the classes i already have below, you will add:
#|ClassAbund$Family_name[i] == "NAME OF FAMILY"
#to the part that says HERE


ClassAbund$group=paste(ClassAbund$Site, ClassAbund$Date)
ClassAbund[c(1,2, 3, 4, 5, 6, 7,9,11, 12, 13,14, 16, 17 ,18, 19, 20,21,22,23,24,25,26,27,28,29,30 )] <- list(NULL)
ClassAbundLength <- length(ClassAbund$group)
ClassAbund$Class <- NA

#because people don't know how to spell:
for (i in 1:ClassAbundLength){
  if(ClassAbund$Family_name[i] == "Palinurdae") { 
    ClassAbund$Family_name[i] <- "Palinuridae"
  }
}



for (i in 1:ClassAbundLength){
  if(ClassAbund$Family_name[i] == "Ariidae"|ClassAbund$Family_name[i] == "Blenniidae"|ClassAbund$Family_name[i] == "Cheilodactylidae"|ClassAbund$Family_name[i] ==  "Clinidae"|ClassAbund$Family_name[i] ==  "Mugilidae"|ClassAbund$Family_name[i] ==  "Sparidae" #HERE
     ) {
    ClassAbund$Class[i] <- "Actinopterygii"
  }
}
for (i in 1:ClassAbundLength){
  if(ClassAbund$Family_name[i] =="Cymothoidae"|ClassAbund$Family_name[i] == "Palinuridae"|ClassAbund$Family_name[i] == "Plagusiidae" #HERE
  ) {
    ClassAbund$Class[i] <- "Malacostraca"
  }
}
for (i in 1:ClassAbundLength){
  if(ClassAbund$Family_name[i] =="Dasyatidae"|ClassAbund$Family_name[i] == "Scyliorhinidae"|ClassAbund$Family_name[i] == "Triakidae" #HERE
  ) {
    ClassAbund$Class[i] <- "Chondrichthyes"
  }
}
for (i in 1:ClassAbundLength){
  if(ClassAbund$Family_name[i] == "Loliginidae"|ClassAbund$Family_name[i] ==  "Octopodidae" #HERE
  ) {
    ClassAbund$Class[i] <- "Cephalopoda"
  }
}
for (i in 1:ClassAbundLength){
  if(ClassAbund$Family_name[i] == "Myxinidae" #HERE
  ) {
    ClassAbund$Class[i] <- "Myxini"
  }
}
for (i in 1:ClassAbundLength){
  if(ClassAbund$Family_name[i] == "Otariidae" #HERE
  ) {
    ClassAbund$Class[i] <- "Mammalia"
  }
}

is.na(ClassAbund$Class)
#checking to make sure everything filled correctly, if there is a TRUE then there is an error in the code -> contact me

ClassVector <- factor(ClassAbund$Class)
ClassLevels <- levels(ClassVector)
ClassLength <- length(ClassLevels)
GroupVector <- factor(ClassAbund$group)
GroupLevels <- levels(GroupVector)
GroupLength <- length(GroupLevels)

ClassSumm <- data.frame(matrix(NA, nrow = (ClassLength), ncol = 3))
colnames(ClassSumm) <- c("ID","Class", "Bait")
ClassSumm$Class <- ClassLevels

#make sure everything has been changed to a numeric system
as.factor(ClassAbund$Bait)
#Make a folder within your current working directory to put some txt files in
#change the working directory location to where it is on your computer
#setwd("~/Dropbox/Documents/Research/SASC/BRUV Paper/ClassData")

setwd("PATHWAY FOR A FOLDER INSIDE MAIN WORKING DIRECTORY HERE")

for(i in 1:GroupLength){

  groupname <- GroupLevels[i]
  as.factor(groupname)
  
  zz <- subset(ClassAbund, group == groupname)
  
  ClassSumm$ID <- groupname
  
  BAIT <- rep(zz$Bait[1], times = 6)
  ClassSumm$Bait <- BAIT
  
  xx<-aggregate(zz$MaxN, by=list(zz$Class), FUN=sum)
  colnames(xx) <- c("Class", "Abundance")
  lengthxx <- length(xx$Class)
  
  yy <- merge(xx,ClassSumm,by=c("Class"), all = TRUE)
  
  yy[is.na(yy)] <- 0
  
  write.table(yy,groupname,sep="\t",row.names=TRUE)

}

filelist <- list.files() 
CLASSESMASTER = NULL 

for (i in 1:length(filelist)){
  xx <- as.data.frame(read.table(filelist[i]))
  CLASSESMASTER <- rbind(CLASSESMASTER, xx)}

#becasuse data is discrete and non-negative, it most likely will not fit a normal distribution
#therefore we will check a negative binomial and a poisson

mod3 <- glm(CLASSESMASTER$Abundance ~ CLASSESMASTER$Bait*CLASSESMASTER$Class, family = poisson)
mod4 <- glm.nb(CLASSESMASTER$Abundance ~ CLASSESMASTER$Bait*CLASSESMASTER$Class)

1-pchisq(mod3$deviance,mod3$df.residual)
#0
1-pchisq(mod4$deviance,mod4$df.residual)
#0.4915173
#The model with the higher value is the best fit
#my data fits the negative binomial distribution better

Anova(mod4, type=3)
#                                       LR Chisq Df Pr(>Chisq)    
#CLASSESMASTER$Bait                        0.632  1     0.4265    
#CLASSESMASTER$Class                      92.645  5     <2e-16 ***
#CLASSESMASTER$Bait:CLASSESMASTER$Class    8.816  5     0.1166    


Sar <- subset(CLASSESMASTER, Bait == "Sardines")
SarAggMean<-aggregate(Sar$Abundance, by=list(Sar$Class), FUN = mean)
SarAggSE<-aggregate(Sar$Abundance, by=list(Sar$Class), FUN = std.error)
SarAggFull <-  merge(SarAggMean,SarAggSE,by=c("Group.1"), all = TRUE)
colnames(SarAggFull) <- c("Class", "Mean", "SE")
SarAggFull$Bait <- "Sardines"

SqP <- subset(CLASSESMASTER, Bait == "Squid_pike")
SqPAggMean<-aggregate(SqP$Abundance, by=list(SqP$Class), FUN = mean)
SqPAggSE<-aggregate(SqP$Abundance, by=list(SqP$Class), FUN = std.error)
SqPAggFull <-  merge(SqPAggMean,SqPAggSE,by=c("Group.1"), all = TRUE)
colnames(SqPAggFull) <- c("Class", "Mean", "SE")
SqPAggFull$Bait <- "Squid and Pike"

ClassPlotDF <- rbind(SarAggFull,SqPAggFull)
ClassPlotDF$poslim <- (ClassPlotDF$Mean + ClassPlotDF$SE)
ClassPlotDF$neglim <- (ClassPlotDF$Mean - ClassPlotDF$SE)

ClassPlot <-ggplot(data=ClassPlotDF, aes(x=Bait, y=Mean)) + geom_point(aes(shape = Class, color=Class, size = 2))+
  labs(x="Bait type", y="Mean abundance (# of organisms)", title="", colour="Class", size=2) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
  axis.title=element_text(size=16, face="bold") , axis.line = element_line(colour = "black"), axis.text = element_text(size=12), legend.title = element_text(size=16, face="bold")) + 
  scale_shape_discrete(guide =guide_legend(label.theme = element_text(angle = 0, face = "italic")), name = "Species") +
  theme(axis.text.x = element_text(size = 14)) + scale_shape_manual(values = c(15,16,17,18,3,4))+
  guides(shape = FALSE, fill = FALSE, size = FALSE, colour = guide_legend(override.aes = list(shape = c(15,16,17,18,3,4))))+
  geom_errorbar(aes(ymax=poslim,ymin=neglim),position=position_dodge(0.9), width=.1, data=ClassPlotDF)
ClassPlot

###### Species Counts ######
#Back to the original pathway so my pathway is:
#setwd("~/Documents/Documents/Research/SASC/BRUV Paper")
setwd("PUT YOUR PATHWAY HERE WITHIN THE QUOTATIONS")

ClassAbund
SarAbund <- subset(ClassAbund, Bait=="Sardines")

Actinopterygii <- subset(SarAbund, Class == "Actinopterygii")
ActinopterygiiFact <- factor(Actinopterygii$Species_name)
levels(ActinopterygiiFact)
Cephalopoda <- subset(SarAbund, Class == "Cephalopoda")
CephalopodaFact <- factor(Cephalopoda$Species_name)
levels(CephalopodaFact)
Chondrichthyes <- subset(SarAbund, Class == "Chondrichthyes")
ChondrichthyesFact <- factor(Chondrichthyes$Species_name)
levels(ChondrichthyesFact)
Malacostraca <- subset(SarAbund, Class == "Malacostraca")
MalacostracaFact <- factor(Malacostraca$Species_name)
levels(MalacostracaFact)
Mammalia <- subset(SarAbund, Class == "Mammalia")
MammaliaFact <- factor(Mammalia$Species_name)
levels(MammaliaFact)
Myxini <- subset(SarAbund, Class == "Myxini")
MyxiniFact <- factor(Myxini$Species_name)
levels(MyxiniFact)


SqPAbund <- subset(ClassAbund, Bait=="Squid_pike")

Actinopterygii <- subset(SqPAbund, Class == "Actinopterygii")
ActinopterygiiFact <- factor(Actinopterygii$Species_name)
levels(ActinopterygiiFact)
Cephalopoda <- subset(SqPAbund, Class == "Cephalopoda")
CephalopodaFact <- factor(Cephalopoda$Species_name)
levels(CephalopodaFact)
Chondrichthyes <- subset(SqPAbund, Class == "Chondrichthyes")
ChondrichthyesFact <- factor(Chondrichthyes$Species_name)
levels(ChondrichthyesFact)
Malacostraca <- subset(SqPAbund, Class == "Malacostraca")
MalacostracaFact <- factor(Malacostraca$Species_name)
levels(MalacostracaFact)
Mammalia <- subset(SqPAbund, Class == "Mammalia")
MammaliaFact <- factor(Mammalia$Species_name)
levels(MammaliaFact)
Myxini <- subset(SqPAbund, Class == "Myxini")
MyxiniFact <- factor(Myxini$Species_name)
levels(MyxiniFact)

###### Environmental Data ######
#Correlations
CORDATA<-AbundSumm
cor.test(CORDATA$Depth,CORDATA$MaxN, method="pearson")
#t = 1.0004, df = 12, p-value = 0.3369
#0.27745 

DepthCor <- ggplot(data = CORDATA, aes(x = Depth, y = MaxN)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)+
  labs(x="Depth (m)", y="Abundance (# of organisms)", title="", colour="", size=2) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.title=element_text(size=16, face="bold") , axis.line = element_line(colour = "black"), axis.text = element_text(size=12), legend.title = element_text(size=16, face="bold")) 
DepthCor

cor.test(CORDATA$Temp,CORDATA$MaxN, method="pearson")
#t = 2.0226, df = 12, p-value = 0.06598
#0.5042283 

TempCor <- ggplot(data = CORDATA, aes(x = Vis, y = MaxN)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)+
  labs(x="Temperature (°C)", y="Abundance (# of organisms)", title="", colour="", size=2) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.title=element_text(size=16, face="bold") , axis.line = element_line(colour = "black"), axis.text = element_text(size=12), legend.title = element_text(size=16, face="bold")) 
TempCor


cor.test(CORDATA$Vis,CORDATA$MaxN, method="pearson")
#t = 1.0004, df = 12, p-value = 0.3369
#0.27745 

VisCor <- ggplot(data = CORDATA, aes(x = Vis, y = MaxN)) + 
          geom_point(color='black') +
          geom_smooth(method = "lm", se = TRUE)+
          labs(x="Visibility (m)", y="Abundance (# of organisms)", title="", colour="", size=2) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          axis.title=element_text(size=16, face="bold") , axis.line = element_line(colour = "black"), axis.text = element_text(size=12), legend.title = element_text(size=16, face="bold")) 
VisCor




