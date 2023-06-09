# Including the libraries  
install.packages("ggplot2",dependancies=TRUE)
library(ggplot2)

install.packages("tidyverse")
library(tidyverse)

install.packages("tidyr")
library(tidyr)

install.packages("dplyr")
library(dplyr)


install.packages("infer")
library(infer)

library(plotly) 
library("gridExtra") 
install.packages("gridExtra") 
install.packages(" ggpubr")
library(ggpubr) 

print("Module 3 Week 3  Aly 6010")
data("PlantGrowth")
#View(GrowthinPlant)  
View(PlantGrowth)


# checking the structure
str(PlantGrowth)
  

# Descriptive statistics of Plant Growth 
summary(PlantGrowth)
meanGrowth <- round(mean(PlantGrowth$weight),2)
stdGrowth<- round(sd(PlantGrowth$weight),4)
rangeGrowth <- round(max(PlantGrowth$weight) - min(PlantGrowth$weight),2)
medianGrowth<- median(PlantGrowth$weight)  
meanGrowth
GrowthinPlantStats <- cbind(meanGrowth, stdGrowth, rangeGrowth, medianGrowth)
View(GrowthinPlantStats)

#Plot Scater plot of all weights
graph_plantweight<-ggplot(PlantGrowth,aes(group,weight,color=group))+geom_point(aes(shape=group) )+
  labs(x="Plant Group",y="Plant weight",title="Plant Group and Weight",subtitle = "Plantgrowth",captions="inbuild dataset PlantGrowth") + geom_smooth()
graph_plantweight

#Plot showing Mean in Box plot 

MeanPlantGrowth<-tapply(PlantGrowth$weight, PlantGrowth$group, mean)   
View(MeanPlantGrowth)
MeanPlantGrowth
boxplot(PlantGrowth$weight ~ PlantGrowth$group,col=c("red","green","blue"), main="Boxplot- Weight Vs. Group",cex=0.5,cex.axis=0.6) 
par(cex.axis=1.5) 

# ploting Histogram  
par(mfrow = c(2,1)) 
hist(PlantGrowth$weight,15)
boxplot(PlantGrowth$weight,xlab ="Weight (g)",main="Boxplot of Weights",horizontal=TRUE)



# Calculating One - tailed and Two tailed test 
# Mean of the population is 5.07 and n=30 
#Calculation
shapiro.test(PlantGrowth$weight) 

#checking the distribution 
p2 <- ggplot(data=PlantGrowth, aes(x=weight, group=group, fill=group)) +
  geom_density(adjust=1.5, alpha=.4) 
p2 

# we get the p values and it is greater then 0.05 which showas it is normally distributed 
#p=0.8915  
 
# Now considerin the mu= 5.1
t0<-PlantGrowth %>% filter(group =="ctrl")
t1<- PlantGrowth %>% filter(group =="trt1") 
t2<- PlantGrowth %>% filter(group == "trt2") 
t1
mean(t0$weight)# n=10
mean(t1$weight) 
mean(t2$weight) 
class(t1)
#One sample test mu=5.1
result1<-t.test(PlantGrowth$weight) 
result1 
result1$p.value 
result1$estimate 
result1$conf.int
result2 <- t.test(PlantGrowth$weight,mu=5.1,alternative = "greater")
result2
result2$p.value
result2 <- t.test(PlantGrowth$weight,mu=5.1,alternative = "less") 
result2 
result2$p.value
# Two sAMPLE TEST   
t.test(t0$weight,t1$weight,alternative = "two.sided")
#Type I error mention 
t.test(t0$weight,t2$weight,alternative = "two.sided")
#Reject 



#Hypothesis testing 

install.packages("infer")
library(infer) 

ctrl <- subset(PlantGrowth, group =="ctrl")
trt1 <- subset(PlantGrowth, group == "trt1")
trt2 <- subset(PlantGrowth, group == "trt2")
hypData <- rbind(ctrl, trt1) 
hypData2<-rbind(ctrl,trt2)

plantgrowth <- hypData %>%
  specify(formula = weight ~ group) %>%  
  hypothesize(null = "independence") %>%
  calculate(stat = "diff in means") %>%
  generate(reps = 1000, type = "permute") 
plantgrowth

diff_means_plants <- hypData %>%
  specify(formula = weight ~ group) %>%
  calculate(stat = "diff in means", order = c("trt1", "ctrl"))
diff_means_plants

visualize(plantgrowth) + 
  shade_p_value(obs_stat = diff_means_plants, direction = "both")

plantgrowth %>%
  get_p_value(obs_stat = diff_means_plants, direction = "both")

### CTRL & TRT2 

diff_means_plants2 <- hypData2 %>%
  specify(formula = weight ~ group) %>%
  calculate(stat = "diff in means", order = c("trt2", "ctrl"))
diff_means_plants2

plantgrowth %>%
  get_p_value(obs_stat = diff_means_plants2, direction = "both") 

visualize(plantgrowth) + 
  shade_p_value(obs_stat = diff_means_plants2, direction = "both")
