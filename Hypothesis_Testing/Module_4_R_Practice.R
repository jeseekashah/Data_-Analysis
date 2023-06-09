install.packages("MASS")
library(MASS) 
library("FSA")
library("FSAdata")
library("magrittr")
library("dplyr")
library("tidyr")
library("ggplot2")
install.packages("plyr")
install.packages("tidyverse")
library("plyr")
library("tidyverse")  
install.packages("RColorBrewer")
install.packages("lessR")
library(lessR) 
library(RColorBrewer)   

#part 1 
#Independent Test case 


Catsdatabase1<-data("cats")
class(Catsdatabase1)
Catsdatabase1<-as.data.frame(cats)
class(Catsdatabase1)
Catsdatabase1
#display(Catsdatabase1) 

catsFemale<-subset(Catsdatabase1, Sex =="F")
catsFemale  
catsMale<-subset(Catsdatabase1,Sex=="M")
catsMale
dim(catsFemale) 
dim(catsMale) 

#Nomality Plot 0
#checking the distribution 
p2 <- ggplot(data=Catsdatabase1, aes(x=Catsdatabase1$Bwt, group=Sex, fill=Sex)) +
  geom_density(adjust=1.5, alpha=.4) 
p2

# Descriptive statistics of Cats Body weight
summary(Catsdatabase1)
meanBwt <- round(mean(Catsdatabase1$Bwt),2)
stdBwt<- round(sd(Catsdatabase1$weight),4)

meanBwt
BwtCatsStats <- cbind(meanBwt, stdBwt)
BwtCatsStats
#Descriptive statistics of CatsFemale & Cats Male 
MeanCatFemale <- round(mean(catsFemale$Bwt),2)
MeanCatMale <- round(mean(catsMale$Bwt),2) 
MeanCatFemaleHwt <- round(mean(catsFemale$Hwt),2)
MeanCatMaleHwt<- round(mean(catsMale$Hwt),2)
Combined_Mean<-cbind(MeanCatFemale,MeanCatMale,MeanCatFemaleHwt,MeanCatMaleHwt)
Combined_Mean   
sd(catsFemale$Bwt)
sd(catsMale$Bwt)


#plot 1 : Distribution of weight based on the Gender
#Plotitng the Cats weight based on the Sex

ggplot(data =Catsdatabase1 , mapping = aes(x = Catsdatabase1$Bwt)) + geom_bar(aes(fill = Catsdatabase1$Sex)) +
  labs(y = 'Count', x = 'Body Weight of Cats', fill = 'Sex')  


#Plot 2
#pivot_longer(c('Bwt', 'Hwt'), names_to = "statistics", values_to = 'count') %>%
#Scatter Plot for Showing Height and Weight relationship of cats 
Catsdatabase1 %>% ggplot(aes(Bwt, Hwt))+
  geom_point(stat = 'identity',aes(color = factor(Sex)))  + theme(axis.text.x = element_text(angle = 40, hjust = 1))+ stat_smooth(method = "lm",col = "#C42126",se=FALSE)+labs(
    title = "Plotting relation between height and weight of cats") 

#Plot 3                                                                                                                               
#Plotting Median Body Weight based on the weight 
par(mfrow = c(1,2)) 
boxplot(catsFemale$Bwt,xlab ="Weight (g)",main="Weights of Female cats",notch=TRUE,
        col=(c("gold","darkgreen"))) 

points(mean(catsFemale$Bwt),col="red")

boxplot(catsMale$Bwt,xlab ="Weight (g)",main="Weights of Male cats",notch=TRUE,
        col=(c("lightblue"))) 
points(mean(catsMale$Bwt),col="red")

#Plot 4
#Plotting the boxplot Heights based on the sex  
colors = c("red", "yellow", "green", "violet", "orange","blue", "pink", "cyan") 
par(mfrow = c(2,1)) 
hist(catsFemale$Hwt,15, main="Histogram showing Female cat heights",col=colors) 
hist(catsMale$Hwt,15,main="Histogram showing Male cat heights",col=colors)
#boxplot(Catsdatabase1$Bwt,xlab ="Weight (g)",main="Boxplot of Weights",horizontal=TRUE) 


#Performing the Analysis Two sample Test
#p<0.05 
#Rejecting the null hypothesis
t.test(catsFemale$Bwt, catsMale$Bwt)  

dim(Catsdatabase1)

#Paired test or dependent 
#Part 2: Evaluation of meditation effect on sleep quality

before_meditation<- c(4.6, 7.8, 9.1, 5.6, 6.9, 8.5, 5.3, 7.1, 3.2, 4.4)
after_meditation <- c(6.6, 7.7, 9.0, 6.2, 7.8, 8.3, 5.9, 6.5, 5.8, 4.9)
Meditationeffectsleep<-cbind(before_meditation,after_meditation)
class(Meditationeffectsleep) 
Meditationeffectsleep<-as.data.frame(Medicationeffectsleep)
Meditationeffectsleep
#t.test(sleepQualityExperiment$Before.Wrkshp, sleepQualityExperiment$fter.Wrkshp,paired=T, conf.level =0.95,alternative="")

# Taking Mean and Standard deviation 
mean(before_meditation) 
sd(before_meditation)
mean(after_meditation)
sd(after_meditation)  

#Checking using Boxplot 
boxplot(before_meditation,after_meditation,main="Boxplot showing the meditation effect on sleep before and after",
         names=c("before","after"),col=(c("purple","pink")))

#Sleep pattern is better after the medication 
#pivot_longer(c('Bwt', 'Hwt'), names_to = "statistics", values_to = 'count') %>%
#Scatter Plot for Showing Height and Weight relationship of cats 
Meditationeffectsleep %>% ggplot(aes(before_meditation, after_meditation))+
  geom_point(stat = 'identity')  + theme(axis.text.x = element_text(angle = 40, hjust = 1))+ stat_smooth(method = "lm",col = "#C42126",se=FALSE)+labs(
    title = "Effect of meditation before and after on sleep quality") 
# it is not equal on both sides of the line as the sleep has increased after the meditation as the number of points lie more towards up than below 


# As the data is dependent here i have considered paired testing 
#Case 1 : Confidence interval is 0.95
t.test(Meditationeffectsleep$before_meditation,Meditationeffectsleep$after_meditation,paired=T,alternative = "less")      
       
#Case 2: Change in the confidence interval to 0.90 paired testing
t.test(Meditationeffectsleep$before_meditation, Meditationeffectsleep$after_meditation, 
              paired=T, conf.level = 0.90,alternative = "less")                                                                                                       



                                                             
 








