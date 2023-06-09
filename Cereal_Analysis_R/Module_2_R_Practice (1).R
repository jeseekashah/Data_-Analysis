#Module 2 - Week 2 

install.packages("ggplot2",dependancies=TRUE)
library(ggplot2)

install.packages("tidyverse")
library(tidyverse)

install.packages("tidyr")
library(tidyr)

install.packages("reshape2")
library(reshape2)

install.packages("dplyr")
library(dplyr)

install.packages("lubridate")
library(lubridate)

install.packages("corrplot")
library(corrplot)  

library(plotly) 

library("gridExtra") 
install.packages("gridExtra") 
install.packages(" ggpubr")
library()

# Reading the file 
#Step 2: Loading the file and cleaning the dataset from Local 
cereal_df<-read.csv("C://Users//Jessica Shah//Desktop//ALY6010//R_Practice_Week2//cereal.csv",header = T,na.string="")


#Step 3 
#Preprocessing the data 
cereal_df$mfr_actual_name <-recode(cereal_df$mfr,
                                   'A' = 'American Home Food Products',
                                   'G' = 'General Mills', 
                                   'K' = 'Kelloggs',
                                   'N' = 'Nabisco',
                                   'P' = 'Post',
                                   'Q' = 'Quaker Oats',
                                   'R' = 'Ralston Purina') 

#Checking the new column that got added 
View(cereal_df) 

#Step 4 : Checking the details  
# Descriptive statistics of Calories  
summary(cereal_df)
meanCalories <- round(mean(cereal_df$calories),2)
stdCalories <- round(sd(cereal_df$calories),4)
rangeCalories <- round(max(cereal_df$calories) - min(cereal_df$calories),2)
medianCalories <- median(cereal_df$calories)  

CaloriesStats <- rbind(meanCalories, stdCalories, rangeCalories, medianCalories)
View(CaloriesStats)
# Descriptive statistics of Protein 
meanProtein <- round(mean(cereal_df$protein),2)
stdProtein <- round(sd(cereal_df$protein),4)
rangeProtein <- round(max(cereal_df$protein) - min(cereal_df$protein),2)
medianProtein <- median(cereal_df$protein)   

ProteinStats <- rbind(meanProtein, stdProtein, rangeProtein, medianProtein)
View(ProteinStats) 

# Descriptive statistics of Fat 
meanFat <- round(mean(cereal_df$fat),2)
stdFat<- round(sd(cereal_df$fat),4)
rangeFat <- round(max(cereal_df$fat) - min(cereal_df$fat),2)
medianFat <- median(cereal_df$fat)   

FatStats <- rbind(meanFat, stdFat, rangeFat, medianFat)
View(FatStats)
 
# Descriptive statistics of Sugar
meanSugar <- round(mean(cereal_df$sugars),2)
stdSuagar<- round(sd(cereal_df$sugars),4)
rangeSugar <- round(max(cereal_df$sugars) - min(cereal_df$sugars),2)
medianSugar <- median(cereal_df$sugars)   

SugarStats <- rbind(meanSugar, stdSuagar, rangeSugar, medianSugar)
View(SugarStats) 

# Descriptive statistics of Fiber
meanFiber <- round(mean(cereal_df$fiber),2)
stdFiber<- round(sd(cereal_df$fiber),4)
rangeFiber <- round(max(cereal_df$fiber) - min(cereal_df$fiber),2)
medianFiber <- median(cereal_df$fiber)   

FiberStats <- rbind(meanFiber, stdFiber, rangeFiber, medianFiber)
View(FiberStats) 

# Descriptive statistics of Carbo
meanCarbo <- round(mean(cereal_df$carbo),2)
stdCarbo<- round(sd(cereal_df$carbo),4)
rangeCarbo<- round(max(cereal_df$carbo) - min(cereal_df$carbo),2)
medianCarbo <- median(cereal_df$carbo)   

CarboStats <- rbind(meanCarbo, stdCarbo, rangeCarbo, medianCarbo)
View(CarboStats)
#Now combining everything together  
combinedData <- cbind(FatStats, CaloriesStats, SugarStats, FiberStats,ProteinStats,CarboStats)

tableData <- combinedData
rownames(tableData) <- c('Mean', 'Standard Deviation', 'Range', 'Median')
colnames(tableData) <- c('Fat', 'Calories', 'Sugar', 'Fiber','Protein','Carbo')
class(tableData)
View(tableData) 
plot()



# Step 5: Plotting the Graph bases on Cereal Components 
#Plot 1: Protein Fiber in each components of the cereals 
boxplot(cereal_df$fat,cereal_df$protein,cereal_df$fiber,cereal_df$carbo,cereal_df$sugar,col = rainbow(ncol(cereal_df)),names=c("fat","protein","fiber","carbo","sugar"), main="Cereals composition",cex=0.5,cex.axis=0.6) 
par(cex.axis=1.5)   

#Step 6 :Showing Count of cereal product manufactured by manufacturers
product_manufactored<-cereal_df %>% group_by(mfr_actual_name) %>% summarise(count=n()) %>% arrange(mfr_actual_name)
View(product_manufactored)   
#name1<-count(cereal_df$name)  

#par(mfrow=c(1,2))
p1<-ggplot(product_manufactored, aes(x=mfr_actual_name, y=count)) + geom_bar(stat="identity",fill="pink")+
   labs(title=" Count of products manufactured") + theme_minimal() + geom_text(aes(label=count), vjust=-0.3, size=3.5)+ ylab("Count of Products") +xlab("Manufacturers")+ coord_flip() 
#Step : Counting per the cold and hot coffee options  
#Plot : Check how many cereals are eating with hot and cold beverage  
Count_Cereal_type=cereal_df %>% group_by(type) %>% summarise(count=n()) %>% arrange(type)
View(Count_Cereal_type) 

p2<-ggplot(data = cereal_df, aes(x = mfr, fill = type)) +
  geom_bar()+stat_count(geom = "text", 
                        aes(label = stat(count)),
                        position=position_fill(vjust=1), colour="black")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+xlab("Manufactures")+labs(title="Manufactures producing cold and hot cereals") + coord_flip()

#Used to arrange the graphs 
#Plot 2 
grid.arrange(p1,p2,nrow=2)
             
#ggarrange(p1, p2,  
#         labels = c("A", "B"),
#         ncol = 2, nrow = 2)


#Step 7 :  Plotting the graph based  Mean Calories and 
cereal_calories_count=cereal_df %>% group_by(mfr_actual_name) %>% dplyr::summarise(count=n(),mean(calories)) %>% arrange(mean(cereal_df$calories))
View(cereal_calories_count)   
#Plot 3: Done
p<-ggplot(cereal_calories_count,aes(`mean(calories)`,count))+  geom_jitter(aes(colour = cereal_calories_count$mfr_actual_name ))+labs(title="Mean calories in cereal product based on manufactures")
#p+geom_point(position="jitter") 
p 

# Step 8: Plotting Rating Vs Mfr 
#Plot 4 : 
ggplot(cereal_df, aes(x=rating, y=mfr_actual_name, color=rating)) + 
  geom_boxplot(notch = TRUE,)+
  geom_jitter(position=position_dodge(0.2)) + labs(title=" Rating Vs. Manufactures")



#Step 10: Plotting sum of rating based calories and Sugar contains in cereals
#Plot 5
p3<-ggplot(cereal_df, aes(calories, rating, color = sugars))+
  geom_point(size = 2)+
  geom_smooth(method = 'lm', formula = y ~ x)+
  scale_color_gradientn(colors = rainbow(n = 5))+labs(title="Rating Vs.calories and sugars in cereals")

p3
#Step 11 : Plotting based on the items made by the manufacture 
# Plot 6 : Plotting graph based on Sugar and Manufacturer 

p4<-ggplot(cereal_df, aes(sugars, mfr_actual_name, color = sugars))+
  geom_point(size = 2)+
  geom_smooth(method = 'lm', formula = y ~ x)+
  scale_color_gradientn(colors = rainbow(n = 5)) +labs(title="Sugars contain w.r.t manufacturer")
p4 

View(cereal_df)

Quaker_Oats<-cereal_df %>% filter(mfr_actual_name=='Quaker Oats') 
View(Quaker_Oats)

#Step 12: 3D graph plotting   Quaker_ Oats 
#Plot 7 

par(mfrow = c(1,3))
plot(Quaker_Oats$protein, Quaker_Oats$fiber, main = "Scatter chart") 
abline(lm(fiber~protein, data = Quacker_Oats), col = "blue")
plot(Quaker_Oats$protein, Quaker_Oats$carbo, main = "jitter chart") 
abline(lm(carbo~protein, data = Quacker_Oats), col = "blue") 
boxplot(Quaker_Oats$protein, main="Protein in Quaker Oats")
#stripchart(Quaker_Oats$protein, vertical = TRUE, data = ddf, 
         #  method = "jitter", add = TRUE, pch = 20, col = 'blue')
#stripchart(Quaker_Oats$protein, "overplot", add = TRUE, at = 0.7) 


#Diaplaying 3 table format values protein calories and sugar
# Descriptive statistics of Sugar
meanQOats <- round(mean(Quacker_Oats$protein),2)
stdSQOats<- round(sd(Quacker_Oats$protein),4)
rangeQOats <- round(max(Quacker_Oats$protein) - min(Quacker_Oats$protein),2)
medianQOats <- median(Quacker_Oats$protein)   

QOATAStats <- rbind(meanQOats, stdSQOats, rangeQOats, medianQOats)
View(QOATAStats)


meanQOats2 <- round(mean(Quacker_Oats$carbo),2)
stdSQOats2<- round(sd(Quacker_Oats$carbo),4)
rangeQOats2 <- round(max(Quacker_Oats$carbo) - min(Quacker_Oats$fiber),2)
medianQOats2 <- median(Quacker_Oats$carbo)   

QOATAStats1 <- rbind(meanQOats2, stdSQOats2, rangeQOats2, medianQOats2)
View(QOATAStats1)


