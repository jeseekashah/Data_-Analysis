#Jeseeka Shah 
# Module 2  assignment : Chi-sqaure - ANOVA test 



# Question 1 :- category :-Blood type , comparing distribution of the blood type based on the large or general hospital 

bloodprob_distribution = c(0.20 , 0.28 , 0.36 , 0.16) 
general_hosiptal_obs = c(12,8,24,6)
blood_distribution <- data.frame(Values_Observed = general_hosiptal_obs, 
                        prob_distribution = bloodprob_distribution)
blood_distribution 

#Using Chi-Square 
test_result<-chisq.test(x = general_hosiptal_obs, p = bloodprob_distribution) 
test_result

test_result$statistic
test_result$parameter
test_result$p.value


#Question 2:- On time performance of the airlines 
Ontime_prob= c(0.71 , 0.08 , 0.09 , 0.12)
Diff_count <- c(125, 10, 25, 40) 


test_result2<-chisq.test(x = Diff_count, p = Ontime_prob) 
test_result2

# Question 3: Ethinity factor decides the admission of movie 
Ethinicity_2013<-c(724,335,174,107) 
Ethinicity_2014<-c(370,292,152,140)
 
movie_data<- data.frame( e_2013 = Ethinicity_2013,
                         e_2014 = Ethinicity_2014) 
 
movie_data=matrix(c(Ethinicity_2013,Ethinicity_2014),nrow=2,byrow=TRUE)
rownames(movie_data) <- c("2013","2014")
colnames(movie_data)=c("Caucasian","Hispanic","African American","Other")

movie_data

test_result3<-chisq.test(movie_data)
test_result3

#Question 4: Women in Military 


army_1<-c(10791,62491) 
navy_2<-c(7816,42750) 
mcorps_3<-c(932,9525) 
air_4<-c(11819,54344)
rows=4
military_women=matrix(c(army_1,navy_2,mcorps_3,air_4),nrow=rows,byrow=TRUE)
rownames(military_women)=c("Army","Navy","Marine Corps","Air Force")
colnames(military_women)=c("Officers","Enlisted")
military_women

alpha<-0.05
test_result4<-chisq.test(military_women)
test_result4



#Question 5: Presence of Sodium 

Condiments_1<-data.frame('sodium'=c(270,130,230,180,80,70,200),'food'=rep('condiments',7),stringsAsfactors=FALSE)
Cereals_2<-data.frame('sodium'=c(260,220,290,290,200,320,140),'food'=rep('cereals',7),stringsAsfactors=FALSE)
Dessert_3<-data.frame('sodium'=c(100,180,250,250,300,360,300,160),'food'=rep('desserts',8),stringsAsfactors=FALSE)

sodium_contains<-rbind(Condiments_1,Cereals_2,Dessert_3) 
sodium_contains
sodium_contains$food<-as.factor(sodium_contains$food)

anova<-aov(sodium~food,data=sodium_contains)
summary(anova)
a.summary<-summary(anova)

df.numerator<-a.summary[[1]][1, "Df"]
df.numerator

df.denominator<-a.summary[[1]][2, "Df"]
df.denominator

F.value<-a.summary[[1]][1, "F value"]
F.value

p.value<-a.summary[[1]][1, "Pr(>F)"]
p.value

ifelse(p.value > alpha, "Fail to reject null hypothesis", "Reject the null hypothesis")  

TukeyHSD(anova)
#Question 6:  


alpha<-0.01

cereal_data_1<-data.frame('mean'=c(578,320,264,249,237),'food'=rep('cereal',5),stringsAsfactors=FALSE)
chocolatecandy_data_2<-data.frame('mean'=c(311,106,109,125,173),'food'=rep('chocolatecandy',5),stringsAsfactors=FALSE)
coffee_data_3<-data.frame('mean'=c(261,185,302,689),'food'=rep('coffee',4),stringsAsfactors=FALSE)

means_df<-rbind(cereal_data_1,chocolatecandy_data_2,coffee_data_3)
means_df$food<-as.factor(means_df$food)

anova<-aov(mean~food,data=means_df)
summary(anova)
a.summary<-summary(anova)

df.numerator<-a.summary[[1]][1, "Df"]
df.numerator

df.denominator<-a.summary[[1]][2, "Df"]
df.denominator

F.value<-a.summary[[1]][1, "F value"]
F.value

p.value<-a.summary[[1]][1, "Pr(>F)"]
p.value

ifelse(p.value > alpha, "Fail to reject null hypothesis", "Reject the null hypothesis") 


#Question 7:   

alpha<-0.05

easternthird<-data.frame('pupil'=c(4946,5953,6202,7243,6113),'section'=rep('easthernthird',5),stringsAsfactors=FALSE)
middlethird<-data.frame('pupil'=c(6149,7451,6000,6479),'section'=rep('middlethird',4),stringsAsfactors=FALSE)
westernthird<-data.frame('pupil'=c(5282,8605,6528,6911),'section'=rep('westernthird',4),stringsAsfactors=FALSE)

pupil<-rbind(easternthird,middlethird,westernthird)
pupil$section<-as.factor(pupil$section)

anova<-aov(pupil~section,data=pupil)
summary(anova)
a.summary<-summary(anova)

df.numerator<-a.summary[[1]][1, "Df"]
df.numerator

df.denominator<-a.summary[[1]][2, "Df"]
df.denominator

F.value<-a.summary[[1]][1, "F value"]
F.value

p.value<-a.summary[[1]][1, "Pr(>F)"]
p.value

ifelse(p.value > alpha, "Fail to reject null hypothesis", "Reject the null hypothesis") 

TukeyHSD(anova)
 
#Question 8  

Food <- c(1,1,1,2,2,2,1,1,1,2,2,2)
Food <- factor(Food)

growthLight <- c(1,1,1,1,1,1,2,2,2,2,2,2)
growthLight <- factor(growthLight)

light <- c(9.2,9.4,8.9,7.1,7.2,8.5,8.5,9.2,8.9,5.5,5.8,7.6)

plantData <- data.frame(Food, growthLight, light)
plantData
 

#Decalring the alpha value 
alpha7<-0.05
# creating data frame 
light1 <- data.frame('result' = c(9.2,9.4,8.9),'foodA'=rep('light1',3)) 
light2<- data.frame('result' = c(8.5,9.2,8.9),'foodA'=rep('light2',3)) 
light3 <- data.frame('result' = c(7.1,7.2,8.5),'foodB'=rep('light1',3)) 
light4<- data.frame('result' = c(5.5,5.8,7.6),'foodB'=rep('light2',3)) 

#Combining data frames
result_ipg1 <- rbind(light1,light2)  
result_ipg1
result_ipg1$foodA<-as.factor(result_ipg1$foodA) 

result_ipg2<-rbind(light3,light4)
result_ipg2$foodB<-as.factor(result_ipg2$foodB)
result_ipg2
# ANOVA test
anova1_SupA <- aov(result~foodA, data=result_ipg1) 
anova1_SupA
anova2_SupB <- aov(result~foodB, data=result_ipg2)

# view model summary
a.summary3 <- summary(anova1)
a.summary3
a.summary4 <- summary(anova2)
a.summary4

# Degree of freedom
# k-1: between group variance - numerator
df.numerator3 <- a.summary3[[1]][1,"Df"]
df.numerator3
df.numerator4 <- a.summary4[[1]][1,"Df"]
df.numerator4

# N -k: between group variance - denominator
df.denominator3 <- a.summary3[[1]][2,"Df"]
df.denominator3
df.denominator4 <- a.summary4[[1]][2,"Df"]
df.denominator4

# extract the F test value from summary
F.value3 <- a.summary3[[1]][[1,"F value"]]
F.value3
F.value4 <- a.summary4[[1]][[1,"F value"]]
F.value4

# extract the p test value from summary
ipgpvalue1 <- a.summary3[[1]][[1,"Pr(>F)"]]
ipgpvalue1
ipgpvalue2 <- a.summary4[[1]][[1,"Pr(>F)"]]
ipgpvalue2

# Comparing p value with alpha to make decision
ifelse(ipgpvalue1 > 0.05,"Fail to reject null hypothesis","reject null hypothesis")
ifelse(ipgpvalue2 > 0.05,"Fail to reject null hypothesis","reject null hypothesis")

# Summary
# Since alpha value is smaller than p value we fail to reject the null hypothesis and conclude that
# there is no difference in mean growth with respect to plant food and light

#Own your own problem :- Baseball team 

bb_df <- read.csv("C:/Users/Jessica Shah/Desktop/Aly6015/Mod2/baseball-2.csv")
summary(bb_df)
 

#Performing the EDA  
#Graph 1
par(mfcol = c(1,2))
plot(density(bb_df$RA), main = "Density plot for RA", ylab = "Frequency")
boxplot(bb_df$RA,horizontal = T,col = terrain.colors(2), main = "Box plot for RA") 
#Graph 2 
par(mfcol = c(1,2))
plot(density(bb_df$RS), main = "Density plot for RS", ylab = "Frequency")
boxplot(bb_df$RS,col=blues9 , main = "Box plot for RS") 
#Graph 3 
par(mfcol = c(1,2))
plot(density(bb_df$OBP), main = "Density plot for OBP", ylab = "Frequency")
boxplot(bb_df$OBP, main = "Box plot for OBP") 
#Reading the library !!!! 
library(psych)
dsc <- describe(bb_df)
dsc
bb_df$Decade <- bb_df$Year - (bb_df$Year %% 10)
bb_df$Decade 

library(dplyr)
library(tidyverse) 

wins <- bb_df %>% group_by(Decade) %>% summarise(wins = sum(W)) %>% as.tibble()
wins 
class(wins)
Values_obsv <- c(13267, 17934, 18926, 17972, 24286, 7289)
Values_expt<- c(1/6,1/6,1/6,1/6,1/6,1/6)

qf(p=0.05, df1 = 5, df2 = 1, lower.tail = FALSE)

chisq.test(Values_obsv, p = Values_expt, correct = FALSE)

chisq.test(wins)


