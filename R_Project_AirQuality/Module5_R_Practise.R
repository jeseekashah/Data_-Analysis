# Including the libraries  
library("FSA")
library("FSAdata")
library("magrittr")
library("dplyr")
library("tidyr")
#library("ggplot2")
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

library(tibble) #modern take on data frames. 
install.packages("dlookr")
library(dlookr) 

install.packages("gridExtra") 
install.packages(" ggpubr")
library(ggpubr)

install.packages("corrplot")
library(corrplot) 
#ggcorrplot 

install.packages("ggcorrplot")
library(ggcorrplot) 

# load package for ggcorrmat 
install.packages("ggstatsplot")
library("ggstatsplot") 
install.packages('ggcorrplot') 

data("airquality")
?airquality

airquality_na<-as.data.frame(airquality)
class(airquality_na)  
dim(airquality_na)
View(airquality) 
colSums(is.na(airquality_na))  

# Making a data set without the null values 
airquality <- na.omit(airquality_na)  

# Removing the null values  
colSums(is.na(airquality)) 
dim(airquality)

#Data Analysis 
View(airquality)

#EDA used 
#Plot 1:
#Boxplot showing the Temperature variation from May to September
boxplot(Temp~Month,
        data=airquality,
        main="Temperature variation from May to Septermber",
        xlab="Month",
        ylab="Degreein Fahrenheit",
        col="Pink",
        border="Blue"
) 

#Plot 2:
#Scatter plOt per month ozone depletion 

ggplot(airquality, aes(x = Month, y = Ozone)) +
  geom_point(aes(colour = Month)) + # Points and color   # Change legend title
  xlab("Month") +              # X-axis label
  ylab(" Ozone")  +             # Y-axis label
  theme(axis.line = element_line(colour = "black", # Changes the default theme
                                 size = 0.24))
#Plot 3:

#Considering the impact of the ozone on the temperature of New york city # there is a stong positive correlation 

ggplot(airquality, aes(x=Ozone, y=Temp)) + 
  geom_point()+
  geom_smooth(method=lm)

#Plot 4:
#Considering the impact of the ozone on the wind speed  # Strong Correlation on the negative side 

ggplot(airquality, aes(x=Ozone, y=Wind)) + 
  geom_point()+
  geom_smooth(method=lm) 

#Plot 5:
#Comapring nmore than more factors together  
ggplot(airquality, aes(x=Ozone, y=Solar.R, color=Temp)) +
  geom_point() 

#Part 1 : Correlatiion Analysis
#Cor Plot 1
# correlogram Correlation Graph 1
ggstatsplot::ggcorrmat(
  data = airquality,
  type = "parametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)  

#Cor Plot 2
#Total correlation using  corrplot  Correlation Graph 2
library(corrplot)

ap_corrmatrix <- cor(airquality)
ap_corrmatrix 
corrplot(ap_corrmatrix)

#Cor Plot 3
#Correlation Table using   Hmisc package 
install.packages("Hmisc") 
library("Hmisc")
corr_aq_tab <- rcorr(as.matrix(airquality))
corr_aq_tab


#Computing Pearson Correlation  
cor(airquality$Ozone, airquality$Solar.R) #moderate correlations
cor(airquality$Wind, airquality$Solar.R) # Low correlation
cor(airquality$Temp, airquality$Solar.R) #Moderate Correlation
cor(airquality$Temp, airquality$Month)#Moderate correlation 
cor(airquality$Ozone, airquality$Temp) # Strong Correlation   
cor(airquality$Wind, airquality$Ozone)  # Srong correlation in Negative side  

#Part B

# Making Regression Linear Plot using Pearson'S Formula   

#Using pearson method to do correlation



# Libraries required  
install.packages("GGally")
library(GGally)  

#Step 1:
ggscatter(airquality, x = "Ozone", y = "Temp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Ozone layer", ylab = "Temperature",col='blue',
          main="Pearson Correlation test") 


#Step 2
#Correlation test using cor.test function
pearson_corr_aq<-cor.test(airquality$Ozone, airquality$Temp, 
                          method = "pearson")
pearson_corr_aq

cor(airquality$Ozone,airquality$Temp) 


#Step 3:
# For making regression table the libraries required are 
library(sjPlot) 
install.packages("sjPlot")

# Regression Table  

Regression_tb <- lm(Ozone ~ Solar.R + Wind + Temp + Month + Day, data = airquality)
summary(Regression_tb)
tab_model(Regression_tb) 




