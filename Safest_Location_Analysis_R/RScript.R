
#Packets that are used to implement the project 
library("FSA")
library("FSAdata") 
install.packages("magrittr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("plyr")
install.packages("tidyverse") 
library("magrittr")
library("dplyr")
library("tidyr")
library("ggplot2")
install.packages("plyr")
install.packages("tidyverse")
library("plyr")
#library("tidyverse")  
install.packages("RColorBrewer")
install.packages("lessR")
library(lessR) 
library(RColorBrewer) 
library("ggplot2")
install.packages("ggplot2")

#library(magrittr)
#library(dplyr)
#library(tidyr)
#library(plyr)
#library(tidyverse)  

print("WEEk 1")


#Step 2: Loading the file and cleaning the data set
Interrogation<-read.csv("C://Users//Jessica Shah//Desktop//ALY6010//Project_R_Week1//Crime_Interogation.csv",header = T,na.string="")
View(Interrogation)

#Step 3:Removing the Na in the table 
Interrogation_Clear <- na.omit(Interrogation)
View(Interrogation_Clear)  

#Step 4 : checking the summary of the data
summary(Interrogation_Clear) 
View(head(Interrogation_Clear,5)) 
View(tail(Interrogation_Clear,5))
 
class(Interrogation_Clear)
#Step 5: Filtering out based on the key situations deciding that state is MA 
Key_situations<- Interrogation_Clear %>% filter( state=='MA', stop_duration <= 10 ) 
View(Key_situations)
#Step 6 :Grouping based on basis 
Grouping_basis=Key_situations %>% group_by(basis) %>% dplyr::summarise(count=n()) %>% arrange(basis)
View(Grouping_basis)   

#Step 7: Plotting the line graph 
par(mar=c(10, 7, 1, 1))  
#par(mar=c(1,1, 1, 1))
#line_1=plot(x=Grouping_basis$basis,y=Grouping_basis$count,type="o",ylim(0,30),xlim()) 
plot<-barplot(Grouping_basis$count,xlab = "Basis",ylab ="Count",main="Bar plot showing the basis of interogation by BPD",cex.axis=0.6,cex=0.6,font=2,ylim=c(0,25),names.arg =Grouping_basis$basis)



# Step 8 :View(Interrogation_Clear$key_situations)
Filterated_based_intel= Key_situations %>% filter ( basis=="Intel") 
View(Filterated_based_intel)
# Step 9 :Now grouping intel based on the city 

Grouping_intel=Filterated_based_intel %>% group_by(city) %>% dplyr::summarise(count=n()) %>% arrange(city)
View(Grouping_intel)
class(Grouping_intel) 
 
# Step 10 :Plotting Count using Bar graph to show the city with intel based interogation 

#line_1=plot(x=Grouping_intel$city,y=Grouping_intel$count,type="o",xlim()) 

par(mar=c(1, 7, 1, 1)) 
pie(Grouping_intel$count,labels = Grouping_intel$count,values = "%", axes=F,space=0.15,ylim = c(0,50),main = "Field interaction conducted based on intel w.r.t cities in MA")
legend("topright", legend = c("Boston", "Dorchester", "Roxbury"),
       fill =  c("white", "lightblue", "mistyrose"),lwd = 1)

# Step 11 :Plotting based on the Vehicle_type that was  
Grouping_Vehicle_type=Filterated_based_intel %>% group_by(vehicle_type) %>% dplyr::summarise(count=n()) %>% arrange(vehicle_type)
#Grouping_Vehicle_type=Filterated_based_intel %>% group_by(vehicle_type) %>% summarise(count=n()) %>% arrange(vehicle_type)
View(Grouping_Vehicle_type) 

#Step 12: Filtering based on the probable cause 
#View(Interrogation_Clear$key_situations)
Filterated_based_probable_cause= Key_situations %>% filter ( basis=="Probable Cause")  
View(Filterated_based_probable_cause)

#Reasonable_suspicion=Key_situations %>% filter ( basis=="Reasonable Suspicion")  

X=filter(Interrogation_Clear, Interrogation_Clear$basis == "Reasonable Suspicion")
View(X)

#Step 13 :Plotting the graph 

Filterated_probable_cause=Filterated_based_probable_cause %>% group_by(city) %>% dplyr::summarise(count=n()) %>% arrange(city)
View(Filterated_probable_cause) 
#Filterated_probable_cause
 
probable_cause_city=count(Filterated_based_probable_cause$city)  
#probable_cause_city_1=count(Filterated_based_probable_cause$city)
#View(probable_cause_city_1)
probable_cause_city=probable_cause_city %>% arrange(desc(freq))
probable_cause_city   

#probable_cause_city_1=count(Filterated_probable_cause$city) 
#probable_cause_city_1=probable_cause_city_1 %>% arrange(desc(freq))
#probable_cause_city_1

class(probable_cause_city$freq)
Changing_vector<-as.numeric(probable_cause_city$freq) 
class(Changing_vector)
#y.features <- as.numeric(X[feature.inds,7]) 
View(probable_cause_city)
#Visualization code 
#par(mar=c(10, 7, 1, 1)) 
#plot(x=Changing_vector,type="o",pos=1) 
#legend("topleft",c("Dorchester","Roxbury","Boston","Hyde Park","South Boston"))
#text(locator(), labels =probable_cause_city_1$x ) 
#c("red line", "black line)")  

#legend=c("Line 1", "Line 2")   
#Step 14: Plotting thr bar plot with cities
plot<-barplot(probable_cause_city$freq,xlab = "Cities in MA",ylab = "Count", main="Bar plot showing the field interaction based on the probable cause",col = c("red", "green", "yellow", "blue","pink"),cex.axis=0.6,font=2,cex.names = 0.5,names.arg =probable_cause_city$x, las = 2)


# Step 15 : Plotting Time series to show the interrogation of Boston police in different years 
library(plyr)
Total_count_of_years<-
        count(Interrogation_Clear, "vehicle_year")  
View(Total_count_of_years)
 
ggplot(Total_count_of_years, aes(x=vehicle_year, y=freq)) +
        geom_line( color="#69b3a2") + theme(axis.text.x=element_text(angle=60, hjust=1)) +  
        xlab("Year") + ggtitle(" The Occurance of the field interaction w.r.t year")


