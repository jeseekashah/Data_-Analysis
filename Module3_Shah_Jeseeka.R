#Header Files
install.packages("caret")
install.packages("ISLR")
library('caret')
library(ISLR)  
library(Hmisc) 
library(corrplot)
library(RColorBrewer) 
library(pROC)

install.packages("dplyr")                            # Install dplyr
library("dplyr") 

#Reading the data 
attach(College)
#College  
#Performing the descriptive analytics  
describe(College)
summary(College)
is.null(College)

 
 ##################################
#Creating the plot to show the trend that the Accept and Enroll for the college dataset 
qplot(Accept, Enroll, data = College, color = factor(Private),
      geom=c("point", "smooth"))  
## Private and Expend ratio and target variable
College %>% ggplot(aes(Private, Expend))+
  geom_boxplot()

## Boxplot showing F.Undergrad and pass rate  for undergrad 
boxplot(College$F.Undergrad,College$P.Undergrad, 
        main = "Multiple boxplots for comparision",
        at = c(1,2),
        names = c("FailUG", "PassUG"),
        las = 2,
        col = c("orange","red"),
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
) 
# Private college vs. Public college 
College %>% ggplot(aes(Private, S.F.Ratio))+
  geom_boxplot()  


# making the correlation plot  
num_cols <- unlist(lapply(College, is.numeric))         # Identify numeric columns
num_cols  
#Saving the numeric values into a variables
data_num2 <- select_if(College, is.numeric)             # Subset numeric columns with dplyr
data_num2 

#Correaltion Matrix  
#Numerical 
M <-cor(data_num2)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu")) 



x##################################
#Splitting the data into 70-30 train-test 
#Create the data partitionn 
trainCollege1<-createDataPartition(College$Accept, p = 0.70, list = FALSE)
trainCollege1  

College_train <- College[ trainCollege1,]  
College_test <-College[-trainCollege1,]  
head(trainCollege1)
############################################### 

model1 <- glm(Private~., data = College_train, family = binomial(link = "logit"))
summary(model1)


model2 <- glm(Private~Apps + Enroll+F.Undergrad+P.Undergrad+Outstate+Books+Personal+PhD+Expend,
              data = College_train,family = binomial(link = "logit") )
summary(model2)

## get the coefficients of the model
coef(model2)

## display the regression coefficients (odds)
exp(coef(model2))
########################################
#Feature Selection Method 
#Forward Selection Method   

#Forward 
model_forward <- glm(formula = Private ~ F.Undergrad + P.Undergrad + Outstate + Grad.Rate + PhD + Apps + Accept + Expend + Enroll, data = College_train, family = binomial(link = "logit")) 
model_forward
summary(model_forward)  
colnames(College)    

## get the coefficients of the model
coef(model_forward)

## display the regression coefficients (odds)
exp(coef(model_forward))

#### tRAIN DATA #1
pre_forward.train =predict(model_forward, newdata=College_train, type="response")
pre_forward.min<- as.factor(ifelse(pre_forward.train >=0.5,"Yes","No")) 
pre_forward.min

 #Creating a Confusion Matrix
train_mat=confusionMatrix(pre_forward.min, College_train$Private, positive='Yes')
train_mat   

 
#Roc  for the forward _ Model 
ROC1 = roc(College_train$Private, pre_forward.train)
plot(ROC1, col = "blue", ylab = "Sensitivity = TP rate", xlab = 'specificity = FP rate') 

auc = auc(ROC1)
auc
################################


### tEST DATA  #1
pre_forward.test =predict(model_forward, newdata=College_test, type="response")
pre_forward.min_tst<- as.factor(ifelse(pre_forward.test >=0.5,"Yes","No")) 
pre_forward.min_tst 

#Creating a Confusion Matrix
test_mat=confusionMatrix(pre_forward.min_tst, College_test$Private, positive='Yes')
test_mat

############ 
#Roc  for the forward _ Model 
ROC1 = roc(College_test$Private, pre_forward.test)
plot(ROC1, col = "blue", ylab = "Sensitivity = TP rate", xlab = 'specificity = FP rate') 

auc = auc(ROC1)
auc


################### Model 2#############################################333
#Significant removed P.Undergrad , Grad.Rate. Apps
#Relevant for the model :- F.Undergrad , 

model_backward <- glm(formula = Private ~ F.Undergrad + PhD + Outstate + Expend , data = College, family = binomial(link = "logit")) 
model_backward 

summary(model_backward)
# Getting Regression Coefficitents 
coef(model_backward)   


#Creating a dataset to check the impact on the probabilites 
model_test_data


#Displaying the regression coefficient in r 
exp(coef(model_backward)) 


#confusion matrix train data #2

Pre_backward_1=predict(model_backward, College_train, type="response")

pre.min_train<- as.factor(ifelse(Pre_backward_1 >=0.5,"Yes","No"))

train_mat_2=confusionMatrix(pre.min_train, College_train$Private, positive='Yes')
train_mat_2

#Roc 
ROC1 = roc(College_train$Private,Pre_backward_1 )
plot(ROC1, col = "blue", ylab = "Sensitivity = TP rate", xlab = 'specificity = FP rate') 

auc = auc(ROC1)
auc
 
###Test  confusion matrix test data #2

Pre_backward_test=predict(model_backward, College_test, type="response")

pre.min_test<- as.factor(ifelse(Pre_backward_test >=0.5,"Yes","No"))

test_mat_2=confusionMatrix(pre.min_test, College_test$Private, positive='Yes')
test_mat_2

#Roc 
ROC1 = roc(College_test$Private,Pre_backward_test )
plot(ROC1, col = "blue", ylab = "Sensitivity = TP rate", xlab = 'specificity = FP rate') 

auc = auc(ROC1)
auc



#Taking Final Model with same values and less variables
# Taking another model  with four parameter 
#Significant removed P.Undergrad , Grad.Rate , removing Expend and adding prec.alumini  
#Relevant for the model :- F.Undergrad , 

model_backward_2<- glm(formula = Private ~  PhD + F.Undergrad +  Outstate +perc.alumni + Expend , data = College, family = binomial(link = "logit")) 
model_backward_2 

summary(model_backward_2)
# Getting Regression Coefficitents 
coef(model_backward)   


#Creating a dataset to check the impact on the probabilites 
model_test_data


#Displaying the regression coefficient in r 
exp(coef(model_backward_2)) 

################################ Train 
#confusion matrix train #3

Pre_backward_2=predict(model_backward_2, College_train, type="response")

pre.min<- as.factor(ifelse(Pre >=0.5,"Yes","No"))

test_mat=confusionMatrix(pre.min, College_train$Private, positive='Yes')
test_mat

#Roc 
ROC1 = roc(College_train$Private, Pre_backward_2)
plot(ROC1, col = "blue", ylab = "Sensitivity = TP rate", xlab = 'specificity = FP rate') 

auc = auc(ROC1)
auc 

##################################### Test

#confusion matrix test #3

Pre_backward_2_test=predict(model_backward_2, College_test, type="response")

pre.min_test<- as.factor(ifelse(Pre_backward_2_test >=0.5,"Yes","No"))

test_mat=confusionMatrix(pre.min_test, College_test$Private, positive='Yes')
test_mat

#Roc 
ROC1 = roc(College_test$Private, Pre_backward_2_test)
plot(ROC1, col = "blue", ylab = "Sensitivity = TP rate", xlab = 'specificity = FP rate') 

auc = auc(ROC1)
auc 

# In this model the accuracy is too lower than other model. 
