
# Import dataset
salesdata<-read.csv("C:/Users/Hp/Desktop/data science/R/Datasets/Sales data.csv")

# EDA
summary(salesdata$Sales)
names(salesdata)

# converting Target variable from categorical to numeric 
salesdata$High=as.factor(ifelse(salesdata$Sales<=8,0,1))

# deleting column sales
salesdata$Sales<-NULL
data<-salesdata


#Check missing value
sapply(data,function(x)sum(is.na(x)))

# to identify outlier 
par(mfrow = c(1,1))         
boxplot(data$CompPrice)     #no
boxplot(data$Income)        #no
boxplot(data$Advertising)   #no
boxplot(data$Population)    #no
boxplot(data$Price)         #no
boxplot(data$Age)           #no
boxplot(data$Education)     #no


##Data Partition
set.seed(54)
library(caret)
split <- createDataPartition(y=data$High, p=0.7, list=FALSE)
train <- data[split,]
test <- data[-split,]

# Model building

Model =tree(High~.,data=train,split = "gini")
Model

# Plotting Model 
plot(Model)
text(Model,pretty =0)

# Prediction on Train data set & Checking Performance
output_train=predict(Model,train,type="class") #class will give ans in 0,1
table(output_train ,train$High)
confusionMatrix(output_train,train$High)

# Prediction on Test data set  & Checking Performance
output_test=predict (Model,test,type ="class")
table(output_test ,test$High)
library(caret)
confusionMatrix(output_test,test$High)


# Pruning the Model 
cv.data =cv.tree(Model ,FUN=prune.misclass )
names(cv.data )
plot(cv.data$size ,cv.data$dev ,type="b")
Prune.Model =prune.misclass (Model ,best =8)

# Plotting Model
plot(Prune.Model)
text(Prune.Model,pretty =0)


# Prediction on Train data set  & Checking Performance
output_Prune_train=predict(Prune.Model,train,type="class")
table(output_Prune_train ,train$High)
confusionMatrix(output_Prune_train,train$High)

# Prediction on Test data set & Checking Performance
output_Prune_test=predict(Prune.Model,test,type="class")
table(output_Prune_test ,test$High)
confusionMatrix(output_Prune_test,test$High)
