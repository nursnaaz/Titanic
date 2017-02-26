rm(list=ls())

setwd("~/Documents/kaggle/titanic")


df <- read.csv("train.csv")

str(df)

df <- df[,-1]

str(df)

apply(df,2,function(x){length(unique(x))})

df$Survived <- as.factor(df$Survived)

df$Pclass <- as.factor(df$Pclass)
str(df)

sum(is.na(df))

apply(df,2,function(x){sum(is.na(x))})


noAge <- df[is.na(df$Age),]
 
library(DMwR)

df <- knnImputation(df)

str(df)

dependent_attr <- df[,1]
independent_attr <-  df[,-1]

str(independent_attr)

library(dummies)
dummydf <- independent_attr[,c('Sex','Embarked')]

str(dummydf)

dummies1 <- dummy(dummydf[,1],verbose = TRUE)
dummies2 <- dummy(dummydf[,2],verbose = TRUE)
dummies3 <- dummy(dummydf[,3],verbose = TRUE)

independent_attr <- independent_attr[,-c(1,2,3,9)]

independent_attr <- cbind(independent_attr,dummies1,dummies2,dummies3)



df <- cbind(independent_attr,df$Survived)

df$Ticket <- as.numeric(df$Ticket)

str(df)

str(survivedDummy)

df <- cbind(df,survivedDummy)
str(df)
df <- df[,-15]
df <- df[,-6]
df <- decostand(df,method = 'range')

numrows <- seq(1,nrow(df),1)
trainrows <- sample(numrows,nrow(df)*.70)
train <- df[trainrows,]
test <- df[-trainrows,]

str(train)






nn <- nnet(yes + no  ~ ., data=train, size=9, linout = TRUE, maxit=1000) 
nn.predict <- predict(nn)


plot(nn)

nn$net.result[[1]]

target_Levels = colnames(dfs <- df[,c(15,16)])

nn$fitted.values[,1]

predicted = target_Levels[max.col(nn$net.result[[1]])]
actual = target_Levels[max.col(nn$response)]

# Compute confusion matrix and calculate recall on Train Data
conf_Matrix = table(actual, predicted)
conf_Matrix 

rm(predicted, actual, conf_Matrix)

# Remove target attribute from Test Data
test_Data_No_Target = subset(test_Data, select=-c(Species))

# Predict 
nn_predict <- compute(nn, covariate= test_Data_No_Target)
rm(test_Data_No_Target)

# View the predicted values
nn_predict$net.result

# Construct Confusion Matrix 
predicted = target_Levels[max.col(nn_predict$net.result)]
actual = test_Data$Species

# Compute confusion matrix and calculate recall on Train Data
conf_Matrix = table(actual, predicted)
conf_Matrix























library(rpart)

rpart_model<-rpart(Survived~Pclass+Sex+Age+Embarked,data=train,method="anova")
plot(rpart_model,main="Regression Tree for Revenue",margin=0.0001,uniform=TRUE)
text(rpart_model,use.n=T,xpd=T,cex=0.8)


predCartTrain=predict(rpart_model,newdata=train, type="vector")
predCartTest=predict(rpart_model, newdata=test, type="vector")


regr.eval(train[,"Survived"], predCartTrain)

regr.eval(test[,"Survived"], predCartTest)

rpart_model


summary(df$Survived)
unique(df$Survived)
str(df$Survived)



c1 <- cut(df$,breaks = 3, labels = c("low","medium","high"))
str(c1)
table(c1)
dim.data.frame(c1)
df$Revenue <-  c1
str(df)



trainrows <- seq(1,nrow(df),1)
rows <- sample(trainrows,nrow(df)*.70)
train <- df[rows,]
test <- df[-rows,]


library("C50")


C50_model<-rpart(Survived~Pclass+Sex,data=train,method="class")
plot(C50_model,main="Regression Tree for Survived",margin=0.0001,uniform=TRUE)
text(C50_model,use.n=T,xpd=T,cex=0.8)

str(df)
#C50_model=C5.0(Revenue~.,data=train,rules=T)

ds_C50_train = predict(C50_model, train, type="class")
preds_C50_test = predict(C50_model, test, type="class")
confmat_C50_train = table(train$Survived, ds_C50_train)
confmat_C50_test = table(test$Survived, preds_C50_test)



library(caret) 

confusionMatrix(confmat_C50_train)

confusionMatrix(confmat_C50_test)

summary(train$Name)

str(df)
df$Ticket <- as.numeric(df$Ticket)
df$Cabin <- as.factor(df$Cabin)
rpart_model2<-rpart(Survived~Pclass+Sex+Age+Embarked+SibSp+Parch+Fare,data=train,method="class")
plot(rpart_model2)
text(rpart_model2,xpd=T,cex=0.8)

ds_C50_train = predict(rpart_model2, train, type="class")
preds_C50_test = predict(rpart_model2, test, type="class")
confmat_C50_train = table(train$Survived, ds_C50_train)
confmat_C50_test = table(test$Survived, preds_C50_test)


library(caret) 

confusionMatrix(confmat_C50_train)

confusionMatrix(confmat_C50_test)


dfTest <- read.csv("test.csv")

str(df)
str(dfTest)

dfTest$Pclass <- as.factor(dfTest$Pclass)
dfTest$Cabin <- as.character(dfTest$Cabin)

preds_C50_test = predict(rpart_model2, dfTest, type="class")

preds_C50_test

dfTest$Survived <- preds_C50_test

str(dfTest)
res <- data.frame(cbind(dfTest$PassengerId,dfTest$Survived))

str(res)
names(res)
names(res)  <- c("PassengerId","Survived")

write.csv(res,file = "result.csv",row.names = F)






































install.packages('party')
library(party)

levels(df$Embarked)

rpart_model2<-cforest(as.factor(Survived)~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=train,controls=cforest_unbiased(ntree=2000, mtry=3))
plot(rpart_model2)
text(rpart_model2,xpd=T,cex=0.8)


ds_C50_train <- predict(rpart_model2, train, OOB=TRUE, type = "response")

preds_C50_test <- predict(rpart_model2, test, OOB=TRUE, type = "response")


confmat_C50_train = table(train$Survived, ds_C50_train)
confmat_C50_test = table(test$Survived, preds_C50_test)


library(caret) 

confusionMatrix(confmat_C50_train)

confusionMatrix(confmat_C50_test)


dfTest <- read.csv("test.csv")

str(df)
str(dfTest)


dfTest$Pclass <- as.factor(dfTest$Pclass)
dfTest$Cabin <- as.character(dfTest$Cabin)
dfTest$Ticket <- as.numeric(dfTest$Ticket)


dfTest$Survived  <- as.factor(0)
preds_C50_test = predict(rpart_model2, dfTest, OOB=TRUE, type = "response")

preds_C50_test

dfTest$Survived <- preds_C50_test

str(dfTest)
res <- data.frame(cbind(dfTest$PassengerId,dfTest$Survived))

str(res)
names(res)
names(res)  <- c("PassengerId","Survived")

write.csv(res,file = "result.csv",row.names = F)


















str(train)

t <- ctree(
  Survived ~ Pclass + Parch + Ticket + 
    Age  + SibSp + Sex, 
  train,
  controls = ctree_control(
    teststat="quad",
    testtype="Univariate",
    mincriterion=.95,
    minsplit=10, 
    minbucket=5,
    maxdepth=0
  )
)

plot(t)



ds_C50_train <- predict(t, train, OOB=TRUE, type = "response")

preds_C50_test <- predict(t, test, OOB=TRUE, type = "response")


confmat_C50_train = table(train$Survived, ds_C50_train)
confmat_C50_test = table(test$Survived, preds_C50_test)


library(caret) 

confusionMatrix(confmat_C50_train)

confusionMatrix(confmat_C50_test)



dfTest <- read.csv("test.csv")

str(df)
str(dfTest)

levels(dfTest$Embarked) = c("",  "C", "Q", "S")

levels(dfTest$Cabin) <- c(levels(df$Cabin))

levels(dfTest$Cabin)

apply(df,2,function(x){class(x)})
apply(dfTest,2,function(x){class(x)})

dfTest$Survived <- as.factor(c(0,1))
class(df$Pclass)
class(dfTest)

dfTest <- dfTest[,-1]
dfTest$Pclass <- as.factor(dfTest$Pclass)
dfTest$Ticket <- as.numeric(dfTest$Ticket)


preds_C50_test = predict(t, dfTest, OOB=TRUE, type = "response")

preds_C50_test

dfTest$Survived <- preds_C50_test

str(dfTest)
res <- data.frame(cbind(dfTest$PassengerId,dfTest$Survived))

str(res)
names(res)
names(res)  <- c("PassengerId","Survived")

write.csv(res,file = "result.csv",row.names = F)