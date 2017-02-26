library(dplyr)
library(randomForest)
library(lattice)
options(width=85)
head(read.csv('train.csv'))

titanic <- read.csv('train.csv') %>%
  mutate(.,Pclass=factor(Pclass),
         Survived=factor(Survived),
         age=ifelse(is.na(Age),35,Age),
         age = cut(age,c(0,2,5,9,12,15,21,55,65,100)),
         A=grepl('A',Cabin),
         B=grepl('B',Cabin),
         C=grepl('C',Cabin),
         D=grepl('D',Cabin),
         cn = as.numeric(gsub('[[:space:][:alpha:]]','',Cabin)),
                              oe=factor(ifelse(!is.na(cn),cn%%2,-1)),
                              train = sample(c(TRUE,FALSE),
                              size=891,
                              replace=TRUE, 
                              prob=c(.9,.1)   ) )
                              test <- read.csv('test.csv') %>%
                              mutate(.,Pclass=factor(Pclass),
                              age=ifelse(is.na(Age),35,Age),
                              age = cut(age,c(0,2,5,9,12,15,21,55,65,100)),
                              A=grepl('A',Cabin),
                              B=grepl('B',Cabin),
                              C=grepl('C',Cabin),
                              D=grepl('D',Cabin),
                              cn = as.numeric(gsub('[[:space:][:alpha:]]','',Cabin)),
         oe=factor(ifelse(!is.na(cn),cn%%2,-1)),
         Embarked=factor(Embarked,levels=levels(titanic$Embarked))
                              )
test$Fare[is.na(test$Fare)]<- median(titanic$Fare)














rf1 <- randomForest(Survived ~ 
                      Sex+Pclass + SibSp +
                      Parch + Fare + 
                      Embarked + age +
                      A+B+C+D +oe,
                    data=titanic,
                    subset=train,
                    replace=FALSE,
                    ntree=1000)














titanic$pred <- predict(rf1,titanic)
with(titanic[!titanic$train,],sum(pred!=Survived)/length(pred))

mygrid <- expand.grid(nodesize=c(2,4,6),
                      mtry=2:5,
                      wt=seq(.5,.7,.05))
sa <- sapply(1:nrow(mygrid), function(i) {
  rfx <- randomForest(Survived ~ 
                        Sex+Pclass + SibSp +
                        Parch + Fare + 
                        Embarked + age +
                        A+B+C+D +oe,
                      data=titanic,
                      subset=train,
                      replace=TRUE,
                      ntree=4000,
                      nodesize=mygrid$nodesize[i],
                      mtry=mygrid$mtry[i],
                      classwt=c(1-mygrid$wt[i],mygrid$wt[i]))  
  preds <- predict(rfx,titanic[!titanic$train,])
  nwrong <- sum(preds!=titanic$Survived[!titanic$train])
  c(nodesize=mygrid$nodesize[i],mtry=mygrid$mtry[i],wt=mygrid$wt[i],pw=nwrong/length(preds))
})
tsa <- as.data.frame(t(sa))
xyplot(pw ~ wt | mtry,group=factor(nodesize), data=tsa,auto.key=TRUE,type='l')





rf2 <- randomForest(Survived ~ 
                      Sex+Pclass + SibSp +
                      Parch + Fare + 
                      Embarked + age +
                      A+B+C+D +oe,
                    data=titanic,
                    replace=TRUE,
                    ntree=5000,
                    nodesize=4,
                    mtry=3,
                    classwt=c(1-.6,.6))  

pp <- predict(rf2,test)
out <- data.frame(
  PassengerId=test$PassengerId,
  Survived=pp,row.names=NULL)
write.csv(x=out,
          file='rf1.csv',
          row.names=FALSE,
          quote=FALSE)
