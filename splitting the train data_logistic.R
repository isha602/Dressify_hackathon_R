trainn=read.csv("C:/Users/Isha/Desktop/train.csv")
testt=read.csv("C:/Users/Isha/Desktop/testcopy.csv")
View(testt)
library(tidyverse)
library(caTools)
#library(visdat)
library(ROCR)
library(e1071)
library(caret)
cleaning_data = function(train){
  not_req <- c("0","null","NULL")
  index_remove <- c()
  for (i in 2:dim(train)[2]){
    vv <- class(train[,i])
    if (vv == "character"){
      t <- table(train[,i])
      t <- data.frame(t)
      r <- with(train, which(train[,i]==0 | train[,i]=="NULL", arr.ind=TRUE))
      index_remove<- append(index_remove,r)#Appending the row index's in the arracy ()
    }
  }
  train1<-train[-index_remove,]
  return(train1)
}

trainn=cleaning_data(trainn)

dim(trainn)


View(testt)#****************************************************
#DATA PRE-PROCESSING 

#cleaning train and test style column data 
table(trainn$Style)
trainn$Style=as.character(trainn$Style)
trainn$Style[c(which(trainn$Style=="sexy"))]="Sexy"
#trainnn$Style=as.character(trainnn$Style)
trainn$Style=as.factor(trainn$Style)
levels(trainn$Style)


table(testt$Style)
testt$Style=as.character(testt$Style)
testt$Style[c(which(testt$Style=="sexy"))]="Sexy"
#trainn$Style=as.character(trainn$Style)
testt$Style=as.factor(testt$Style)
levels(testt$Style)

#cleaning trainn price column data
summary(trainn$Price)
table(trainn$Price)
trainn$Price=as.character(trainn$Price)
trainn$Price[c(which(trainn$Price=='low'))]='Low'
trainn$Price[c(which(trainn$Price=='high'))]='High'
trainn$Price[which(trainn$Price=="")] = 'Average'
trainn$Price[which(trainn$Price=="0")] = 'Average'
trainn$Price=as.character(trainn$Price)
trainn$Price =as.factor(trainn$Price)
levels(trainn$Price)
View(trainn)
length(trainn$Recommendation)

#cleaning testt price column data
table(testt$Price)
testt$Price[c(which(testt$Price=='low'))]='Low'
testt$Price[c(which(testt$Price=='high'))]='High'
testt$Price[which(testt$Price=="")] = 'Average'
testt$Price=as.character(testt$Price)
testt$Price =as.factor(testt$Price)
levels(testt$Price)


library(dplyr)
str(trainn$Rating)
str(testt$Rating)
summary(trainn$Rating)
summary(testt$Rating)
#hist(trainn$Rating,main = "Rating in trainn data",xlab = "Rating",col = "red")
#hist(testt$Rating,main = "Rating in testt data",xlab = "Rating",col = "blue")

dim(subset(trainn,Rating >=1 & Rating <= 3))
#next we check number of observations having ratings between 0 to 1
dim(subset(trainn,Rating >=0 & Rating <=1))

#checking the summary of the above range of rating scores
summary(select(subset(trainn,Rating >=0 & Rating <=1),Rating))
#So, we see all observations in above category have in fact, 0 rating

#next we check the portion 3-4
dim(subset(trainn,Rating >=3 & Rating <=3.5))
dim(subset(trainn,Rating >=3.5 & Rating <=4))

#and now we check portion 4-4.5 and 4.5-5
dim(subset(trainn,Rating >=4 & Rating <=4.5))
dim(subset(trainn,Rating >=4.5 & Rating <=5))

# binning the rating variable for both the trainn and testt sets
trainn$Rating[trainn$Rating >=0  & trainn$Rating <= 1 ] <- 0
trainn$Rating[trainn$Rating >= 2.5 & trainn$Rating <= 4.5] <- 1
trainn$Rating[trainn$Rating > 4.5 & trainn$Rating <= 5] <- 2

#performing the same operations on testt data,
testt$Rating[testt$Rating >= 0 & testt$Rating <= 1] <- 0
testt$Rating[testt$Rating >= 2.5 & testt$Rating <= 4.5] <- 1
testt$Rating[testt$Rating > 4.5 & testt$Rating <= 5] <- 2

trainn$Rating <- factor(trainn$Rating)
testt$Rating <- factor(testt$Rating)

table(trainn$Rating,trainn$Recommendation)
prop.table(table(trainn$Rating,trainn$Recommendation),1)*100
summary(trainn$Rating)
summary(testt$Rating)


# cleaning size trainn and testting 
table(trainn$Size)
summary(trainn$Size)
trainn$Size =as.character(trainn$Size)
trainn$Size[c(which(trainn$Size=='free'))]='F'
trainn$Size[c(which(trainn$Size=='small'))]='S'
trainn$Size[c(which(trainn$Size=='s'))]='S'
trainn$Size =as.factor(trainn$Size)
levels(trainn$Size)

table(testt$Size)
summary(testt$Size)
testt$Size =as.character(testt$Size)
testt$Size[c(which(testt$Size=='free'))]='F'
testt$Size[c(which(testt$Size=='small'))]='S'
testt$Size[c(which(testt$Size=='s'))]='S'
testt$Size =as.factor(testt$Size)
levels(testt$Size)


# cleaning neckline trainnig and test data

trainn$NeckLine
summary(trainn$NeckLine)
table(trainn$NeckLine)
trainn$NeckLine=as.character(trainn$NeckLine)
trainn$NeckLine[trainn$NeckLine == "sweetheart"] = "Sweetheart"
#trainn$NeckLine[trainn$NeckLine == "0"] = " o-neck "
trainn$NeckLine=as.factor(trainn$NeckLine)
levels(trainn$NeckLine)
#plot(trainn$NeckLine)

testt$NeckLine
summary(testt$NeckLine)
table(testt$NeckLine)
testt$NeckLine=as.character(testt$NeckLine)
testt$NeckLine[testt$NeckLine == "sweetheart"] = "Sweetheart"
testt$NeckLine[testt$NeckLine == "NULL"] = "o-neck"
testt$NeckLine=as.factor(testt$NeckLine)
levels(testt$NeckLine)
#plot(testt$NeckLine)

#cleaning waistline trainning and testing data
table(trainn$waiseline)
summary(trainn$waiseline)
trainn$waiseline=as.character(trainn$waiseline)
trainn$waiseline[trainn$waistline=="0"]="natural"
trainn$waiseline=as.factor(trainn$waiseline)
levels(trainn$waiseline)
#plot(trainn$waiseline)

testt$waiseline
summary(testt$waiseline)
table(testt$waiseline)
testt$waiseline=as.character(testt$waiseline)
testt$waiseline[testt$waistline=="princess"]="null"
testt$waiseline=as.factor(testt$waiseline)
levels(testt$waiseline)


# cleaning season training and testing data
trainn$Season
summary(trainn$Season)
table(trainn$Season)
trainn$Season=as.character(trainn$Season)
trainn$Season[c(which(trainn$Season=='Automn'))]='Autumn'
trainn$Season[c(which(trainn$Season=='spring'))]='Spring'
trainn$Season[c(which(trainn$Season=='summer'))]='Summer'
trainn$Season[c(which(trainn$Season=='winter'))]='Winter'
trainn$Season[which(trainn$Season=="")] = 'Summer'
trainn$Season[which(trainn$Season=="0")] = 'Summer'
trainn$Season=as.factor(trainn$Season)
levels(trainn$Season)
#plot(trainn$Season)
table(trainn$Season)

testt$Season
summary(testt$Season)
testt$Season=as.character(testt$Season)
testt$Season[c(which(testt$Season=='Automn'))]='Autumn'
testt$Season[c(which(testt$Season=='spring'))]='Spring'
testt$Season[c(which(testt$Season=='summer'))]='Summer'
testt$Season[c(which(testt$Season=='winter'))]='Winter'
testt$Season[which(testt$Season=="")] = 'Summer'
testt$Season=as.factor(testt$Season)
levels(testt$Season)
#plot(testt$Season)
table(testt$Season)

# cleaning Sleevelength training and testing data
table(trainn$SleeveLength)
trainn$SleeveLength[trainn$SleeveLength %in% c('sleeevless')] = 'sleevless'
trainn$SleeveLength[trainn$SleeveLength %in% c('sleeveless')] = 'sleevless'
trainn$SleeveLength[trainn$SleeveLength %in% c('sleveless')] = 'sleevless'
#trainn$SleeveLength[trainn$SleeveLength %in% c('cap-sleeves')] = 'capsleeves'
trainn$SleeveLength[trainn$SleeveLength %in% c('half')] = 'halfsleeve'
trainn$SleeveLength[trainn$SleeveLength %in% c('thressqatar')] = 'threequarter'
trainn$SleeveLength[trainn$SleeveLength %in% c('threequater')] = 'threequarter'
trainn$SleeveLength[trainn$SleeveLength %in% c('NULL')] = 'sleevless'
trainn$SleeveLength[trainn$SleeveLength == "Butterfly"] <- "OTHER"
trainn$SleeveLength[trainn$SleeveLength == "cap-sleeves"] <- "OTHER"
trainn$SleeveLength[trainn$SleeveLength == "Petal"] <- "OTHER"
trainn$SleeveLength[trainn$SleeveLength == "turndowncollor"] <- "OTHER"
trainn$SleeveLength=as.character(trainn$SleeveLength)
trainn$SleeveLength=as.factor(trainn$SleeveLength)
levels(trainn$SleeveLength)
#plot(trainn$SleeveLength)

table(testt$SleeveLength)
testt$SleeveLength[testt$SleeveLength %in% c('sleeevless')] = 'sleevless'
testt$SleeveLength[testt$SleeveLength %in% c('sleeveless')] = 'sleevless'
testt$SleeveLength[testt$SleeveLength %in% c('sleveless')] = 'sleevless'
testt$SleeveLength[testt$SleeveLength %in% c('capsleeves')] = 'OTHER'
testt$SleeveLength[testt$SleeveLength %in% c('half')] = 'halfsleeve'
testt$SleeveLength[testt$SleeveLength %in% c('thressqatar')] = 'threequarter'
testt$SleeveLength[testt$SleeveLength %in% c('threequater')] = 'threequarter'
testt$SleeveLength[testt$SleeveLength %in% c('NULL')] = 'sleevless'
testt$SleeveLength[testt$SleeveLength %in% c('cap-sleeves')] = 'OTHER'
testt$SleeveLength=as.character(testt$SleeveLength)
testt$SleeveLength=as.factor(testt$SleeveLength)
levels(testt$SleeveLength)
#plot(testt$SleeveLength)

# cleaning Material training and testing data

table(trainn$Material)
trainn$Material=as.character(trainn$Material)
trainn$Material[c(which(trainn$Material=='modal'))]='model'
trainn$Material[c(which(trainn$Material=='sill'))]='silk'
trainn$Material[c(which(trainn$Material=='viscos'))]='cotton'
trainn$Material[c(which(trainn$Material=='knitting'))]='cotton'
trainn$Material[c(which(trainn$Material=='lace'))]="silk"
trainn$Material=as.factor(trainn$Material)
levels(trainn$Material)
#plot(trainn$Material)

table(testt$Material)
testt$Material=as.character(testt$Material)
testt$Material[c(which(testt$Material=='modal'))]='model'
testt$Material[c(which(testt$Material=='sill'))]='silk'
testt$Material[c(which(testt$Material=='viscos'))]='cotton'
testt$Material[c(which(testt$Material=='knitting'))]='cotton'
testt$Material[c(which(testt$Material=='lace'))]="silk"
testt$Material=as.factor(testt$Material)
levels(testt$Material)
#plot(testt$Material)

# cleaning Fabric type training and testing data

table(trainn$FabricType)
trainn$FabricType=as.character(trainn$FabricType)
#trainn$FabricType[trainn$FabricType=='flannael']='flannel'
#trainn$FabricType[trainn$FabricType=='woolen']='wollen'
#trainn$FabricType[trainn$FabricType=='null']='chiffon'
#trainn$FabricType[trainn$FabricType=='0']='chiffon'
trainn$FabricType[trainn$FabricType=='terry']='flannael'
trainn$FabricType=as.factor(trainn$FabricType)
levels(trainn$FabricType)
table(testt$FabricType)
testt$FabricType[testt$FabricType=='terry']='flannael'
# cleaning pattern type training and testing data
table(trainn$Pattern.Type)
trainn$Pattern.Type=as.character(trainn$Pattern.Type)
trainn$Pattern.Type[trainn$Pattern.Type=='leopard']='animal'
trainn$Pattern.Type[trainn$Pattern.Type=='splice']='solid'
trainn$Pattern.Type[trainn$Pattern.Type=='floral']='solid'
#trainn$Pattern.Type[trainn$Pattern.Type=='0']='solid'
trainn$Pattern.Type=as.factor(trainn$Pattern.Type)
levels(trainn$Pattern.Type)

table(trainn$Decoration)
trainn$Decoration=as.character(trainn$Decoration)
trainn$Decoration[trainn$Decoration=='rivet']='lace'

trainn$Decoration=as.factor(trainn$Decoration)
levels(trainn$Decoration)

#table(trainn$Decoration)
testt$Decoration=as.character(testt$Decoration)
testt$Decoration[testt$Decoration=='rivet']='lace'

trainn$Decoration=as.factor(trainn$Decoration)
levels(trainn$Decoration)
table(testt$Pattern.Type)

testt$Pattern.Type=as.character(testt$Pattern.Type)
testt$Pattern.Type[testt$Pattern.Type=='leapord']='animal'
testt$Pattern.Type[testt$Pattern.Type=='splice']='solid'
testt$Pattern.Type[testt$Pattern.Type=='floral']='solid'
testt$Pattern.Type=as.factor(testt$Pattern.Type)
levels(testt$Pattern.Type)


#***********************************************************************************

#applying logistic regression model 

#install.packages('caTools')

library(caTools)
set.seed(88)
split = sample.split(trainn$Recommendation, SplitRatio = 0.8)
#splittung the training data
dresstrain = subset(trainn, split == TRUE)
dresstest = subset(trainn, split == FALSE)
#logistic regression model
modeltest = glm (Recommendation ~ ., data = dresstrain[,c(-1,-7)], family = binomial)
summary(modeltest)

#predicting for the dresstrain data 
predict_trainn = predict(modeltest, type = 'response')
predict_trainn
predict_train1=ifelse(predict_trainn>0.5,1,0)
predict_train1
table_mat01 = table(dresstrain$Recommendation, predict_train1)
table_mat01

#checking the accuracy for dresstrain 
accuracy_Testt = sum(diag(table_mat01)) / sum(table_mat01)
accuracy_Testt

#prediction for the dresstest data
predicttest = predict(modeltest, type = 'response',newdata=testt[,c(-1,-7,-14)])
predicttest

pred=ifelse(predicttest>0.5,1,0)
pred
testt$Recommendation_prediction <- pred
write.csv(testt, "A008_RDS_SUBMISSION_FINAL_08.csv")

