

##Make Date Feature:calculate days as a percentage 
##of Days/365
makeDateFeature<-function(df){
  Month <- as.numeric(substr(df$Date,6,7))
  df$Month <- as.factor(Month)
  Day <- as.numeric(substr(df$Date,9,10))
  Month_to_Day <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  Month_culmulate_Day <- sapply(1:12,function(x){
    sum(Month_to_Day[1:x])
  })
  TotalDay <- Month_culmulate_Day[Month] + Day
  df$TotalDayFeature <- TotalDay/365
  
  return(df)
}
#Input the data from Source
Source <- "/Users/yifanli/Downloads/Walmart-Competetion/"
readcsv <- function(path){
  path <-  paste0(Source,path)
  output <- read.csv(path,stringsAsFactors=F)
}
train <- readcsv("train.csv")
test <- readcsv("test.csv")
stores <- readcsv("stores.csv")
features <- readcsv("features.csv")
sampleSubmission <- readcsv("sampleSubmission.csv")
#Make the original isHolday to IsHoliday to align things
colnames(train)[5] <- "IsHoliday"

#Make the Date to have a format of Date rather String
train$Date <- as.Date(train$Date,format='%Y-%m-%d')
test$Date <- as.Date(test$Date,format='%Y-%m-%d')
features$Date <- as.Date(features$Date,format='%Y-%m-%d')

#Make the weekly_sales as numeric
train$Weekly_Sales <- as.numeric(sub("\\,", "",train$Weekly_Sales))
sampleSubmission$Weekly_Sales <- as.numeric(sub("\\,", "",sampleSubmission$Weekly_Sales))
#Merge several dataframes to one big dataframe
train_feature <- merge(train,features,by=c("Store","Date","IsHoliday"))
tfs <- merge(train_feature,stores,by="Store")
tfs <- makeDateFeature(tfs)
tfs <- fillMarkDownZero(tfs)


#Make the NA markdown to 0
fillMarkDownZero <- function(df) {
  df$MarkDown1[is.na(df$MarkDown1)] <- 0
  df$MarkDown2[is.na(df$MarkDown2)] <- 0
  df$MarkDown3[is.na(df$MarkDown3)] <- 0
  df$MarkDown4[is.na(df$MarkDown4)] <- 0
  df$MarkDown5[is.na(df$MarkDown5)] <- 0
  return (df)
}


#Get ff, the test dataset
test_feature <- merge(test,features,by=c("Date","Store","IsHoliday"))
testfs <- merge(test_feature,stores,by="Store")
testfs<-makeDateFeature(testfs)
testfs <- fillMarkDownZero(testfs)
sum(is.na(testfs))
head(testfs,100)

#Make the CPI and Unemployment rate filled in for test dataframe
cpitrain <- testfs[!is.na(testfs$CPI),]
cpifill <- testfs[is.na(testfs$CPI),]

#Predict cpi using linear regression model
cpimodel <- lm(CPI~Temperature+Fuel_Price,data=cpitrain)
summary(cpimodel)
testfs[is.na(testfs$CPI),]$CPI <-predict(cpimodel,cpifill)
sum(is.na(testfs$CPI))

emptrain <- testfs[!is.na(testfs$Unemployment),]
empfill <- testfs[is.na(testfs$Unemployment),]
#Predict employment using linear regression model
empmodel <- lm(Unemployment~Temperature+CPI,data=emptrain)
summary(empmodel)
testfs[is.na(testfs$Unemployment),]$Unemployment <-predict(empmodel,empfill)
sum(is.na(testfs$Unemployment))

#Reorder the testfs so that the Store, Dept and Date
#To in a strict increasing order
testfs_reorder <- data.frame(Store=testfs$Store,
                             Dept=testfs$Dept,
                             Date=testfs$Date,testfs[,c(-1,-2,-4)])

ff<- testfs_reorder[do.call(order,testfs_reorder),]
rownames(ff)<-NULL
tfs$Month <- as.numeric(tfs$Month)

#Write it out... So that you can directly access it
#from the csvs next time
write.csv(ff,file=paste0(Source,"ff.csv"),row.names=FALSE)
write.csv(tfs,file=paste0(Source,"tfs.csv"),row.names=FALSE)

#Using weighted LAD(Least Absolute Deviations)
#Which is the rule that the Kaggle uses to evaluation:
#http://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting/details/evaluation
#I uses this method to get the best result between different
#models
library(quantreg)

weight <- rep(1,nrow(tfs))
weight[tfs$IsHoliday == T] <- 5

#The rq{quantreg} Quantile Regression
fit1 <- rq(weights = weight,method="fn",tau = 26/50,Weekly_Sales ~poly(TotalDayFeature,3)+ Size+IsHoliday:Month  + Temperature:Fuel_Price  
           +Dept * (MarkDown1+MarkDown2+MarkDown3 + MarkDown5+MarkDown4)+CPI+Unemployment*Store,data=tfs)
summary(fit1)
ff$Month <- as.numeric(ff$Month)
pre_rq <- predict.rq(fit1,newdata = ff)
realSubmission<- data.frame(Id=sampleSubmission$Id,
                            Weekly_Sales=pre_rq)
write.csv(realSubmission,file=paste0(Source,"quantreg.csv"),row.names=FALSE)

#The linear regression modeling
model <- lm(weights = weight,Weekly_Sales ~poly(TotalDayFeature,3)+ Size+IsHoliday:Month  + Temperature:Fuel_Price  
            +Dept * (MarkDown1+MarkDown2+MarkDown3 + MarkDown5+MarkDown4)+CPI+Unemployment*Store,data=tfs)
summary(model) 

#There is no Type C in training data
ff$Type[ff$Type=="C"]<-"B"
ff$Month <- as.numeric(ff$Month)
#predictions
pre<- predict(model,ff)

realSubmission<- data.frame(Id=sampleSubmission$Id,
                            Weekly_Sales=pre)

write.csv(realSubmission,file=paste0(Source,"unemploy+store.csv"),row.names=FALSE)



#The glmnet modeling     
X= model.matrix(Weekly_Sales ~poly(TotalDayFeature,3)+ Size+IsHoliday:Month  + Temperature:Fuel_Price  
+Dept * (MarkDown1+MarkDown2+MarkDown3 + MarkDown5+MarkDown4)+CPI+Unemployment*Store,data=tfs)                 
Y = tfs$Weekly_Sales


library(glmnet)
ncol(ff)
ncol(tfs)
sum(is.na(ff))
#get the weights distribution based on the description on Kaggle competition
weight <- rep(1,nrow(tfs))
weight[tfs$IsHoliday == T] <- 5
str(weight)
#ridge alpha = 0 (better)

#lasso alpha = 1
#elastic net alpha = 0.5
m <- glmnet(X, Y, alpha=0,weights = weight)
summary(m)
print(m)
plot(m, xvar='lambda')
m <- cv.glmnet(X, Y, alpha=0, nfolds=10,weights = weight)
plot(m)
m$lambda


#ff$IsHoliday <- ff$IsHoliday * 3
popsX <- model.matrix( ~poly(TotalDayFeature,3)+ Size+IsHoliday:Month  + Temperature:Fuel_Price  
                      +Dept * (MarkDown1+MarkDown2+MarkDown3+MarkDown4+MarkDown5)+CPI+Unemployment,data=ff)
#popsX <- model.matrix(~ TotalDayFeature + poly(TotalDayFeature,2)+IsHoliday * Month+poly(Temperature^2)
 #                     +Dept*(MarkDown1+MarkDown2+MarkDown3+MarkDown4+MarkDown5)+CPI+Size, data=ff)
#predictions <- predict(m, popsX, s=m$lambda[100])
predictions <- predict(m, popsX, s='lambda.min')
?predict
str(ff)
realSubmission<- data.frame(Id=sampleSubmission$Id,"Weekly_Sales"=data.frame(predictions))
colnames(realSubmission)[2]<-"Weekly_Sales"
head(realSubmission)
nrow(realSubmission)


write.csv(realSubmission,file=paste0(Source,"submission_ridge10_with_alpha_0.2.csv"),row.names=FALSE)


###End


##Some data Visualizations::
?write.csv
?aggregate
cc <- aggregate(Weekly_Sales ~ Store + Date,train,mean)
cc

##Sizes comparison between train and test data
library(ggplot2)


ggplot()+geom_jitter(data=ff,aes(x=Type,y=Size))
ggplot()+geom_jitter(data=tfs,aes(x=Type,y=Size))

??geom

ff$veg <- 'test'
tfs$veg <- 'train'
ff$Weekly_Sales <- 1

vegLengths <- rbind(ff[,c('veg','Size','Weekly_Sales')],tfs[,c('veg','Size','Weekly_Sales')])
head(vegLengths)
ggplot()+geom_histogram(data=vegLengths,aes(x=Size,fill=veg))+
  geom_histogram(data=vegLengths,aes(x=Weekly_Sales,fill=veg))

ggplot()+geom_histogram(data=vegLengths,aes(x=Weekly_Sales,fill=veg))

