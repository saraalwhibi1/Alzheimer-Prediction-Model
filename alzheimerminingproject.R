dataset = read.csv('alzheimer.csv')
str(dataset)

#data cleaning
#checking null
is.na(dataset)
sum(is.na(dataset))
dataset$SES [is.na(dataset$SES)] <- mean(dataset$SES, na.rm = TRUE)
dataset$MMSE [is.na(dataset$MMSE)] <- mean(dataset$SES, na.rm = TRUE)
sum(is.na(dataset))

#remove outliers
install.packages('outliers')
library(outliers)
outEDUC=outlier(dataset$EDUC,logical=TRUE)
Find_outlier1 = which(outEDUC==TRUE, arr.ind=TRUE)
dataset=dataset[-Find_outlier1,]
nrow(dataset)

outSES=outlier(dataset$SES,logical=TRUE)
Find_outlier2 = which(outSES==TRUE, arr.ind=TRUE)
dataset=dataset[-Find_outlier2,]
nrow(dataset)

outCDR=outlier(dataset$CDR,logical=TRUE)
Find_outlier3 = which(outCDR==TRUE, arr.ind=TRUE)
dataset=dataset[-Find_outlier3,]
nrow(dataset)

outeTIV=outlier(dataset$eTIV,logical=TRUE)
Find_outlier4 = which(outeTIV==TRUE, arr.ind=TRUE)
dataset=dataset[-Find_outlier4,]
nrow(dataset)

outMMSE=outlier(dataset$MMSE,logical=TRUE)
Find_outlier5 = which(outMMSE==TRUE, arr.ind=TRUE)
dataset=dataset[-Find_outlier5,]
nrow(dataset)

outASF=outlier(dataset$ASF,logical=TRUE)
Find_outlier6 = which(outASF==TRUE, arr.ind=TRUE)
dataset=dataset[-Find_outlier6,]
nrow(dataset)


#data transformation
#encoding
dataset$M.F = factor(dataset$M.F, levels = c('M','F'), labels = c(0,1))
dataset$Group = factor(dataset$Group, levels = c('Nondemented','Demented','Converted'), labels = c(1,2,3))
View(dataset)
#normalization
normalize <- function(x) {return((x-min(x))/(max(x)-min(x)))}
dataset$eTIV <- normalize(dataset$eTIV)
str(dataset)



#Before we start classification we will remove the third class label which is "Converted", due to our small data set.
library(Hmisc)
describe(dataset)
dataset = dataset [!(dataset$Group== 3), ]
describe(dataset)


#classification
set.seed(2894)



#1-Split data (50% - 50%)
#MyFormula and tables:
ind <- sample(2, nrow(dataset), replace= TRUE, prob= c(0.5,0.5))
trainData <- dataset[ind2==1,]
testData  <- dataset[ind2==2,]
myFormula <- Group ~ M.F + Age + EDUC + SES + MMSE + CDR + eTIV + nWBV +ASF
dataset_ctree <- ctree(myFormula, data=trainData)
table(predict(dataset_ctree), trainData$Group)

#Trees:
print(dataset_ctree)
plot(dataset_ctree)
plot(dataset_ctree, type='simple')

#Test:
testPred <- predict(dataset_ctree, newdata = testData)

#Evalute Model:
table(testPred,testData$Group)
library(e1071)
library(caret)
results <- confusionMatrix(testPred, testData$Group)
acc<- results$overall["Accuracy"]*100
acc
results


#2-Split data (70% - 30%)
#MyFormula and tables:
ind <- sample(2, nrow(dataset), replace= TRUE, prob= c(0.7,0.3))
trainData <- dataset[ind==1,]
testData  <- dataset[ind==2,]
myFormula <- Group ~ M.F + Age + EDUC + SES + eTIV + nWBV +ASF
dataset_ctree <- ctree(myFormula, data=trainData)
table(predict(dataset_ctree), trainData$Group)

#Trees:
print(dataset_ctree)
plot(dataset_ctree)
plot(dataset_ctree, type='simple')

#Test:
testPred <- predict(dataset_ctree, newdata = testData)

#Evalute Model:
table(testPred,testData$Group)
library(e1071)
library(caret)
results <- confusionMatrix(testPred, testData$Group)
acc<- results$overall["Accuracy"]*100
acc
results


#3-Split data (80% - 20%)
#MyFormula and tables:
ind <- sample(2, nrow(dataset), replace= TRUE, prob= c(0.8,0.2))
trainData <- dataset[ind==1,]
testData  <- dataset[ind==2,]
myFormula <- Group ~ M.F + Age + EDUC + SES + eTIV + nWBV +ASF
dataset_ctree <- ctree(myFormula, data=trainData)
table(predict(dataset_ctree), trainData$Group)

#Trees:
print(dataset_ctree)
plot(dataset_ctree)
plot(dataset_ctree, type='simple')

#Test:
testPred <- predict(dataset_ctree, newdata = testData)

#Evalute Model:
table(testPred,testData$Group)
((117+84)/nrow(testData))*100
results <- confusionMatrix(testPred, testData$Group)
acc<- results$overall["Accuracy"]*100
acc
results


#clustering

#convert to numeric before clustering
dataset$M.F <- as.numeric(as.character(dataset$M.F))


# k-means clustering 

set.seed(8953)
# prepreocessing 
dataset1 <- dataset [ ,c(2,3,4,5,6,7,8,9)]
dataset1 <- scale(dataset1)
## visualize clustering package
install.packages("factoextra")
library(factoextra)
library(ggplot2)
#valdation package
library(cluster)


# run kmeans clustering to find 2 clusters
kmeans.alzheimer <- kmeans(dataset1, 2)
# print the clusterng result
kmeans.alzheimer
## visualize clustering
fviz_cluster(kmeans.alzheimer, data = dataset1)
#Silhouette width for each cluster 
Silhouette <- silhouette (kmeans.alzheimer$cluster,dist(dataset1))
fviz_silhouette(Silhouette)


# run kmeans clustering to find 3 clusters
kmeans.alzheimer <- kmeans(dataset1, 3)
# print the clusterng result
kmeans.alzheimer
## visualize clustering
fviz_cluster(kmeans.alzheimer, data = dataset1)
#Silhouette width for each cluster 
Silhouette <- silhouette (kmeans.alzheimer$cluster,dist(dataset1))
fviz_silhouette(Silhouette)


# run kmeans clustering to find 4 clusters
kmeans.alzheimer <- kmeans(dataset1, 4)
# print the clusterng result
kmeans.alzheimer
## visualize clustering
fviz_cluster(kmeans.alzheimer, data = dataset1)
#Silhouette width for each cluster 
Silhouette <- silhouette (kmeans.alzheimer$cluster,dist(dataset1))
fviz_silhouette(Silhouette)


#Optimal number of clusters for all clusters:
fviz_nbclust(dataset1, kmeans, method = "silhouette")+labs(subtitle = "Silhouette method")





