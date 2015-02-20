# Predicting activity quality of lifting exercises


## Abstract

It is well-known that physical activity improves life quality and also helps to live longer. The main reason is that physical activity reduces the risk of heart disease, obesity and other fatal diseases. Even for healthy people it is recommended to practice exercise regularly to improve their metabolism. 

In order to have an positive impact in our health it is also required to use a proper technique, this means, "activity quality".
Recently, using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is possible to collect data about personal activity. In this work, we will  use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. Our task is to predict the activity quality of these participants. More information is available from the website http://groupware.les.inf.puc-rio.br/har. 


## Loading and Processing the Data

The dataset for our project can be found at the following links, 

A) for the training set https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv and 

B) for the testing set at https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv.

These datasets are part of the original dataset from the link, http://groupware.les.inf.puc-rio.br/static/WLE/WearableComputing_weight_lifting_exercises_biceps_curl_variations.csv.


First, we need to understand these datasets. As a first step we load the training dataset and explore it to identify the most relevant variables for our problem. In the webpage of the original dataset they do not provide a description of variables. 

 

```r
options(scipen = 1, digits = 5)
# Loading the raw csv file
set.seed(4543)
Train <- read.csv("./pml-training.csv",header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA",""))
Test <- read.csv("./pml-testing.csv",header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA",""))
infoTrain <- dim(Train)
# 'data.frame':        19622 obs. of  160 variables:
```

Our training dataset contain 19622 instances and 160 variables.
We check the variable with missing values. Since variables with more than 80% of missing values does not have an impact on the prediction, we decided to remove them from our dataset. From 160 columns we end up with only 60 variables which contain the relevant information for our prediction.


```r
TrainNAfree <- Train[ , ! apply( Train , 2 , function(x) {sum(is.na(x))/length(x)}  >0.8 ) ]
dim(TrainNAfree)
```

```
## [1] 19622    60
```

```r
TrainNAfree$classe <-as.factor(TrainNAfree$classe)
```

For these dataset we do not have a cookbook with the description of all variables. The only information we have is coming from the webpage
http://groupware.les.inf.puc-rio.br/har#weight_lifting_exercises and the corresponding paper related with the data ( for detail see reference [1]) (http://groupware.les.inf.puc-rio.br/public/papers/2013.Velloso.QAR-WLE.pdf). Details about all variable can be retrieved using summary(). 
Since we are interested in predicting the activity quality using the activity monitors, we remove the variables which are not related with activity monitors, these are the first seven variables that are related with personal information (user name) and detail of the experiment as time and window information (e.g. raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp,new_window, num_window). We also remove the first variable, this is only counting each measurement. 


```r
Trainfinal <- TrainNAfree[,-c(1:7)]
str(Trainfinal)
```

```
## 'data.frame':	19622 obs. of  53 variables:
##  $ roll_belt           : num  1.41 1.41 1.42 1.48 1.48 1.45 1.42 1.42 1.43 1.45 ...
##  $ pitch_belt          : num  8.07 8.07 8.07 8.05 8.07 8.06 8.09 8.13 8.16 8.17 ...
##  $ yaw_belt            : num  -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 ...
##  $ total_accel_belt    : int  3 3 3 3 3 3 3 3 3 3 ...
##  $ gyros_belt_x        : num  0 0.02 0 0.02 0.02 0.02 0.02 0.02 0.02 0.03 ...
##  $ gyros_belt_y        : num  0 0 0 0 0.02 0 0 0 0 0 ...
##  $ gyros_belt_z        : num  -0.02 -0.02 -0.02 -0.03 -0.02 -0.02 -0.02 -0.02 -0.02 0 ...
##  $ accel_belt_x        : int  -21 -22 -20 -22 -21 -21 -22 -22 -20 -21 ...
##  $ accel_belt_y        : int  4 4 5 3 2 4 3 4 2 4 ...
##  $ accel_belt_z        : int  22 22 23 21 24 21 21 21 24 22 ...
##  $ magnet_belt_x       : int  -3 -7 -2 -6 -6 0 -4 -2 1 -3 ...
##  $ magnet_belt_y       : int  599 608 600 604 600 603 599 603 602 609 ...
##  $ magnet_belt_z       : int  -313 -311 -305 -310 -302 -312 -311 -313 -312 -308 ...
##  $ roll_arm            : num  -128 -128 -128 -128 -128 -128 -128 -128 -128 -128 ...
##  $ pitch_arm           : num  22.5 22.5 22.5 22.1 22.1 22 21.9 21.8 21.7 21.6 ...
##  $ yaw_arm             : num  -161 -161 -161 -161 -161 -161 -161 -161 -161 -161 ...
##  $ total_accel_arm     : int  34 34 34 34 34 34 34 34 34 34 ...
##  $ gyros_arm_x         : num  0 0.02 0.02 0.02 0 0.02 0 0.02 0.02 0.02 ...
##  $ gyros_arm_y         : num  0 -0.02 -0.02 -0.03 -0.03 -0.03 -0.03 -0.02 -0.03 -0.03 ...
##  $ gyros_arm_z         : num  -0.02 -0.02 -0.02 0.02 0 0 0 0 -0.02 -0.02 ...
##  $ accel_arm_x         : int  -288 -290 -289 -289 -289 -289 -289 -289 -288 -288 ...
##  $ accel_arm_y         : int  109 110 110 111 111 111 111 111 109 110 ...
##  $ accel_arm_z         : int  -123 -125 -126 -123 -123 -122 -125 -124 -122 -124 ...
##  $ magnet_arm_x        : int  -368 -369 -368 -372 -374 -369 -373 -372 -369 -376 ...
##  $ magnet_arm_y        : int  337 337 344 344 337 342 336 338 341 334 ...
##  $ magnet_arm_z        : int  516 513 513 512 506 513 509 510 518 516 ...
##  $ roll_dumbbell       : num  13.1 13.1 12.9 13.4 13.4 ...
##  $ pitch_dumbbell      : num  -70.5 -70.6 -70.3 -70.4 -70.4 ...
##  $ yaw_dumbbell        : num  -84.9 -84.7 -85.1 -84.9 -84.9 ...
##  $ total_accel_dumbbell: int  37 37 37 37 37 37 37 37 37 37 ...
##  $ gyros_dumbbell_x    : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ gyros_dumbbell_y    : num  -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 ...
##  $ gyros_dumbbell_z    : num  0 0 0 -0.02 0 0 0 0 0 0 ...
##  $ accel_dumbbell_x    : int  -234 -233 -232 -232 -233 -234 -232 -234 -232 -235 ...
##  $ accel_dumbbell_y    : int  47 47 46 48 48 48 47 46 47 48 ...
##  $ accel_dumbbell_z    : int  -271 -269 -270 -269 -270 -269 -270 -272 -269 -270 ...
##  $ magnet_dumbbell_x   : int  -559 -555 -561 -552 -554 -558 -551 -555 -549 -558 ...
##  $ magnet_dumbbell_y   : int  293 296 298 303 292 294 295 300 292 291 ...
##  $ magnet_dumbbell_z   : num  -65 -64 -63 -60 -68 -66 -70 -74 -65 -69 ...
##  $ roll_forearm        : num  28.4 28.3 28.3 28.1 28 27.9 27.9 27.8 27.7 27.7 ...
##  $ pitch_forearm       : num  -63.9 -63.9 -63.9 -63.9 -63.9 -63.9 -63.9 -63.8 -63.8 -63.8 ...
##  $ yaw_forearm         : num  -153 -153 -152 -152 -152 -152 -152 -152 -152 -152 ...
##  $ total_accel_forearm : int  36 36 36 36 36 36 36 36 36 36 ...
##  $ gyros_forearm_x     : num  0.03 0.02 0.03 0.02 0.02 0.02 0.02 0.02 0.03 0.02 ...
##  $ gyros_forearm_y     : num  0 0 -0.02 -0.02 0 -0.02 0 -0.02 0 0 ...
##  $ gyros_forearm_z     : num  -0.02 -0.02 0 0 -0.02 -0.03 -0.02 0 -0.02 -0.02 ...
##  $ accel_forearm_x     : int  192 192 196 189 189 193 195 193 193 190 ...
##  $ accel_forearm_y     : int  203 203 204 206 206 203 205 205 204 205 ...
##  $ accel_forearm_z     : int  -215 -216 -213 -214 -214 -215 -215 -213 -214 -215 ...
##  $ magnet_forearm_x    : int  -17 -18 -18 -16 -17 -9 -18 -9 -16 -22 ...
##  $ magnet_forearm_y    : num  654 661 658 658 655 660 659 660 653 656 ...
##  $ magnet_forearm_z    : num  476 473 469 469 473 478 470 474 476 473 ...
##  $ classe              : Factor w/ 5 levels "A","B","C","D",..: 1 1 1 1 1 1 1 1 1 1 ...
```

```r
newDim <- dim(Trainfinal)
```

## Results


Our new dataset contains only 53 variables. 
We plot all the variables to detect outliers and variables with low variability. We may need to remove variables with small variability from our dataset, i.e. those variables that do not change too much for all the instances. 



```r
Trainnoclass <- Trainfinal[,-c(53)]
colnames(Trainnoclass) <- 1:52
Trainnoclass$index <- seq(1:newDim[1])
```
 

 


We do not see variables with too low variability, I am including a plot of some variables with the lower variability. However, we keep these variables because they may carry important information for our classifier.
 
 
 ```r
 library(ggplot2)
 library(grid)
 library(gridExtra)
 p1=qplot(Trainnoclass[,53], Trainnoclass[,1],ylab=names(Trainnoclass)[2],xlab='index')
 p2=qplot(Trainnoclass[,53], Trainnoclass[,2],ylab=names(Trainnoclass)[2],xlab='index')
 p3=qplot(Trainnoclass[,53], Trainnoclass[,3],ylab=names(Trainnoclass)[2],xlab='index')
 p4=qplot(Trainnoclass[,53], Trainnoclass[,4],ylab=names(Trainnoclass)[2],xlab='index')
 p5=qplot(Trainnoclass[,53], Trainnoclass[,5],ylab=names(Trainnoclass)[2],xlab='index')
 p6=qplot(Trainnoclass[,53], Trainnoclass[,6],ylab=names(Trainnoclass)[2],xlab='index')
 grid.arrange(p1, p2, p3, p4, p5 ,p6, ncol = 3, main = "Figure 1")
 ```
 
 ![](machine_learning_project_files/figure-html/Variable-1.png) 

We found few outliers in our data. The number of outliers is so small compared with the number of instances we have in our data that we do not remove them. If we see some indication that these few outliers are deteriorating our classifier, we will remove them later. 
 
 
 ```r
 library(ggplot2)
 library(grid)
 library(gridExtra)
 p1=qplot(Trainnoclass[,53], Trainnoclass[,31],ylab=names(Trainnoclass)[2],xlab='index')
 p2=qplot(Trainnoclass[,53], Trainnoclass[,32],ylab=names(Trainnoclass)[2],xlab='index')
 p3=qplot(Trainnoclass[,53], Trainnoclass[,33],ylab=names(Trainnoclass)[2],xlab='index')
 p4=qplot(Trainnoclass[,53], Trainnoclass[,44],ylab=names(Trainnoclass)[2],xlab='index')
 p5=qplot(Trainnoclass[,53], Trainnoclass[,45],ylab=names(Trainnoclass)[2],xlab='index')
 p6=qplot(Trainnoclass[,53], Trainnoclass[,46],ylab=names(Trainnoclass)[2],xlab='index')
 grid.arrange(p1, p2, p3, p4, p5 ,p6, ncol = 3, main = "Figure 2")
 ```
 
 ![](machine_learning_project_files/figure-html/Outlier-1.png) 
 
Now, we plot the correlation matrix to detect which variables are strongly linear correlated. In this plot we show the linear correlation coefficient for each pair of variable. When this coefficient is closer to one it means that they are very correlated. Figure 3 shows that there are several linear strongly correlated variables.

 
 ```r
 library(corrplot)
 Trainplot <- Trainnoclass[,-c(53)]
 M <- cor(Trainplot)
 corrplot(M, method = "circle",tl.cex = 0.7) #plot matrix
 title("Figure 3", line = -1.5)
 ```
 
 ![](machine_learning_project_files/figure-html/unnamed-chunk-1-1.png) 


Here we use numbers for the column name for a better understanding and visualization. 
From these plots we can see that some variables are very correlated, for instance, the first four variables are very correlated and these are "roll_belt", "pitch_belt","yaw_belt","total_accel_belt". Also we see other regions with large correlation, variable, 4, 8 and 9 ("total_accel_belt",accel_belt_y","accel_belt_z") also variables "magnet_arm_x" "magnet_arm_y" "magnet_arm_z". 
Here are the first ten variables plotted together. 
 

```r
Train1_10 <- Trainnoclass[,1:10]
plot(Train1_10, main="Figure 4")
```

![](machine_learning_project_files/figure-html/Plot of variables-1.png) 

Figure 4 shows that some variables (from 1 to 10) are, in fact, correlated as it was shown from the correlation plot. Since we have too many instances in our dataset it is hard to see from this plot the correlation between variables. 


Now, we want to remove variables that are too correlated. We use the correlation matrix and remove these variables to reduce pair-wise correlation. The cutoff value of the correlation coefficient is 0.75, so, if two variables shows a correlation coefficient larger than 0.75, we remove one of the variable from our dataset. This can be justified based on the fact that when two variables are strongly linear correlated we can say that they are linear dependent, i.e, one variable can be written as a linear combination of the other variable. So, we do not need to consider both variables, including only one will carry the information of both variables. 



```r
library(caret)
```

```
## Loading required package: lattice
```

```r
 M <- cor(Trainnoclass)
 M <- cor(Trainfinal[,-c(53)])
c1 <- findCorrelation(M, cutoff = .75, verbose = FALSE)
TrainnoCorr <- Trainfinal
TrainnoCorr <- TrainnoCorr[,-c1]
dimCorr <- dim(TrainnoCorr)
```
 
We have ended up with a reduced dataset of 33 variables. Here is the correlation plot with the new set of variables.


```r
MnoCorr <- cor(TrainnoCorr[,-c(33)])
corrplot(MnoCorr, method = "circle",tl.cex = 0.7) #plot matrix
title("Figure 5", line = -1)
```

![](machine_learning_project_files/figure-html/Correlation Plot-1.png) 

This new correlation matrix does not show too much correlation between pair of variables as the first plot.  
 

Now, we have a dataset with variable containing relevant information to make prediction. We may need to check the distribution of the variables to detect some skew variables, or also to find some variables with large values compared with other that need to be regularized. For the present project, we decided to keep the variable as it is and make a try in our predictor, if we see that our predictor has a bad quality we need to be back to this point and get a better quality for our dataset. 


Random Forest (RF) is used here as our machine learning algorithm. RF is very powerful and very reliable for classification problem. 
We set by now the number of tree equal to 50. We included the flag importance=TRUE to be able to obtain later the most important variable from our classifier. 


```r
library(randomForest)
```

```
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

```r
TrainnoCorr$classe <- Trainfinal$classe
modrfnoCorr <- randomForest(formula = classe ~ ., data = TrainnoCorr, ntree = 50, importance=TRUE) 
modrfnoCorr
```

```
## 
## Call:
##  randomForest(formula = classe ~ ., data = TrainnoCorr, ntree = 50,      importance = TRUE) 
##                Type of random forest: classification
##                      Number of trees: 50
## No. of variables tried at each split: 5
## 
##         OOB estimate of  error rate: 0.74%
## Confusion matrix:
##      A    B    C    D    E class.error
## A 5576    2    0    1    1  0.00071685
## B   20 3758   15    1    3  0.01027127
## C    0   29 3370   21    2  0.01519579
## D    1    0   36 3174    5  0.01305970
## E    1    0    3    4 3599  0.00221791
```

```r
prffinal = predict(modrfnoCorr,TrainnoCorr)
```


I may need to generate a validation set to verify how good my prediction is. But since I am using random forest, I can use the OOB prediction from the algorithm as an estimation for the OOB (out-of-bag) error.
I perfomed an optimization analysis to find the best value of ntree for our prediction:


```r
 modrfnoCorr <- randomForest(formula = classe ~ ., data = TrainnoCorr, ntree = 500, importance=TRUE,do.trace=100) 
```

```
## ntree      OOB      1      2      3      4      5
##   100:   0.51%  0.09%  0.61%  1.05%  1.00%  0.14%
##   200:   0.43%  0.05%  0.47%  0.88%  0.93%  0.08%
##   300:   0.42%  0.07%  0.42%  0.91%  0.87%  0.08%
##   400:   0.41%  0.05%  0.47%  0.85%  0.84%  0.08%
##   500:   0.42%  0.07%  0.45%  0.82%  0.93%  0.11%
```

From previous data, we found that the optimal values of ntree is 300. For this value the OOB error converge to 0.42%. The value of OOB error of 0.41% for ntree=400 does not look stable because it increases again to 0.42% for ntree=500. 
 
If we separate the data set in k-folds and perform a k-fold cross-validation the OOB error (or error=1-accuracy) will be very similar (for large value of k) to the value from the random forest estimation (for more details, see the link http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm). Note that inside of random forest they perform a leave one out cross validation that corresponds to use k-fold cross-validation where k is equal to the number of data sample, so, the k-fold cross validation error will be closer to the OOB error when k be very large.
The main point is that Random Forest algorithm estimates internally the set error, so we are getting an unbiased estimate of the train/test set error. The random forest algorithm in each tree uses a different bootstrap sample from the original data and also one-third of these sample are not going to be part of the bootstrap sample to generate the tree. If we compare with the cross-validation method, what random forest is doing is very similar to use cross-validation. In some sense, we are doing a cross-validation in each step. In principle, random forest does not suffer of overfitting too much, however it is always better to tune some parameter for a better prediction of an unknown set.  Also note the comment about random forest from the following link (http://topepo.github.io/caret/varimp.html) "For each tree, the prediction accuracy on the out-of-bag portion of the data is recorded. Then the same is done after permuting each predictor variable. The difference between the two accuracies are then averaged over all trees, and normalized by the standard error.". 


Now, we find the most important variables from the random forest algorithm:



```r
importance(modrfnoCorr, type=1)
```

```
##                      MeanDecreaseAccuracy
## yaw_belt                           84.051
## gyros_belt_x                       38.720
## gyros_belt_y                       32.205
## gyros_belt_z                       49.563
## magnet_belt_x                      42.363
## magnet_belt_z                      58.471
## roll_arm                           50.324
## pitch_arm                          36.201
## yaw_arm                            39.981
## total_accel_arm                    39.486
## gyros_arm_x                        42.317
## gyros_arm_z                        37.606
## magnet_arm_x                       29.080
## magnet_arm_z                       36.341
## roll_dumbbell                      53.930
## pitch_dumbbell                     28.702
## yaw_dumbbell                       44.705
## total_accel_dumbbell               46.038
## gyros_dumbbell_y                   36.403
## gyros_dumbbell_z                   50.675
## magnet_dumbbell_z                  69.347
## roll_forearm                       38.319
## pitch_forearm                      49.930
## yaw_forearm                        38.082
## total_accel_forearm                39.030
## gyros_forearm_x                    38.211
## gyros_forearm_y                    43.159
## accel_forearm_x                    34.025
## accel_forearm_z                    35.558
## magnet_forearm_x                   28.497
## magnet_forearm_y                   39.434
## magnet_forearm_z                   48.648
```

From last result we can conclude that all the variables we included in our model are important. We do not see large difference between variable importance.


As a final step, we can try a 2-fold cross-validation and compare the error = 1 - accuracy with that reported from random forest for ntree=200. So, first, we separate the data in two folds,


```r
 inTrain <- createDataPartition(TrainnoCorr$classe,p=0.5,list=FALSE)
 foldn1 <- TrainnoCorr[inTrain,]
 foldn2 <- TrainnoCorr[-inTrain,]
```


then we train the classifier using the random forest algorithm and generate the error.
Now, we train on fold1 and test on fold2.


```r
modfoldn1 <- randomForest(formula = classe ~ ., data = foldn1, ntree = 200) 
predT1test2 <- predict(modfoldn1,foldn2)
confusionMatrix(foldn2$classe,predT1test2)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2783    3    3    1    0
##          B   23 1862   10    1    2
##          C    0   34 1661   16    0
##          D    0    0   27 1576    5
##          E    0    1    3    2 1797
## 
## Overall Statistics
##                                         
##                Accuracy : 0.987         
##                  95% CI : (0.984, 0.989)
##     No Information Rate : 0.286         
##     P-Value [Acc > NIR] : <2e-16        
##                                         
##                   Kappa : 0.983         
##  Mcnemar's Test P-Value : NA            
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.992    0.980    0.975    0.987    0.996
## Specificity             0.999    0.995    0.994    0.996    0.999
## Pos Pred Value          0.997    0.981    0.971    0.980    0.997
## Neg Pred Value          0.997    0.995    0.995    0.998    0.999
## Prevalence              0.286    0.194    0.174    0.163    0.184
## Detection Rate          0.284    0.190    0.169    0.161    0.183
## Detection Prevalence    0.284    0.193    0.174    0.164    0.184
## Balanced Accuracy       0.995    0.988    0.984    0.992    0.998
```

```r
ac1 <- as.data.frame(confusionMatrix(foldn2$classe,predT1test2)$overall)[1,1]
```

We train on fold2 and test on fold1.


```r
modfoldn2 <- randomForest(formula = classe ~ ., data = foldn2, ntree = 200) 
predT2test1 <- predict(modfoldn2,foldn1)
confusionMatrix(foldn1$classe,predT2test1)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2789    0    0    0    1
##          B    8 1882    7    0    2
##          C    0    6 1684   21    0
##          D    2    0   25 1579    2
##          E    0    0    0    6 1798
## 
## Overall Statistics
##                                        
##                Accuracy : 0.992        
##                  95% CI : (0.99, 0.994)
##     No Information Rate : 0.285        
##     P-Value [Acc > NIR] : <2e-16       
##                                        
##                   Kappa : 0.99         
##  Mcnemar's Test P-Value : NA           
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.996    0.997    0.981    0.983    0.997
## Specificity             1.000    0.998    0.997    0.996    0.999
## Pos Pred Value          1.000    0.991    0.984    0.982    0.997
## Neg Pred Value          0.999    0.999    0.996    0.997    0.999
## Prevalence              0.285    0.192    0.175    0.164    0.184
## Detection Rate          0.284    0.192    0.172    0.161    0.183
## Detection Prevalence    0.284    0.194    0.174    0.164    0.184
## Balanced Accuracy       0.998    0.997    0.989    0.990    0.998
```

```r
ac2 <- as.data.frame(confusionMatrix(foldn1$classe,predT2test1)$overall)[1,1]
```

From previous results we can calculate the average error 1.07535%. As we can see this value is larger that the OOB from the random forest algorithm as we expected. They will approach each other only when k is large. I may be that one of the reason that our classifier works very well on our dataset is because our data is balanced, this means that the distribution by class (number of instances) is not so different for every class (classe A = 2803 instances,  B = 1886,  C = 1719,  D = 1599 and  E= 1805).


So, now with the optimized value of ntree and we are ready to make predicton on the test set


```r
Test[,160] <- NULL
predfinal = predict(modrfnoCorr,Test)
submitnoCorr <- as.character(predfinal)
submitnoCorr
```

```
##  [1] "B" "A" "B" "A" "A" "E" "D" "B" "A" "A" "B" "C" "B" "A" "E" "E" "A"
## [18] "B" "B" "B"
```

Since we do not have the actual value for the variable classe for the test set we can not say whether this result is good or not. However, since our error was very small (~ 0.42%) we are expecting good prediction.
Note that we need to remove the last variable from the Test set, this variable does not appear in the training set and without removing it we can not make prediction. Variables we have already included in our Test set should be in the Train set, we can not make a prediction using a variable we have not trained before. However, our Train set can have less variables that our test set, in this case, in the prediction process our classifier will not use these variables.





# References


[1] Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H. Qualitative Activity Recognition of Weight Lifting Exercises. Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human '13) . Stuttgart, Germany: ACM SIGCHI, 2013. (Read more: http://groupware.les.inf.puc-rio.br/har#weight_lifting_exercises#ixzz3S7F1U8XM)

