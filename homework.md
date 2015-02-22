---
title: "HW"
author: "J.Pan"
date: "Tuesday, February 17, 2015"
output: md_document:
  variant: markdown_github
---

Load data
=======================================================================
That will be better if I can understand the meaning of all the variable, But that is not the case here. In this section, i load my data.


```r
setwd("D:/stat learning")
test<-read.table("pml-testing.csv",header=T,sep=",")
training<-read.table("pml-training.csv",header=T,sep=",")
dim(test)
```

```
## [1]  20 160
```

Data preprocessing
============================================
take away the NA-value or meaningless column in testing and training data.

Because the variable "new_window" are all "no" in the tesing data
I don't think the variable is informative in training data .
Base on this ,I also took out the sample with new_window is "yes" in training data

Time data is also meaningless for me


```r
NA_number<-apply(test,2,function(x) sum(is.na(x)))
new_test<-test[,-which(NA_number==20)]
new_test$new_window      <- NULL
new_test$cvtd_timestamp      <- NULL
dim(new_test)
```

```
## [1] 20 58
```

```r
new_training<-training[-which(NA_number==20)]
del<-which(training$new_window=="yes")
new_training<-new_training[-del,]
new_training$new_window      <- NULL
new_training$cvtd_timestamp      <- NULL
dim(new_training)
```

```
## [1] 19216    58
```

Seprate the sample by name
===========================================================================
Because this is a biological data, i believe the indiviual effect is exist.
So I split data by their user name.


```r
library(ggplot2)

sub_carlitos<-subset(new_training,user_name=="carlitos")
sub_carlitos_clean<-sub_carlitos[,-c(1,2,5)]

sub_pedro<-subset(new_training,user_name=="pedro")
sub_pedro_clean<-sub_pedro[,-c(1,2,5)]

sub_jeremy<-subset(new_training,user_name=="jeremy")
sub_jeremy_clean<-sub_jeremy[,-c(1,2,5)]

sub_adelmo<-subset(new_training,user_name=="adelmo")
sub_adelmo_clean<-sub_adelmo[,-c(1,2,5)]

sub_eurico<-subset(new_training,user_name=="eurico")
sub_eurico_clean<-sub_eurico[,-c(1,2,5)]

sub_charles<-subset(new_training,user_name=="charles")
sub_charles_clean<-sub_charles[,-c(1,2,5)]
```

Variable selection before prediction
================================================

1. I observe that "raw_timestamp_part_1" has nearly 100% prediction power of  in all the samples


```r
qplot(classe,raw_timestamp_part_1,data=sub_charles_clean,geom="boxplot")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
qplot(classe,raw_timestamp_part_1,data=sub_eurico_clean,geom="boxplot")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-2.png) 

```r
qplot(classe,raw_timestamp_part_1,data=sub_adelmo_clean,geom="boxplot")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-3.png) 

```r
qplot(classe,raw_timestamp_part_1,data=sub_carlitos_clean,geom="boxplot")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-4.png) 

```r
qplot(classe,raw_timestamp_part_1,data=sub_pedro_clean,geom="boxplot")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-5.png) 

```r
qplot(classe,raw_timestamp_part_1,data=sub_jeremy_clean,geom="boxplot")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-6.png) 


2.base on the ground of point 1, it is very strange. I dont believe this variable is the only variable to decide the classe. But I believe this variable is highly correlate to the classe

I choose the variable which has correlation (>0.2) with "raw_timestamp_part_1", and use these variables for random forest predict model.

```r
cor_value_carlitos<-apply(sub_carlitos_clean[,-c(1,dim(sub_carlitos_clean)[2])],2,function(x) cor(as.numeric(x), sub_carlitos_clean$raw_timestamp_part_1,method="spearman"))
cor_value_eurico<-apply(sub_eurico_clean[,-c(1,dim(sub_eurico_clean)[2])],2,function(x) cor(as.numeric(x), sub_eurico_clean$raw_timestamp_part_1,method="spearman"))
cor_value_adelmo<-apply(sub_adelmo_clean[,-c(1,dim(sub_adelmo_clean)[2])],2,function(x) cor(as.numeric(x), sub_adelmo_clean$raw_timestamp_part_1,method="spearman"))
```

```
## Warning in cor(as.numeric(x), sub_adelmo_clean$raw_timestamp_part_1,
## method = "spearman"): the standard deviation is zero
```

```
## Warning in cor(as.numeric(x), sub_adelmo_clean$raw_timestamp_part_1,
## method = "spearman"): the standard deviation is zero
```

```
## Warning in cor(as.numeric(x), sub_adelmo_clean$raw_timestamp_part_1,
## method = "spearman"): the standard deviation is zero
```

```r
cor_value_charles<-apply(sub_charles_clean[,-c(1,dim(sub_charles_clean)[2])],2,function(x) cor(as.numeric(x), sub_charles_clean$raw_timestamp_part_1,method="spearman"))
cor_value_pedro<-apply(sub_pedro_clean[,-c(1,dim(sub_pedro_clean)[2])],2,function(x) cor(as.numeric(x), sub_pedro_clean$raw_timestamp_part_1,method="spearman"))

cor_value_jeremy<-apply(sub_jeremy_clean[,-c(1,dim(sub_jeremy_clean)[2])],2,function(x) cor(as.numeric(x), sub_jeremy_clean$raw_timestamp_part_1,method="spearman",use="complete.obs"))
```

```
## Warning in cor(as.numeric(x), sub_jeremy_clean$raw_timestamp_part_1,
## method = "spearman", : the standard deviation is zero
```

```
## Warning in cor(as.numeric(x), sub_jeremy_clean$raw_timestamp_part_1,
## method = "spearman", : the standard deviation is zero
```

```
## Warning in cor(as.numeric(x), sub_jeremy_clean$raw_timestamp_part_1,
## method = "spearman", : the standard deviation is zero
```

```r
training_carlitos<-sub_carlitos_clean[which(abs(cor_value_carlitos)>0.3)]
training_carlitos<-cbind(training_carlitos,classe=sub_carlitos_clean$classe)
dim(training_carlitos)
```

```
## [1] 3056   15
```

```r
training_eurico<-sub_eurico_clean[which(abs(cor_value_eurico)>0.3)]
training_eurico<-cbind(training_eurico,classe=sub_eurico_clean$classe)
dim(training_eurico)
```

```
## [1] 3016    8
```

```r
training_adelmo<-sub_adelmo_clean[which(abs(cor_value_adelmo)>0.1)]
training_adelmo<-cbind(training_adelmo,classe=sub_adelmo_clean$classe)
dim(training_adelmo)
```

```
## [1] 3809   31
```

```r
training_charles<-sub_charles_clean[which(abs(cor_value_charles)>0.1)]
training_charles<-cbind(training_charles,classe=sub_charles_clean$classe)
dim(training_charles)
```

```
## [1] 3455   35
```

```r
training_pedro<-sub_pedro_clean[which(abs(cor_value_pedro)>0.1)]
training_pedro<-cbind(training_pedro,classe=sub_pedro_clean$classe)
head(training_pedro)
```

```
##     roll_belt yaw_belt accel_belt_x accel_belt_z yaw_arm gyros_arm_z
## 166       129     1.63          -43         -188     151       -0.11
## 167       129     1.53          -45         -185     151       -0.10
## 168       129     1.07          -42         -184     150       -0.08
## 169       129     0.95          -44         -187     150       -0.07
## 170       129     0.87          -42         -188     150       -0.05
## 171       129     0.78          -41         -189     150       -0.05
##     accel_arm_y accel_arm_z magnet_arm_x magnet_arm_z roll_dumbbell
## 166         -76         -87         -432          460     -78.49260
## 167         -77         -86         -427          461     -93.11186
## 168         -75         -87         -432          454    -101.60582
## 169         -76         -87         -428          450     -76.76639
## 170         -77         -86         -426          455     -76.87968
## 171         -75         -88         -430          450     -63.19692
##     pitch_dumbbell yaw_dumbbell gyros_dumbbell_z accel_dumbbell_x
## 166     -62.347002     41.09499            -0.10              -19
## 167     -36.078286     50.22204            -0.03              -11
## 168     -21.972226     50.00949            -0.05              -10
## 169      -8.179190     79.64546            -0.03               -4
## 170       7.732938     79.60293            -0.10                4
## 171      37.601149     80.15876            -0.15               23
##     accel_dumbbell_y accel_dumbbell_z magnet_dumbbell_x magnet_dumbbell_y
## 166              -23               13               499              -562
## 167              -25               15               494              -560
## 168              -39               22               495              -557
## 169              -34               35               499              -569
## 170              -36               37               493              -561
## 171              -37               45               497              -567
##     magnet_dumbbell_z yaw_forearm gyros_forearm_z accel_forearm_z
## 166              -155         108           -0.02            -198
## 167              -151         108           -0.05            -197
## 168              -137         108            0.00            -198
## 169              -123         108           -0.05            -196
## 170              -124         108           -0.08            -198
## 171              -113         107           -0.11            -193
##     magnet_forearm_x magnet_forearm_y classe
## 166             -139              741      A
## 167             -144              747      A
## 168             -146              744      A
## 169             -139              742      A
## 170             -145              749      A
## 171             -143              750      A
```

```r
training_jeremy<-sub_jeremy_clean[which(abs(cor_value_jeremy)>0.1)]
training_jeremy<-cbind(training_jeremy,classe=sub_jeremy_clean$classe)
dim(training_jeremy)
```

```
## [1] 3325   30
```

Prediction
========================================


```r
library(caret)
```

```
## Loading required package: lattice
```

```r
set.seed(3456)
trainIndex <- createDataPartition(training_pedro$classe, p = .8,list=F)
class(training_pedro[,3])
```

```
## [1] "integer"
```

```r
tran1<-training_pedro[trainIndex,]
test1<-training_pedro[-trainIndex,]
modFit_pedro<-train(classe~.,data=tran1,method="rf")
```

```
## Loading required package: randomForest
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

```r
pred <- predict(modFit,test1)
```

```
## Error in predict(modFit, test1): 找不到物件 'modFit'
```

```r
a<-table(pred,test1$classe)
```

```
## Error in table(pred, test1$classe): 找不到物件 'pred'
```

```r
sum(diag(a))/sum(a)
```

```
## Error in diag(a): 找不到物件 'a'
```

```r
trainIndex <- createDataPartition(training_jeremy$classe, p = .8,list=F)
tran1<-training_jeremy[trainIndex,]
test1<-training_jeremy[-trainIndex,]
modFit_jeremy<-train(classe~.,data=tran1,method="rf")
pred <- predict(modFit_jeremy,test1)
table(pred,test1$classe)
```

```
##     
## pred   A   B   C   D   E
##    A 231   0   0   0   0
##    B   0  95   2   0   0
##    C   0   0 124   3   0
##    D   0   0   1  98   0
##    E   0   0   0   0 110
```

```r
trainIndex <- createDataPartition(training_charles$classe, p = .8,list=F)
tran1<-training_charles[trainIndex,]
test1<-training_charles[-trainIndex,]
modFit_charles<-train(classe~.,data=tran1,method="rf")

trainIndex <- createDataPartition(training_carlitos$classe, p = .8,list=F)
tran1<-training_carlitos[trainIndex,]
test1<-training_carlitos[-trainIndex,]
modFit_carlitos<-train(classe~.,data=tran1,method="rf")
pred <- predict(modFit_carlitos,test1)
table(pred,test1$classe)
```

```
##     
## pred   A   B   C   D   E
##    A 164   0   0   0   0
##    B   0 132   0   0   0
##    C   0   2  97   1   3
##    D   0   0   0  94   0
##    E   0   0   0   0 116
```

```r
trainIndex <- createDataPartition(training_eurico$classe, p = .8,list=F)
tran1<-training_eurico[trainIndex,]
test1<-training_eurico[-trainIndex,]
modFit_eurico<-train(classe~.,data=tran1,method="rf")
pred <- predict(modFit_eurico,test1)
table(pred,test1$classe)
```

```
##     
## pred   A   B   C   D   E
##    A 165   0   4   1   1
##    B   1 115   2   2   0
##    C   1   0  88   2   4
##    D   0   0   0 109   1
##    E   2   1   2   0 100
```

```r
trainIndex <- createDataPartition(training_adelmo$classe, p = .8,list=F)
tran1<-training_adelmo[trainIndex,]
test1<-training_adelmo[-trainIndex,]
modFit_adelmo<-train(classe~.,data=tran1,method="rf")
pred <- predict(modFit_adelmo,test1)
table(pred,test1$classe)
```

```
##     
## pred   A   B   C   D   E
##    A 228   1   0   0   0
##    B   0 151   0   0   0
##    C   0   0 146   0   0
##    D   0   0   1 101   2
##    E   0   0   0   0 130
```

predict the test data

```r
pred_classe<-NULL
for(i in 1:20){
  if (test$user_name[i]=="pedro"){
    pred_classe[i]=predict(modFit_pedro,test[i,])
  }
  else if (test$user_name[i]=="adelmo"){
    pred_classe[i]=predict(modFit_adelmo,test[i,])
  }
  else if (test$user_name[i]=="eurico"){
    pred_classe[i]=predict(modFit_eurico,test[i,])
  }
  else if (test$user_name[i]=="carlitos"){
    pred_classe[i]=predict(modFit_carlitos,test[i,])
  }
  else if (test$user_name[i]=="charles"){
    pred_classe[i]=predict(modFit_charles,test[i,])
  }
  else if (test$user_name[i]=="jeremy"){
    pred_classe[i]=predict(modFit_jeremy,test[i,])
  }
}

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(pred_classe)
```
