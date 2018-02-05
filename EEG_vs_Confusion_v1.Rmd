---
title: "EEG_vs_Confusion_v1"
author: "Adithya Ajith"
date: "1 November 2016"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
eeg_dta %>% 
group_by(subject.ID) %>% 
summarise_each(funs(mean))

```{r Load_Libraries, message=FALSE, warning=FALSE, include=FALSE}
library(needs)
needs(dplyr, e1071 , formattable , knitr , corrplot , ggfortify , ROCR , randomForest , boot, FSelector)

```

```{r Load_original_data}

eeg_dta = read.csv("EEG data.csv")

eeg_dta =eeg_dta %>% 
  filter( subject.ID != 6 )

if(T)
  {eeg_dta$subject.ID[eeg_dta$subject.ID == 9] = 6

eeg_dta =eeg_dta %>% 
  arrange(subject.ID)}


if(T)   # Put T for normalising
{vars = names(eeg_dta)[-c(1,2, 3 , 4, 14,15)]
eeg_dta = eeg_dta %>% 
  group_by( subject.ID) %>% 
  mutate_each_(funs(normalize),vars)
rm(vars)
#range(eeg_dta$Raw[eeg_dta$subject.ID==2])
}


wang =eeg_dta %>% 
     group_by( subject.ID ,Video.ID) %>% 
    summarise( Attention = mean(Attention),
               Meditation = mean(Meditation),
               Raw = mean(Raw),
               Delta = mean(Delta),
               Theta =mean(Theta),
               Alpha.1 =mean(Alpha.1),
               Alpha.2 =mean(Alpha.2),
               Beta.1  =mean(Beta.1) ,
               Beta.2 =mean(Beta.2),
               Gamma1 =mean( Gamma1) ,
               Gamma2 =mean(Gamma2),
               predefined.label = factor(mean(predefined.label)),
               Self.defined.label = factor(mean(Self.defined.label)))
# wang$Alpha.1 = log(wang$Alpha.1)
# wang[, c(3 , 4, 14 ) ]  <- NULL

#variance
wang =eeg_dta %>% 
     group_by( subject.ID ,Video.ID) %>% 
    summarise( Attention = var(Attention), 
               Meditation = var(Meditation),
               Raw = var(Raw),
               Delta = var(Delta),
               Theta = var(Theta),
               Alpha.1 = var(Alpha.1),
               Alpha.2 = var(Alpha.2),
               Beta.1  = var(Beta.1) ,
               Beta.2 = var(Beta.2),
               Gamma1 = var( Gamma1) ,
               Gamma2 = var(Gamma2),
               Self.defined.label = factor(mean(Self.defined.label)))

set.seed(1)
wang  <-  wang[sample(nrow(wang)),]


```

```{r plots}

par(mfrow=c(3,3))
plot(Raw~ Self.defined.label , data=wang , main= "Raw" , xlab ="Confusion")
plot(Delta~ Self.defined.label , data=wang , main= "Delta" , xlab ="Confusion")
plot(Theta~ Self.defined.label , data=wang , main= "Theta" , xlab ="Confusion")
plot(Alpha.1~ Self.defined.label , data=wang , main ="Alpha.1", xlab ="Confusion")
plot(Alpha.2~ Self.defined.label , data=wang, main = "Alpha.2", xlab ="Confusion")
plot(Beta.1~ Self.defined.label , data=wang, main="Beta.1", xlab ="Confusion")
plot(Beta.2~ Self.defined.label , data=wang, main="Beta.2", xlab ="Confusion")
plot(Gamma1~ Self.defined.label , data=wang, main="Gamma1", xlab ="Confusion")
plot(Gamma2~ Self.defined.label , data=wang, main="Gamma2",xlab ="Confusion")

a= c(0:5,7:9)
for(i in a)
    plot(Theta ~ Self.defined.label , data=wang[wang$subject.ID==i, ],xlab ="Confusion", main=paste("subject.ID", i))

for(i in a)
    plot(Alpha.1 ~ Self.defined.label , data=wang[wang$subject.ID==i, ],xlab ="Confusion", main=paste("subject.ID", i))

for(i in a)
    plot(Alpha.2 ~ Self.defined.label , add= T,data=wang[wang$subject.ID==i, ],xlab ="Confusion", main=paste("subject.ID", i))

for(i in a)
    plot(Beta.1 ~ Self.defined.label , data=wang[wang$subject.ID==i, ],xlab ="Confusion", main=paste("subject.ID", i))

plot(eeg_dta$Theta[eeg_dta$subject.ID[eeg_dta$Self.defined.label==0] == 0], type='l')
plot(eeg_dta$Theta[eeg_dta$subject.ID[eeg_dta$Self.defined.label==1] == 0], type='l')


```



```{r naiveBayes}

class_accu = numeric(9)
for(i in 0:8){
  nb = naiveBayes( Self.defined.label ~ Attention + Meditation + Theta + Alpha.1 + Alpha.2 + Delta + Beta.1 + Beta.2+ Gamma1 + Gamma2 + Raw, data = wang  , subset= wang$subject.ID != i)
  
   p = predict(nb , wang[ wang$subject.ID == i,] , type = "class")
  class_accu[i+1] = mean(p == wang$Self.defined.label[ wang$subject.ID == i ])
  
}
class_accu
mean(class_accu)

class_accu = numeric(9)
for(i in 0:8){
  nb = naiveBayes( predefined.label ~ Attention + Meditation + Theta + Alpha.1 + Alpha.2 + Delta + Beta.1 + Beta.2+ Gamma1 + Gamma2 + Raw, data = wang  , subset= wang$subject.ID != i)
  
   p = predict(nb , wang[ wang$subject.ID == i,] , type = "class")
  class_accu[i+1] = mean(p == wang$Self.defined.label[ wang$subject.ID == i ])
  
}
class_accu
mean(class_accu)

class_accu = numeric(9)
for(i in 0:8){
  nb = naiveBayes( Self.defined.label ~ Theta + Alpha.1 + Alpha.2, data = wang  , subset= wang$subject.ID != i)
  
   p = predict(nb , wang[ wang$subject.ID == i,] , type = "class")
  class_accu[i+1] = mean(p == wang$Self.defined.label[ wang$subject.ID == i ])
  
}
class_accu
mean(class_accu)



class_accu = numeric(9)
for(i in 0:8){
  nb = naiveBayes( Self.defined.label ~  Theta + Alpha.1 + Alpha.2, data = wang  , subset= wang$subject.ID != i)
   p = predict(nb , wang[ wang$subject.ID == i,] , type = "class")
  class_accu[i+1] = mean(p == wang$Self.defined.label[ wang$subject.ID == i ])
}
class_accu
mean(class_accu)

class_accu = numeric(9)
for(i in 0:8){
  nb = naiveBayes( Self.defined.label ~  Theta + Alpha.1 + Alpha.2, data = wang  , subset= wang$subject.ID != i)
   p = predict(nb , wang[ wang$subject.ID == i,] , type = "class")
  class_accu[i+1] = mean(p == wang$Self.defined.label[ wang$subject.ID == i ])
}
class_accu
mean(class_accu)

# Student specific
if(F){
class_accu = numeric(9)
vid_accu = numeric(10)
for(i in 0:8){
  for(j in 0:9){
    sang = wang %>% filter(subject.ID == i)
  nb = naiveBayes( Self.defined.label ~  Theta + Alpha.1 + Alpha.2, data = sang  , subset= sang$Video.ID !=j)
   p = predict(nb , sang[ sang$Video.ID ==j,] , type = "class")
  vid_accu[j+1] = mean(p == sang$Self.defined.label[ sang$Video.ID ==j ])
  }
  class_accu[i+1]=mean(vid_accu)
}
class_accu
mean(class_accu)}

#plot(class_accu, type= "l" , ylim = c(0:1))

```

```{r cross validation}
#set.seed(1)
#wang<-wang[sample(nrow(wang)),]

par(mfrow=c(2,2))
qqnorm(wang$Theta , main ="Theta"); qqline(wang$Theta)
qqnorm(wang$Alpha.2 , main = "Alpha.2"); qqline(wang$Alpha.2)
qqnorm(wang$Alpha.1 , main = "Alpha.1"); qqline(wang$Alpha.1)
qqnorm(log(wang$Alpha.1) , main = "Log transform of Alpha.1"); qqline(log(wang$Alpha.1))


#Create 10 equally size folds
folds <- cut(seq(1,nrow(wang)),breaks=10,labels=FALSE)
class_accu = numeric(10)

#Perform 10 fold cross validation
for(i in 1:10){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    nb = naiveBayes( Self.defined.label ~  Theta + Alpha.1 + Alpha.2, data = wang[-testIndexes, ])
    p = predict(nb , wang[ testIndexes,] , type = "class")
    class_accu[i] = mean(p == wang$Self.defined.label[ testIndexes ])
    
}
class_accu
mean(class_accu)

#plot()
```


```{r LDA }
library(MASS)
class_accu = numeric(9)
for(i in 0:8){
      nb = lda( Self.defined.label ~  Delta +Theta + Alpha.1+ Alpha.2  +Beta.1+ Beta.2  + Gamma1  + Gamma2 , data = wang  , subset= wang$subject.ID != i)
 
   p = predict(nb , wang[ wang$subject.ID == i,c(-1,-2,-14,-15)] , type = "class")
  class_accu[i+1] = mean(p$class == wang$Self.defined.label[ wang$subject.ID == i ])
  
}
class_accu
mean(class_accu)

class_accu = numeric(9)
for(i in 0:8){
      nb = lda( Self.defined.label ~  Theta + Alpha.1+ Alpha.2, data = wang  , subset= wang$subject.ID != i)
 
   p = predict(nb , wang[ wang$subject.ID == i,c(-1,-2,-14,-15)] , type = "class")
  class_accu[i+1] = mean(p$class == wang$Self.defined.label[ wang$subject.ID == i ])
  
}
detach(package:MASS)
class_accu
mean(class_accu)

```

```{r qda }
library(MASS)
class_accu = numeric(9)
for(i in 0:8){
  nb = qda( Self.defined.label ~  Delta +  Theta + Alpha.1+ Alpha.2  +Beta.1+ Beta.2  + Gamma1  + Gamma2 , data = wang  , subset= wang$subject.ID != i)
  
   p = predict(nb , wang[ wang$subject.ID == i,c(-1,-2,-14,-15)] , type = "class")
  class_accu[i+1] = mean(p$class == wang$Self.defined.label[ wang$subject.ID == i ])
  
}
class_accu
mean(class_accu)

class_accu = numeric(9)
for(i in 0:8){
  nb = qda( Self.defined.label ~ Theta + Alpha.1+ Alpha.2, data = wang  , subset= wang$subject.ID != i)
  
   p = predict(nb , wang[ wang$subject.ID == i,c(-1,-2,-14,-15)] , type = "class")
  class_accu[i+1] = mean(p$class == wang$Self.defined.label[ wang$subject.ID == i ])
  
}
detach(package:MASS)
class_accu
mean(class_accu)

```

```{r logistic_regression }

class_accu = numeric(9)
for(i in 0:8){
  nb = glm( Self.defined.label ~  Delta +  Theta + Alpha.1+ Alpha.2  +Beta.1+ Beta.2  + Gamma1  + Gamma2 , data = wang  , subset= wang$subject.ID != i , family = "binomial")
  
   p = predict(nb , wang[ wang$subject.ID == i,c(-1,-2,-14,-15)] , type = "response")
   p = ifelse(p>0.5 , 1 , 0)
  class_accu[i+1] = mean(p == wang$Self.defined.label[ wang$subject.ID == i ])
  
}
class_accu
mean(class_accu)

class_accu = numeric(9)
for(i in 0:8){
  nb = glm( Self.defined.label ~  Theta + Alpha.1+ Alpha.2, data = wang  , subset= wang$subject.ID != i , family = "binomial")
  
   p = predict(nb , wang[ wang$subject.ID == i,c(-1,-2,-14,-15)] , type = "response")
   p = ifelse(p>0.5 , 1 , 0)
  class_accu[i+1] = mean(p == wang$Self.defined.label[ wang$subject.ID == i ])
  }
class_accu
mean(class_accu)

nb = glm( Self.defined.label ~  Theta + Alpha.1+ Alpha.2, data = wang  , family = "binomial")
cv.error= cv.glm(wang, nb , K=10)
```

```{r KNN }

if(F){
k_mean = numeric(20)
for(k in 1:20){
for(i in 0:8){
  
  nb =class::knn(wang[wang$subject.ID != i,c(5:13)],wang[wang$subject.ID == i,c(5:13)] ,wang$Self.defined.label[ wang$subject.ID != i ] ,k) 
table(nb , wang$Self.defined.label[ wang$subject.ID == i ])
  
  class_accu[i+1] = mean(nb == wang$Self.defined.label[ wang$subject.ID == i ])
  }
class_accu
k_mean[k]=mean(class_accu)}
which.max(k_mean)
}

k=7
class_accu = numeric(9)

for(i in 0:8){
  
  nb =class::knn(wang[wang$subject.ID != i,c(5:13)],wang[wang$subject.ID == i,c(5:13)] ,wang$Self.defined.label[ wang$subject.ID != i ] ,k) 
table(nb , wang$Self.defined.label[ wang$subject.ID == i ])
  
  class_accu[i+1] = mean(nb == wang$Self.defined.label[ wang$subject.ID == i ])
  }
class_accu
mean(class_accu)



k_mean = numeric(20)
for(k in 1:10){
class_accu = numeric(9)
  for(i in 0:8){
    nb =class::knn(wang[wang$subject.ID != i,c(7:9)],wang[wang$subject.ID == i,c(7:9)] ,wang$Self.defined.label[ wang$subject.ID != i ] ,k) 
table(nb , wang$Self.defined.label[ wang$subject.ID == i ])
  
  class_accu[i+1] = mean(nb == wang$Self.defined.label[ wang$subject.ID == i ])
  
}
class_accu
k_mean[k]=mean(class_accu)}
which.max(k_mean)
```

```{r SVM}
if(F){
set.seed (1)
tune.out=tune(svm , Self.defined.label ~ Raw  + Delta +  Theta + Alpha.1+ Alpha.2  +Beta.1+ Beta.2  + Gamma1  + Gamma2 , data = wang , kernel ="radial",ranges =list(cost=c(0.1 ,1 ,10 , 80 , 90 , 110,120 ,100),gamma=c(0.001,0.1,0.2,0.3,0.4,0.5,1) ))
summary(tune.out)}

class_accu = numeric(9)
for(i in 0:8){
  set.seed(101)
tune.out=tune(svm , Self.defined.label ~ Raw  + Delta +  Theta + Alpha.1+ Alpha.2  +Beta.1+ Beta.2  + Gamma1  + Gamma2 , data = wang[wang$subject.ID != i,] , kernel ="radial",ranges =list(cost=c(0.1 ,1 ,10 , 80 , 90 , 110,120 ,100),gamma=c(0.001,0.1,0.2,0.3,0.4,0.5,1) ))
p = predict(tune.out$best.model, newdata = wang[ wang$subject.ID == i,])
class_accu[i+1] = mean(p == wang$Self.defined.label[ wang$subject.ID == i ])

}
class_accu
mean(class_accu)

class_accu = numeric(9)
for(i in 0:8){
  set.seed(101)
tune.out=tune(svm , Self.defined.label ~ Theta + Alpha.1+ Alpha.2, data = wang[wang$subject.ID != i,] , kernel ="radial",ranges =list(cost=c(0.1 ,1 ,10 ,80, 90,110,120,100),gamma=c(0.001,0.1,0.2,0.3,0.4,0.5,1) ))
p = predict(tune.out$best.model, newdata = wang[ wang$subject.ID == i,])
class_accu[i+1] = mean(p == wang$Self.defined.label[ wang$subject.ID == i ])

}
class_accu
mean(class_accu)

```

```{r Random_forest}

set.seed(1)
class_accu = numeric(9)
k=3
for(i in 0:8){
forest = randomForest(Self.defined.label ~ Raw  + Delta +  Theta + Alpha.1+ Alpha.2  +Beta.1+ Beta.2  + Gamma1  + Gamma2 , data = wang[wang$subject.ID != i,] , mtry=k, importance =TRUE)
p= predict(forest , newdata = wang[wang$subject.ID == i,])
class_accu[i] = mean(p == wang$Self.defined.label[ wang$subject.ID == i ])
}
mean(class_accu)

set.seed(1)
k=3
class_accu = numeric(9)
for(i in 0:8){
forest = randomForest(Self.defined.label ~ Theta +Alpha.1+Alpha.2, data = wang[wang$subject.ID != i,] , mtry=k)
p= predict(forest , newdata = wang[wang$subject.ID == i,])
class_accu[i] = mean(p == wang$Self.defined.label[ wang$subject.ID == i ])
}
mean(class_accu)

```

```{r ROCR}

ROCRplot =function(probabilty , test, ...){
predob = prediction(probabilty , wang$Self.defined.label[test])
perf = performance(predob , measure = "tpr", x.measure = "fpr")
auc= performance(predob, measure = "auc")
#plot(perf,...)
#abline(a=0, b= 1, lty=3)
#print(paste( "AUC for", ..., round(auc@y.values[[1]],3)))
return(round(auc@y.values[[1]],3))
}

#set.seed(1)
#wang<-wang[sample(nrow(wang)),]

folds <- cut(seq(1,nrow(wang)),breaks=10,labels=FALSE)
AUC = numeric(10)

#Perform 10 fold cross validation
for(i in 1:10){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    nb = naiveBayes( Self.defined.label ~  Theta + Alpha.1 +Alpha.2, data = wang[-testIndexes, ])
    p = predict(nb , wang[ testIndexes,] , type = "raw")
    AUC[i] = ROCRplot(p[,2] ,testIndexes  )
    
}
AUC
mean( AUC )


```

```{r kernal_2 Cleaning and plotting EEG signals}

eeg_dta$Sample <-
  ave( 1:nrow(eeg_dta), eeg_dta$subject.ID, factor(eeg_dta$Video.ID), FUN=function(x) 1:length(x))

p1 <- ggplot(eeg_dta, aes(x=Sample, y=Theta, colour=as.factor(subject.ID)))
p1 + geom_line()  +scale_colour_brewer(name="Subject.ID")
, palette = "Set3"

for (i in unique(eeg_dta$subject.ID)) {
  for (j in unique(eeg_dta$Video.ID)) {
    subject_df <- subset(eeg_dta, subject.ID == i & Video.ID == j)
    print(paste("Participant", i, "-", "Video", j))
    plot(subject_df$Raw, type = 'l', ylim = c(-200, 200), xlab = "Sample", ylab = "mV")
  }
}
  i =6 
    for (j in unique(eeg_dta$Video.ID)) {
      subject_df <- subset(eeg_dta, subject.ID == i & Video.ID == j)
      print(paste("Participant", i, "-", "Video", j))
      plot(subject_df$Raw, type = 'l', ylim = c(-200, 200), xlab = "Sample", ylab = "mV")
    }


```

```{r Kernal_3 spikes in theta}
r <- read.csv('EEG data.csv',header=T)
plotR <- function(s,v) {
   om <- mean(r[r[,1]!=6,7])
   rall <- r[r[,1]==s& r[,2]==v,7:9]
   #r0 <- r[r[,1]==s& r[,2]==v,7]
   r0 <- rall[,1]
   ra1 <- r[r[,1]==s& r[,2]==v,8]
   #ralpha2 <- r[r[,1]==s& r[,2]==v,9]
   m <- round(mean(r0),-3)
   sdv <- round(sd(r0),-2)
   p <- round(length(r0[r0>=om])/length(r0),2)
   p2 <- round(length(r0[r0>=m])/length(r0),2)
   sdl <- mean(r[r[,1]==s&r[,2]==v,15])
   pdl <- mean(r[r[,1]==s&r[,2]==v,14])
   t <- paste("SD=",sdl," PD=",pdl," P=",p,"\nMean=",m," Sdev=",sdv,"\n Pown=",p2)
   t2 <- paste("Subject:",s," Video:",v)
   plot(r0,type='l',ylim=c(0,1500000),xlab='',main=t,sub=t2)
   lines(ra1,col='red',lwd=1)
   #lines(ralpha2,col='blue',lwd=2)
   abline(h=mean(r[r[,1]==s&r[,2]==v,7]))
}

plotSubject <- function(s) {
    par(mar=c(5,2,4,2))
    par(mfrow=c(4,3))
    #par(oma=c(0,0,2,0))
    ttl <- paste('Subject:',s)
    for (i in 0:9) plotR(s,i)
}  
    
for (s in c(0:5,7:9))  plotSubject(s)

```

```{r muzza_on_action}

wang_theta = NULL
for (i in 0:8)
  for(j in 0:9){
m=eeg_dta %>% filter( eeg_dta$subject.ID == i , eeg_dta$Video.ID ==j) %>% select(Theta)
m=mean(m$Theta)

theta = eeg_dta %>% 
  select( subject.ID ,Video.ID,Theta, Self.defined.label) %>% 
  filter( eeg_dta$subject.ID == i , eeg_dta$Video.ID ==j) %>% 
       mutate(Theta_trans = Theta - (1.2*m))
wang_theta =rbind(wang_theta , theta)
  }


wang_theta =wang_theta %>% 
  group_by( subject.ID ,Video.ID) %>%
  filter(Theta_trans>0) %>% 
  summarise(Theta = n(),
            Self.defined.label = factor(mean(Self.defined.label)))

#wang_theta$Theta_trans=ifelse(wang_theta$Theta_trans>0,wang_theta$Theta_trans,0)

wang_theta_sum =wang_theta %>% 
     group_by( subject.ID ,Video.ID) %>%
      summarise( Theta=mean(Theta_trans),
                              Self.defined.label = factor(mean(Self.defined.label)))

wang = wang_theta


```

```{r trash}
if(F){sub_6 = eeg_dta %>% 
  select(-Video.ID , -predefined.label ,-Self.defined.label ) %>% 
group_by(subject.ID) %>% 
summarise_each(funs(mean))
write.csv(file="file.csv", x=sub_6 , row.names = FALSE)}

work =eeg_dta %>% 
  select( subject.ID ,Video.ID ) %>%  
  group_by( subject.ID ,Video.ID) %>% 
    summarise(count = n())

  nb = naiveBayes( wang[ wang$subject.ID != i ,c(-1,-2,-14,-15)] , wang[wang$subject.ID != i , 15])
  
  contrasts(wang$Self.defined.label)
nb = naiveBayes( Self.defined.label ~  Attention  +      Meditation    + Raw            +    Delta   +         
 Theta           +   Alpha.1    +       Alpha.2  +      Beta.1     +        Beta.2      +    Gamma1  +         
Gamma2 , data = wang )


p = predict(nb , wang[,c(-1,-2,-14,-15)])
table(p,wang$Self.defined.label)
mean(p == wang$Self.defined.label)


```

```{r DWT}
sc = read.table("synthetic_control.data", header=F, sep= "")
wtData = NULL
for (i in 1:nrow(sc)) {
a <- t(sc[i,])
wt <- dwt(a, filter="haar", boundary="periodic")
wtData <- rbind(wtData, unlist(c(wt@W,wt@V[[wt@level]])))
}
wtData <- as.data.frame(wtData)
classId <- c(rep("1",100), rep("2",100), rep("3",100), rep("4",100), rep("5",100), rep("6",100))
wtSc <- data.frame(cbind(classId, wtData))
#wtSc <- data.frame(cbind(classId, sc))

library(party)
ct <- ctree(classId ~ ., data=wtSc,controls = ctree_control(minsplit=30, minbucket=10,maxdepth=5))
pClassId <- predict(ct)
table(classId, pClassId)
(sum(classId==pClassId)) / nrow(wtSc)

```

```{r kernel_1 dimension-reduction-of-eeg-data}
demography<-read.csv("demographic info.csv")

#EEG Data Exploring 
#Positive correlations are displayed in blue and negative correlations in red color. Color intensity and the size of the circle are proportional to the correlation coefficients.

#correlation<-cor(eeg_dta)
correlation<-cor(wang[,c(3:13)])
corrplot(correlation, type="lower")

#Add significance level to the correlogram
#Correlations with p-value > 0.01 are considered as insignificant. In this case the correlation coefficient values are leaved blank or crosses are added.

cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], ...)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
    }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
corrplot(correlation, type="lower", p.mat = cor.mtest(eeg_dta), sig.level = 0.01)


#PCA
pca <- prcomp(eeg_dta, center = TRUE, scale. = TRUE) 
print(pca)
plot(pca, type = "l")
summary(pca)

eeg$subject.ID<-as.factor(eeg$subject.ID)
eeg$predefined.label<-as.factor(eeg$predefined.label)
eeg$Self.defined.label<-as.factor(eeg$Self.defined.label)

autoplot(pca, data =eeg, colour ='Self.defined.label', alpha=I(0.4))
autoplot(pca, data =eeg, colour ='predefined.label', alpha=I(0.4), loadings = TRUE, loadings.label = TRUE, loadings.colour = 'blue4', loadings.label.colour = 'darkgreen')
autoplot(pca, data =eeg, colour ='subject.ID', alpha=I(0.3))

pca <- prcomp(eeg_dta[eeg_dta$Video.ID[eeg_dta$subject.ID == 0] ==0 ,3:13 ], center = TRUE, scale. = TRUE)

```

```{r DWT with theta}

wtData = NULL
for (i in 0:8)
  for(j in c(0:9)){
  cat(i,j,",")
  a= eeg_dta %>% 
filter( eeg_dta$subject.ID ==i , eeg_dta$Video.ID ==j) %>% 
select(Theta)
a=matrix(t(a))

wt <- dwt(a, filter="haar", boundary="periodic" , fast)
wtData <- rbind(wtData, unlist(c(wt@W,wt@V[[wt@level]])))
}
```

```{r normalize_data}
vars = names(eeg_dta)[-c(1,2, 3 , 4, 14,15)]
wang_normalized = data.frame(matrix(ncol = 15, nrow = 0))
colnames(wang_normalized) = names(eeg_dta)
for( i  in 0:8)
  for(j in 0:9 ){
        sang = eeg_dta %>% 
      filter( subject.ID == i,Video.ID == j) %>% 
      mutate_each_(funs(normalize),vars)
        wang_normalized = rbind(wang_normalized ,sang )
  }

wang_normalized = eeg_dta %>% 
  group_by( subject.ID) %>% 
  mutate_each_(funs(normalize),vars)

vars = names(eeg_dta)[-c(1,2, 3 , 4, 14,15)]
wang_normalized = data.frame(matrix(ncol = 15, nrow = 0))
colnames(wang_normalized) = names(eeg_dta)
for( i  in 0:8)
  for(j in 0:9 ){
        sang = eeg_dta %>% 
      filter( subject.ID == i,Video.ID == j) %>% 
      mutate_each_(funs(normalize),vars)
        wang_normalized = rbind(wang_normalized ,sang )
  }

wang =wang_normalized %>% 
     group_by( subject.ID ,Video.ID) %>% 
    summarise( Attention = mean(Attention), 
               Meditation =mean(Meditation),
               Raw = mean(Raw),
               Delta = mean(Delta),
               Theta =mean(Theta),
               Alpha.1 =mean(Alpha.1),
               Alpha.2 =mean(Alpha.2),
               Beta.1  =mean(Beta.1) ,
               Beta.2 =mean(Beta.2),
               Gamma1 =mean( Gamma1) ,
               Gamma2 =mean(Gamma2),
               predefined.label =mean(predefined.label),
               Self.defined.label = factor(mean(Self.defined.label)))
rm(sang , vars , wang_normalized , i, j)

```


```{r HFES}

eeg_dta = read.csv("EEG data.csv")
eeg_dta =eeg_dta %>%   filter( subject.ID != 6 )
eeg_dta$subject.ID[eeg_dta$subject.ID == 9] = 6
eeg_dta =eeg_dta %>%   arrange(subject.ID)

vars = names(eeg_dta)[-c(1,2, 3 , 4, 14,15)]
eeg_dta = eeg_dta %>% group_by( subject.ID) %>% mutate_each_(funs(normalize),vars)
rm(vars)
#range(eeg_dta$Raw[eeg_dta$subject.ID==2])

wang =eeg_dta %>% 
     group_by( subject.ID ,Video.ID) %>% 
    summarise( Delta = mean(Delta),
               Theta =mean(Theta),
               Alpha.1 =mean(Alpha.1),
               Alpha.2 =mean(Alpha.2),
               Beta.1  =mean(Beta.1) ,
               Beta.2 =mean(Beta.2),
               Gamma1 =mean( Gamma1) ,
               Gamma2 =mean(Gamma2),
               Self.defined.label = factor(mean(Self.defined.label)))

wang =eeg_dta %>% 
     group_by( subject.ID ,Video.ID) %>% 
    summarise(                Delta_v = var(Delta),
               Theta_v =var(Theta),
               Alpha.1_v =var(Alpha.1),
               Alpha.2_v = var(Alpha.2),
               Beta.1_v  = var(Beta.1) ,
               Beta.2_v = var(Beta.2),
               Gamma1_v = var( Gamma1) ,
               Gamma2_v = var(Gamma2),
               Self.defined.label = factor(mean(Self.defined.label)))


wang[, c(1,2)] <- NULL

#variable selection
forest = randomForest(Self.defined.label ~ . , data = wang ,  importance =TRUE)
varImpPlot(forest, main = "Feature Ranking with Random Forest")
importance <- forest$importance
write.csv(file="Forest_importance.csv", x= importance , row.names = T)


#chi squared test
chi_weights <- chi.squared(Self.defined.label ~ . , data = wang)
write.csv(file="Chi_squared_importance.csv", x=chi_weights , row.names = T)

# Relief algorithim to find the ranking of the variables.  
relief_weights <- relief(  Self.defined.label ~ . , data= wang, neighbours.count = 20, sample.size = 90)
write.csv(file="Relief_weights_importance.csv", x=relief_weights , row.names = T)


entropy = information.gain( Self.defined.label ~ . , data = wang)

ga_ctrl <- gafsControl(functions = rfGA,
                       method = "repeatedcv",
                       repeats = 2)
set.seed(10)
system.time(rf_ga <- gafs(x = wang[,c(1:18)], y = wang$Self.defined.label,
              iters = 100,
              gafsControl = ga_ctrl))
rf_ga


```

