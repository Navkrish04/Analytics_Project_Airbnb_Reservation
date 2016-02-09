require(xgboost)
require(Matrix)
require(data.table)
require(vcd)
library(readr)
library(stringr)
library(caret)
library(car)

Training[is.na(Training)]<- -1
Training$age[Training$age > 115] <- -1
Training$age[Training$age < 10] <- -1
Training <- Training[c(-1,-3,-4)]
Train <- data.table(Training,keep.rownames = F)
head(Train)
Train[,Agediscret := as.factor(ifelse(age>-1,round(age/10,0),age))]
summary(Train$age)
str(Train)
Train[,AgeCat := as.factor(ifelse(age>35,"Old","Young"))]
head(Train)
table(Train$language)
levels(Train[,country_destination])
Sparse_Matrix <- sparse.model.matrix(Train$country_destination~.,data=Train)
head(Sparse_Matrix)
str(Sparse_Matrix)
y <- recode(Train$country_destination,"'NDF'=0; 'US'=1; 'other'=2; 'FR'=3; 'CA'=4; 'GB'=5; 'ES'=6; 'IT'=7; 'PT'=8; 'NL'=9; 'DE'=10; 'AU'=11")
bst <- xgboost(data = Sparse_Matrix,label = y, max.depth = 10,set.seed=123,
eta = 0.1, nthread = 3, nround = 30,num_class=13,objective = "multi:softprob")

importance <- xgb.importance(Sparse_Matrix@Dimnames[[2]], model = bst)

head(importance)
library(Ckmeans.1d.dp)
xgb.plot.importance(importance_matrix = importance)

Test_Users[is.na(Test_Users)]<- -1
Test_Users$age[Test_Users$age > 115] <- -1
Test_Users$age[Test_Users$age < 10] <- -1

Test <- data.table(Test_Users,keep.rownames = F)

Test[,Agediscret := as.factor(ifelse(age>-1,round(age/10,0),age))]
Test[,AgeCat := as.factor(ifelse(age>35,"Old","Young"))]

TestSparse_Matrix <- sparse.model.matrix(~.,data=Test)
  pred <- predict(bst,TestSparse_Matrix)
 summary(pred)

 predictions <- as.data.frame(matrix(pred, nrow=12))
 rownames(predictions) <- c('NDF','US','other','FR','CA','GB','ES','IT','PT','NL','DE','AU')
 predictions_top5 <- as.vector(apply(predictions, 2, function(x) names(sort(x)[12:8])))

write.csv(predictions_top5,file="Prediction_SM.csv",row.names = FALSE)
  outcome = Train[,country_destination]
levels(outcome)
num.class = length(levels(outcome))
levels(outcome) = 1:num.class
head(outcome)

pred <- matrix(pred,nrow = 12,ncol=length(pred)/12)
str(Test)
Test$id <- seq(1,62096,by=1)
str(Test)
