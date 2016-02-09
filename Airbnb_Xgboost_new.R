is.narequire(caret)
require(corrplot)
require(Rtsne)
require(xgboost)
require(stats)
require(knitr)
require(ggplot2)
knitr::opts_chunk$set(cache=TRUE)

data <- read.csv("Training_Data.csv")
Training <- data[c(-1,-3,-4)]
Training[is.na(Training)]<- -1
Training$age[Training$age > 100] <- -1
Training$age[Training$age < 10] <- -1
str(Training)
Training$AgeDiscret <- ifelse(Training$age>-1,round(Training$age/10,0),Training$age)
Training$AgeCat <- ifelse(Training$age > 35,"Old","Young")
outcome.org= Training[,"country_destination"]
outcome = outcome.org
levels(outcome)
num.class = length(levels(outcome))
levels(outcome) = 1:num.class
head(outcome)

zero.var <- nearZeroVar(Training[-13],saveMetrics = TRUE)
zero.var


Training.matrix <- sparse.model.matrix(Training$country_destination~.,data=Training)

str(Training.matrix)

param <- list("objective" = "multi:softprob",     
              "num_class" = num.class,  
              "eval_metric" = "merror",   
              "nthread" = 3,  
              "max_depth" = 9,    
              "eta" = 0.1,    
              "gamma" = 0,    
              "subsample" = 0.5,     
              "colsample_bytree" = 0.5)

y = as.matrix(as.integer(outcome)-1)
set.seed= 1234
nround= 25
bst <- xgboost(param=param, data=Training.matrix, label=y, 
            nrounds=nround, prediction=TRUE, verbose=FALSE) 


Test_Users$AgeDiscret <- ifelse(Test_Users$age>-1,round(Test_Users$age/10,0),Test_Users$age)
Test_Users$AgeCat <- ifelse(Test_Users$age > 35,"Old","Young")
Test.matrix <- sparse.model.matrix(~.,data=Test_Users)
pred <- predict(bst,Test.matrix)
predictions <- as.data.frame(matrix(pred, nrow=12))
rownames(predictions) <- c('NDF','US','other','FR','CA','GB','ES','IT','PT','NL','DE','AU')
predictions_sort <- as.vector(apply(predictions, 2, function(x) names(sort(x)[12:8])))

ids <- NULL
for (i in 1:NROW(Test_Users)) {
  idx <- Test_Users$id[i]
  ids <- append(ids, rep(idx,5))
}

submission <- NULL
submission$id <- ids
submission$country <- predictions_sort

submission <- as.data.frame(submission)
write.csv(submission, "submission.csv", quote=FALSE, row.names = FALSE)




write.csv(submission, "submission.csv", quote=FALSE, row.names = FALSE)

