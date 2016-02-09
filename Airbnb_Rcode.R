Data <- read.csv("Training_Data.csv")
table(Data$country_destination)
Train_Ctr1 <- Data[!(Data$country_destination %in% c("NDF","US",
                                                     "other")),]
Train_NDF <- Data[Data$country_destination %in% c("NDF"),]
Train_Other <- Data[Data$country_destination %in% c("other"),]
Train_US <- Data[Data$country_destination %in% c("US"),]
Train_Final <- rbind(Train_NDF[order(runif(8000)),],Train_Other[order(runif(6500)),],Train_US[order(runif(8000)),],Train_Ctr1)
summary(Train_Final)
Train_Final$SeqID <- seq(1,38938,by=1)
SeqID <- cbind(Train_Final$id,Train_Final$SeqID)
Train_Final <- Train_Final[c(-1,-3)] 
Train_Final <- Train_Final[-2]
Train_Final[is.na(Train_Final)]<- -1
Train_Final$age[Train_Final$age > 115] <- -1
Train_Final$age[Train_Final$age < 10] <- -1
Train <- Train_Final[order(runif(1:29203)),]
Test <- Train_Final[order(runif(29204:38939)),]
GB <- gbm(formula = Train$country_destination ~ ., distribution = "multinomial", 
      data = Train[-1], n.trees = 500, interaction.depth = 10, shrinkage = 0.05, 
      bag.fraction = 0.5, cv.folds = 3, keep.data = FALSE)
summary(GB)

Test_Users <- read.csv("Test_Users_PP.csv")
Test_Users <- Test_Users[c(-1,-3)]
Test_Users[is.na(Test_Users)]<- -1
Test_Users$age[Test_Users$age > 115] <- -1
Test_Users$age[Test_Users$age < 10] <- -1

Test_Users$date_account_created <- as.numeric(Test_Users$date_account_created)
Test_Users$Date_First_Active <- as.numeric(Test_Users$Date_First_Active)
Test_Users$TimeData_Timestamp_First_Active <- as.numeric(Test_Users$TimeData_Timestamp_First_Active)

Pred <- predict(GB,Test_Users,type="response",n.trees = 500)
write.csv(Pred,file="Try2_R.csv",row.names = FALSE)

