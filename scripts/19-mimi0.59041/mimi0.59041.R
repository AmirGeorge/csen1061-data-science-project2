# Amir's roaming data
# Slope average and from month 1 to 5
# Score 0.59041
library(dplyr)
library(knitr)
library(RWeka)

RF <- make_Weka_classifier("weka/classifiers/trees/RandomForest")
NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
MLP <- make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")

trainDf <- read.csv('data/train.csv')
testDf <- read.csv('data/test.csv')
contractRefDf <- read.csv('data/contract_ref.csv')
calendarRefDf <- read.csv('data/calendar_ref.csv')
dailyAggDf <- read.csv('data/daily_aggregate.csv')
roamingDf <- read.csv('data/roaming_monthly.csv')

trainDf$TARGET <- as.factor(trainDf$TARGET)

slope1  <-  function(x){
  as.double(x[5])-as.double(x[3])
} 
  
slope2  <-  function(x){
  as.double(x[7])-as.double(x[5])
} 

slope3  <-  function(x){
  as.double(x[9])-as.double(x[7])
} 

slope4  <-  function(x){
  as.double(x[11])-as.double(x[9])
} 

slope1to5  <-  function(x){
  (as.double(x[11])-as.double(x[3]))/4
} 

  avg_slope  <-  function(x){
    return (as.double(x[13])+as.double(x[14])+as.double(x[15])+as.double(x[16]))/4
  } 

  avg2_slope  <-  function(x){
    return (as.double(x[12])+as.double(x[13])+as.double(x[14])+as.double(x[15]))/4
  } 

trainRoamDf <- trainDf
#prepare tain data
trainRoamDf$slop1 <- apply(trainRoamDf,1,slope1)
trainRoamDf$slop2 <- apply(trainRoamDf,1,slope2)
trainRoamDf$slop3 <- apply(trainRoamDf,1,slope3)
trainRoamDf$slop4 <- apply(trainRoamDf,1,slope4)
trainRoamDf$slop1to5 <- apply(trainRoamDf,1,slope1to5)
trainRoamDf$avg_slop <- apply(trainRoamDf,1,avg_slope)


testRoamDf <- testDf
##prepare test data
testRoamDf$slop1 <- apply(testRoamDf,1,slope1)
testRoamDf$slop2 <- apply(testRoamDf,1,slope2)
testRoamDf$slop3 <- apply(testRoamDf,1,slope3)
testRoamDf$slop4 <- apply(testRoamDf,1,slope4)
testRoamDf$slop1to5 <- apply(testRoamDf,1,slope1to5)
testRoamDf$avg_slop <- apply(testRoamDf,1,avg2_slope)

###############
#contract plan
temp <- data.frame(CONTRACT_KEY=contractRefDf$CONTRACT_KEY,  VALUE_SEGMENT = contractRefDf$VALUE_SEGMENT)
trainRoamDf <- merge(trainRoamDf, temp, by = "CONTRACT_KEY")
testRoamDf <- merge(testRoamDf, temp, by = "CONTRACT_KEY")


trainRoamDf[,"R206_USAGE"] <- 0
trainRoamDf[,"R206_SESSION_COUNT"] <- 0
trainRoamDf[,"R207_USAGE"] <- 0
trainRoamDf[,"R207_SESSION_COUNT"] <- 0
trainRoamDf[,"R208_USAGE"] <- 0
trainRoamDf[,"R208_SESSION_COUNT"] <- 0
trainRoamDf[,"R209_USAGE"] <- 0
trainRoamDf[,"R209_SESSION_COUNT"] <- 0
trainRoamDf[,"R210_USAGE"] <- 0
trainRoamDf[,"R210_SESSION_COUNT"] <- 0


testRoamDf[,"R206_USAGE"] <- 0
testRoamDf[,"R206_SESSION_COUNT"] <- 0
testRoamDf[,"R207_USAGE"] <- 0
testRoamDf[,"R207_SESSION_COUNT"] <- 0
testRoamDf[,"R208_USAGE"] <- 0
testRoamDf[,"R208_SESSION_COUNT"] <- 0
testRoamDf[,"R209_USAGE"] <- 0
testRoamDf[,"R209_SESSION_COUNT"] <- 0
testRoamDf[,"R210_USAGE"] <- 0
testRoamDf[,"R210_SESSION_COUNT"] <- 0

for (k in unique(roamingDf$CONTRACT_KEY)) {
  orig <- roamingDf[roamingDf$CONTRACT_KEY==k,]
  if (trainRoamDf[trainRoamDf$CONTRACT_KEY==k,] %>% nrow > 0) {
    val <- orig[orig$CALL_MONTH_KEY == 206,]
    if (nrow(val) > 0) {
      trainRoamDf[trainRoamDf$CONTRACT_KEY==k,"R206_USAGE"] = val$USAGE
      trainRoamDf[trainRoamDf$CONTRACT_KEY==k,"R206_SESSION_COUNT"] = val$SESSION_COUNT
    }
    val <- orig[orig$CALL_MONTH_KEY == 207,]
    if (nrow(val) > 0) {
      trainRoamDf[trainRoamDf$CONTRACT_KEY==k,"R207_USAGE"] = val$USAGE
      trainRoamDf[trainRoamDf$CONTRACT_KEY==k,"R207_SESSION_COUNT"] = val$SESSION_COUNT
    }
    val <- orig[orig$CALL_MONTH_KEY == 208,]
    if (nrow(val) > 0) {
      trainRoamDf[trainRoamDf$CONTRACT_KEY==k,"R208_USAGE"] = val$USAGE
      trainRoamDf[trainRoamDf$CONTRACT_KEY==k,"R208_SESSION_COUNT"] = val$SESSION_COUNT
    }
    val <- val[val$CALL_MONTH_KEY == 209,]
    if (nrow(val) > 0) {
      trainRoamDf[trainRoamDf$CONTRACT_KEY==k,"R209_USAGE"] = val$USAGE
      trainRoamDf[trainRoamDf$CONTRACT_KEY==k,"R209_SESSION_COUNT"] = val$SESSION_COUNT
    }
    val <- orig[orig$CALL_MONTH_KEY == 210,]
    if (nrow(val) > 0) {
      trainRoamDf[trainRoamDf$CONTRACT_KEY==k,"R210_USAGE"] = val$USAGE
      trainRoamDf[trainRoamDf$CONTRACT_KEY==k,"R210_SESSION_COUNT"] = val$SESSION_COUNT
    }
  }
  else {
    val <- orig[orig$CALL_MONTH_KEY == 206,]
    if (nrow(val) > 0) {
      testRoamDf[testRoamDf$CONTRACT_KEY==k,"R206_USAGE"] = val$USAGE
      testRoamDf[testRoamDf$CONTRACT_KEY==k,"R206_SESSION_COUNT"] = val$SESSION_COUNT
    }
    val <- orig[orig$CALL_MONTH_KEY == 207,]
    if (nrow(val) > 0) {
      testRoamDf[testRoamDf$CONTRACT_KEY==k,"R207_USAGE"] = val$USAGE
      testRoamDf[testRoamDf$CONTRACT_KEY==k,"R207_SESSION_COUNT"] = val$SESSION_COUNT
    }
    val <- orig[orig$CALL_MONTH_KEY == 208,]
    if (nrow(val) > 0) {
      testRoamDf[testRoamDf$CONTRACT_KEY==k,"R208_USAGE"] = val$USAGE
      testRoamDf[testRoamDf$CONTRACT_KEY==k,"R208_SESSION_COUNT"] = val$SESSION_COUNT
    }
    val <- val[val$CALL_MONTH_KEY == 209,]
    if (nrow(val) > 0) {
      testRoamDf[testRoamDf$CONTRACT_KEY==k,"R209_USAGE"] = val$USAGE
      testRoamDf[testRoamDf$CONTRACT_KEY==k,"R209_SESSION_COUNT"] = val$SESSION_COUNT
    }
    val <- orig[orig$CALL_MONTH_KEY == 210,]
    if (nrow(val) > 0) {
      testRoamDf[testRoamDf$CONTRACT_KEY==k,"R210_USAGE"] = val$USAGE
      testRoamDf[testRoamDf$CONTRACT_KEY==k,"R210_SESSION_COUNT"] = val$SESSION_COUNT
    }
  }
}

trainRoamDf <- trainRoamDf %>% mutate(X206_SESSION_COUNT = X206_SESSION_COUNT - R206_SESSION_COUNT,
                                      X206_USAGE = X206_USAGE - R206_USAGE,
                                      X207_SESSION_COUNT = X207_SESSION_COUNT - R207_SESSION_COUNT,
                                      X207_USAGE = X207_USAGE - R207_USAGE,
                                      X208_SESSION_COUNT = X208_SESSION_COUNT - R208_SESSION_COUNT,
                                      X208_USAGE = X208_USAGE - R208_USAGE,
                                      X209_SESSION_COUNT = X209_SESSION_COUNT - R209_SESSION_COUNT,
                                      X209_USAGE = X209_USAGE - R209_USAGE,
                                      X210_SESSION_COUNT = X210_SESSION_COUNT - R210_SESSION_COUNT,
                                      X210_USAGE = X210_USAGE - R210_USAGE)

testRoamDf <- testRoamDf %>% mutate(X206_SESSION_COUNT = X206_SESSION_COUNT - R206_SESSION_COUNT,
                                      X206_USAGE = X206_USAGE - R206_USAGE,
                                      X207_SESSION_COUNT = X207_SESSION_COUNT - R207_SESSION_COUNT,
                                      X207_USAGE = X207_USAGE - R207_USAGE,
                                      X208_SESSION_COUNT = X208_SESSION_COUNT - R208_SESSION_COUNT,
                                      X208_USAGE = X208_USAGE - R208_USAGE,
                                      X209_SESSION_COUNT = X209_SESSION_COUNT - R209_SESSION_COUNT,
                                      X209_USAGE = X209_USAGE - R209_USAGE,
                                      X210_SESSION_COUNT = X210_SESSION_COUNT - R210_SESSION_COUNT,
                                      X210_USAGE = X210_USAGE - R210_USAGE)



myModel <- MLP(TARGET~X206_SESSION_COUNT + X206_USAGE + 
                 X207_SESSION_COUNT + X207_USAGE + 
                 X208_SESSION_COUNT + X208_USAGE + 
                 X209_SESSION_COUNT + X209_USAGE + 
                 X210_SESSION_COUNT + X210_USAGE +
                 R206_SESSION_COUNT + R206_USAGE + 
                 R207_SESSION_COUNT + R207_USAGE + 
                 R208_SESSION_COUNT + R208_USAGE + 
                 R209_SESSION_COUNT + R209_USAGE + 
                 R210_SESSION_COUNT + R210_USAGE+
                 
                                slop1to5 + avg_slop+VALUE_SEGMENT
                            , data=trainRoamDf)
myTarget = predict(myModel, newdata = testRoamDf, type="class")
myResult <- data.frame(CONTRACT_KEY=testRoamDf$CONTRACT_KEY, PREDICTED_TARGET=myTarget)
write.table(myResult, file="slopeRoamValueSEGM.csv", sep =",", row.names= FALSE)