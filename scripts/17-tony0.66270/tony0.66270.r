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
#dailyAggDf <- read.csv('data/daily_aggregate.csv')
roamingDf <- read.csv('data/roaming_monthly.csv')

trainDf$TARGET <- as.factor(trainDf$TARGET)

trainRoamDf <- trainDf
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

testRoamDf <- testDf
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

contractRefDf$GENDER[contractRefDf$GENDER == "f"] <- "Female"
contractRefDf$GENDER[contractRefDf$GENDER == "F"] <- "Female"
contractRefDf$GENDER[contractRefDf$GENDER == "m"] <- "Male"
contractRefDf$GENDER[contractRefDf$GENDER == "M"] <- "Male"
contractRefDf$GENDER[contractRefDf$GENDER == "Not Entered"] <- "Unknown"

trainRoamDf <- merge(trainRoamDf, contractRefDf, by = "CONTRACT_KEY")
testRoamDf <- merge(testRoamDf, contractRefDf, by = "CONTRACT_KEY")

myModel <- AdaBoostM1(TARGET~X206_SESSION_COUNT + X206_USAGE + 
                        X207_SESSION_COUNT + X207_USAGE + 
                        X208_SESSION_COUNT + X208_USAGE + 
                        X209_SESSION_COUNT + X209_USAGE + 
                        X210_SESSION_COUNT + X210_USAGE +
                        R206_SESSION_COUNT + R206_USAGE + 
                        R207_SESSION_COUNT + R207_USAGE + 
                        R208_SESSION_COUNT + R208_USAGE + 
                        R209_SESSION_COUNT + R209_USAGE + 
                        R210_SESSION_COUNT + R210_USAGE +
                        GENDER
                      , data=trainRoamDf, control = Weka_control(W = MLP))
myTarget = predict(myModel, newdata = testRoamDf, type="class")
myResult <- data.frame(CONTRACT_KEY=testRoamDf$CONTRACT_KEY, PREDICTED_TARGET=myTarget)
write.table(myResult, file="output/bsmEllah.csv", sep =",", row.names= FALSE)