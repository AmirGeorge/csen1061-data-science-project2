library(dplyr)
library(knitr)
library(RWeka)

RF <- make_Weka_classifier("weka/classifiers/trees/RandomForest")
NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
MLP <- make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")

trainDf <- read.csv('data/train.csv')
testDf <- read.csv('data/test.csv')

trainDf$TARGET <- as.factor(trainDf$TARGET)

decision_trees_model <- MLP(TARGET~X206_SESSION_COUNT + X206_USAGE + X207_SESSION_COUNT + X207_USAGE + X208_SESSION_COUNT + X208_USAGE + X209_SESSION_COUNT + X209_USAGE + X210_SESSION_COUNT + X210_USAGE
                            , data=trainDf)
a = predict(decision_trees_model, newdata = testDf, type="class")
z <- data.frame(CONTRACT_KEY=testDf$CONTRACT_KEY, PREDICTED_TARGET=a)
write.table(z, file="output/bsmEllah.csv", sep =",", row.names= FALSE)