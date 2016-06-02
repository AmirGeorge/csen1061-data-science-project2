library(dplyr)
library(knitr)
library(RWeka)

RF <- make_Weka_classifier("weka/classifiers/trees/RandomForest")
NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
MLP <- make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")

trainDf <- read.csv('data/train.csv')
testDf <- read.csv('data/test.csv')

trainDf$TARGET <- as.factor(trainDf$TARGET)
nnmodel <- MLP(TARGET~., data=trainDf)
a = predict(nnmodel, newdata = testDf, type="class")
z <- data.frame(CONTRACT_KEY=testDf$CONTRACT_KEY, PREDICTED_TARGET=a)
write.table(z, file="output/bsmEllah.csv", sep =",", row.names= FALSE)