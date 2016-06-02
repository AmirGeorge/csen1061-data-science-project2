testDf <- read.csv('data/test.csv')
myResult <- data.frame(CONTRACT_KEY=testDf$CONTRACT_KEY, PREDICTED_TARGET=0)
write.table(myResult, file="game7obena.csv", sep =",", row.names= FALSE)