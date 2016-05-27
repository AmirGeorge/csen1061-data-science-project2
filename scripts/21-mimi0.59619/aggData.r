# X206_USAGE_Satuday..X206_USAGE_Friday
# X206_USAGE_Weekday
# X206_USAGE_Workday
# ======================================
# WEEKDAY_FLAG
# X206_USAGE_CELL_1...X206_USAGE_CELL_28
# ======================================
# Nafs el klam for the sessions


library(dplyr)
library(knitr)
library(RWeka)

MLP <- make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")

trainDf <- read.csv('data/train.csv')
testDf <- read.csv('data/test.csv')
contractRefDf <- read.csv('data/contract_ref.csv')
calendarRefDf <- read.csv('data/calendar_ref.csv')
dailyAggDf <- read.csv('data/daily_aggregate.csv')
roamingDf <- read.csv('data/roaming_monthly.csv')

trainDf$TARGET <- as.factor(trainDf$TARGET)
colnames(dailyAggDf)[2] <- "DATE_KEY"
a <- merge(dailyAggDf, calendarRefDf, BY = "DATE_KEY")
a$TOTAL_CONSUMPTION <- a$TOTAL_CONSUMPTION/1024/1024
# weekdays <- c('Thursday','Saturday','Sunday','Monday','Tuesday','Wednesday')

trainRoamDf <- trainDf
testRoamDf <- testDf

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








train.df <- trainRoamDf
test.df  <- testRoamDf
# -----------------------------------------------------------
local_sunday_usage <- a %>% filter(ROAMING_FLAG == "LOCAL" & DAY_NAME == "Sunday") %>% group_by(CONTRACT_KEY)  %>% summarise(local_sunday_usg = sum(TOTAL_CONSUMPTION), local_sunday_session_counts = sum(NO_OF_SESSIONS))
roaming_sunday_usage <- a %>% filter(ROAMING_FLAG == "ROAMING" & DAY_NAME == "Sunday") %>% group_by(CONTRACT_KEY)  %>% summarise(roaming_sunday_usg = sum(TOTAL_CONSUMPTION), roaming_sunday_session_counts = sum(NO_OF_SESSIONS))
train.df <- merge(train.df,local_sunday_usage, BY = "CONTRACT_KEY",all.x = T)
train.df <- merge(train.df,roaming_sunday_usage, BY = "CONTRACT_KEY",all.x = T)
train.df <- train.df %>% mutate(local_sunday_usg = ifelse(is.na(local_sunday_usg),0,local_sunday_usg))
train.df <- train.df %>% mutate(roaming_sunday_usg = ifelse(is.na(roaming_sunday_usg),0,roaming_sunday_usg))
train.df <- train.df %>% mutate(local_sunday_session_counts = ifelse(is.na(local_sunday_session_counts),0,local_sunday_session_counts))
train.df <- train.df %>% mutate(roaming_sunday_session_counts = ifelse(is.na(roaming_sunday_session_counts),0,roaming_sunday_session_counts))


local_monday_usage <- a %>% filter(ROAMING_FLAG == "LOCAL" &DAY_NAME == "Monday") %>% group_by(CONTRACT_KEY)  %>% summarise(local_monday_usg = sum(TOTAL_CONSUMPTION), local_monday_session_counts = sum(NO_OF_SESSIONS))
roaming_monday_usage <- a %>% filter(ROAMING_FLAG == "ROAMING" &DAY_NAME == "Monday") %>% group_by(CONTRACT_KEY)  %>% summarise(roaming_monday_usg = sum(TOTAL_CONSUMPTION), roaming_monday_session_counts = sum(NO_OF_SESSIONS))
train.df <- merge(train.df,local_monday_usage, BY = "CONTRACT_KEY",all.x = T)
train.df <- merge(train.df,roaming_monday_usage, BY = "CONTRACT_KEY",all.x = T)
train.df <- train.df %>% mutate(local_monday_usg = ifelse(is.na(local_monday_usg),0,local_monday_usg))
train.df <- train.df %>% mutate(roaming_monday_usg = ifelse(is.na(roaming_monday_usg),0,roaming_monday_usg))
train.df <- train.df %>% mutate(local_monday_session_counts = ifelse(is.na(local_monday_session_counts),0,local_monday_session_counts))
train.df <- train.df %>% mutate(roaming_monday_session_counts = ifelse(is.na(roaming_monday_session_counts),0,roaming_monday_session_counts))


local_tuesday_usage <- a %>% filter(ROAMING_FLAG == "LOCAL" & DAY_NAME == "Tuesday") %>% group_by(CONTRACT_KEY)  %>% summarise(local_tuesday_usg = sum(TOTAL_CONSUMPTION), local_tuesday_session_counts = sum(NO_OF_SESSIONS))
roaming_tuesday_usage <- a %>% filter(ROAMING_FLAG == "ROAMING" & DAY_NAME == "Tuesday") %>% group_by(CONTRACT_KEY)  %>% summarise(roaming_tuesday_usg = sum(TOTAL_CONSUMPTION), roaming_tuesday_session_counts = sum(NO_OF_SESSIONS))
train.df <- merge(train.df,local_tuesday_usage, BY = "CONTRACT_KEY",all.x = T)
train.df <- merge(train.df,roaming_tuesday_usage, BY = "CONTRACT_KEY",all.x = T)
train.df <- train.df %>% mutate(local_tuesday_usg = ifelse(is.na(local_tuesday_usg),0,local_tuesday_usg))
train.df <- train.df %>% mutate(roaming_tuesday_usg = ifelse(is.na(roaming_tuesday_usg),0,roaming_tuesday_usg))
train.df <- train.df %>% mutate(local_tuesday_session_counts = ifelse(is.na(local_tuesday_session_counts),0,local_tuesday_session_counts))
train.df <- train.df %>% mutate(roaming_tuesday_session_counts = ifelse(is.na(roaming_tuesday_session_counts),0,roaming_tuesday_session_counts))


local_wednesday_usage <- a %>% filter(ROAMING_FLAG == "LOCAL" & DAY_NAME == "Wednesday") %>% group_by(CONTRACT_KEY)  %>% summarise(local_wednesday_usg = sum(TOTAL_CONSUMPTION), local_wednesday_session_counts = sum(NO_OF_SESSIONS))
roaming_wednesday_usage <- a %>% filter(ROAMING_FLAG == "ROAMING" & DAY_NAME == "Wednesday") %>% group_by(CONTRACT_KEY)  %>% summarise(roaming_wednesday_usg = sum(TOTAL_CONSUMPTION), roaming_wednesday_session_counts = sum(NO_OF_SESSIONS))
train.df <- merge(train.df,local_wednesday_usage, BY = "CONTRACT_KEY",all.x = T)
train.df <- merge(train.df,roaming_wednesday_usage, BY = "CONTRACT_KEY",all.x = T)
train.df <- train.df %>% mutate(local_wednesday_usg = ifelse(is.na(local_wednesday_usg),0,local_wednesday_usg))
train.df <- train.df %>% mutate(roaming_wednesday_usg = ifelse(is.na(roaming_wednesday_usg),0,roaming_wednesday_usg))
train.df <- train.df %>% mutate(local_wednesday_session_counts = ifelse(is.na(local_wednesday_session_counts),0,local_wednesday_session_counts))
train.df <- train.df %>% mutate(roaming_wednesday_session_counts = ifelse(is.na(roaming_wednesday_session_counts),0,roaming_wednesday_session_counts))


local_thursday_usage <- a %>% filter(ROAMING_FLAG == "LOCAL" & DAY_NAME == "Thursday") %>% group_by(CONTRACT_KEY)  %>% summarise(local_thursday_usg = sum(TOTAL_CONSUMPTION), local_thursday_session_counts = sum(NO_OF_SESSIONS))
roaming_thursday_usage <- a %>% filter(ROAMING_FLAG == "ROAMING" & DAY_NAME == "Thursday") %>% group_by(CONTRACT_KEY)  %>% summarise(roaming_thursday_usg = sum(TOTAL_CONSUMPTION), roaming_thursday_session_counts = sum(NO_OF_SESSIONS))
train.df <- merge(train.df,local_thursday_usage, BY = "CONTRACT_KEY",all.x = T)
train.df <- merge(train.df,roaming_thursday_usage, BY = "CONTRACT_KEY",all.x = T)
train.df <- train.df %>% mutate(local_thursday_usg = ifelse(is.na(local_thursday_usg),0,local_thursday_usg))
train.df <- train.df %>% mutate(local_thursday_session_counts = ifelse(is.na(local_thursday_session_counts),0,local_thursday_session_counts))
train.df <- train.df %>% mutate(local_thursday_usg = ifelse(is.na(local_thursday_usg),0,local_thursday_usg))
train.df <- train.df %>% mutate(roaming_thursday_session_counts = ifelse(is.na(roaming_thursday_session_counts),0,roaming_thursday_session_counts))



local_friday_usage <- a %>% filter(ROAMING_FLAG == "LOCAL" & DAY_NAME == "Friday") %>% group_by(CONTRACT_KEY)  %>% summarise(local_friday_usg = sum(TOTAL_CONSUMPTION), local_friday_session_counts = sum(NO_OF_SESSIONS))
roaming_friday_usage <- a %>% filter(ROAMING_FLAG == "ROAMING" & DAY_NAME == "Friday") %>% group_by(CONTRACT_KEY)  %>% summarise(roaming_friday_usg = sum(TOTAL_CONSUMPTION), roaming_friday_session_counts = sum(NO_OF_SESSIONS))
train.df <- merge(train.df,local_friday_usage, BY = "CONTRACT_KEY",all.x = T)
train.df <- merge(train.df,roaming_friday_usage, BY = "CONTRACT_KEY",all.x = T)
train.df <- train.df %>% mutate(local_friday_usg = ifelse(is.na(local_friday_usg),0,local_friday_usg))
train.df <- train.df %>% mutate(roaming_friday_usg = ifelse(is.na(roaming_friday_usg),0,roaming_friday_usg))
train.df <- train.df %>% mutate(local_friday_session_counts = ifelse(is.na(local_friday_session_counts),0,local_friday_session_counts))
train.df <- train.df %>% mutate(roaming_friday_session_counts = ifelse(is.na(roaming_friday_session_counts),0,roaming_friday_session_counts))


local_saturday_usage <- a %>% filter(ROAMING_FLAG == "LOCAL" & DAY_NAME == "Saturday") %>% group_by(CONTRACT_KEY)  %>% summarise(local_saturday_usg = sum(TOTAL_CONSUMPTION), local_saturday_session_counts = sum(NO_OF_SESSIONS))
roaming_saturday_usage <- a %>% filter(ROAMING_FLAG == "ROAMING" & DAY_NAME == "Saturday") %>% group_by(CONTRACT_KEY)  %>% summarise(roaming_saturday_usg = sum(TOTAL_CONSUMPTION), roaming_saturday_session_counts = sum(NO_OF_SESSIONS))
train.df <- merge(train.df,local_saturday_usage, BY = "CONTRACT_KEY",all.x = T)
train.df <- merge(train.df,roaming_saturday_usage, BY = "CONTRACT_KEY",all.x = T)
train.df <- train.df %>% mutate(local_saturday_usg = ifelse(is.na(local_saturday_usg),0,local_saturday_usg))
train.df <- train.df %>% mutate(roaming_saturday_usg = ifelse(is.na(roaming_saturday_usg),0,roaming_saturday_usg))
train.df <- train.df %>% mutate(local_saturday_session_counts = ifelse(is.na(local_saturday_session_counts),0,local_saturday_session_counts))
train.df <- train.df %>% mutate(roaming_saturday_session_counts = ifelse(is.na(roaming_saturday_session_counts),0,roaming_saturday_session_counts))

# --------------------------------------------------------------------
# -----------------------------------------------------------
local_sunday_usage <- a %>% filter(ROAMING_FLAG == "LOCAL" & DAY_NAME == "Sunday") %>% group_by(CONTRACT_KEY)  %>% summarise(local_sunday_usg = sum(TOTAL_CONSUMPTION), local_sunday_session_counts = sum(NO_OF_SESSIONS))
roaming_sunday_usage <- a %>% filter(ROAMING_FLAG == "ROAMING" & DAY_NAME == "Sunday") %>% group_by(CONTRACT_KEY)  %>% summarise(roaming_sunday_usg = sum(TOTAL_CONSUMPTION), roaming_sunday_session_counts = sum(NO_OF_SESSIONS))
test.df <- merge(test.df,local_sunday_usage, BY = "CONTRACT_KEY",all.x = T)
test.df <- merge(test.df,roaming_sunday_usage, BY = "CONTRACT_KEY",all.x = T)
test.df <- test.df %>% mutate(local_sunday_usg = ifelse(is.na(local_sunday_usg),0,local_sunday_usg))
test.df <- test.df %>% mutate(roaming_sunday_usg = ifelse(is.na(roaming_sunday_usg),0,roaming_sunday_usg))
test.df <- test.df %>% mutate(local_sunday_session_counts = ifelse(is.na(local_sunday_session_counts),0,local_sunday_session_counts))
test.df <- test.df %>% mutate(roaming_sunday_session_counts = ifelse(is.na(roaming_sunday_session_counts),0,roaming_sunday_session_counts))


local_monday_usage <- a %>% filter(ROAMING_FLAG == "LOCAL" &DAY_NAME == "Monday") %>% group_by(CONTRACT_KEY)  %>% summarise(local_monday_usg = sum(TOTAL_CONSUMPTION), local_monday_session_counts = sum(NO_OF_SESSIONS))
roaming_monday_usage <- a %>% filter(ROAMING_FLAG == "ROAMING" &DAY_NAME == "Monday") %>% group_by(CONTRACT_KEY)  %>% summarise(roaming_monday_usg = sum(TOTAL_CONSUMPTION), roaming_monday_session_counts = sum(NO_OF_SESSIONS))
test.df <- merge(test.df,local_monday_usage, BY = "CONTRACT_KEY",all.x = T)
test.df <- merge(test.df,roaming_monday_usage, BY = "CONTRACT_KEY",all.x = T)
test.df <- test.df %>% mutate(local_monday_usg = ifelse(is.na(local_monday_usg),0,local_monday_usg))
test.df <- test.df %>% mutate(roaming_monday_usg = ifelse(is.na(roaming_monday_usg),0,roaming_monday_usg))
test.df <- test.df %>% mutate(local_monday_session_counts = ifelse(is.na(local_monday_session_counts),0,local_monday_session_counts))
test.df <- test.df %>% mutate(roaming_monday_session_counts = ifelse(is.na(roaming_monday_session_counts),0,roaming_monday_session_counts))


local_tuesday_usage <- a %>% filter(ROAMING_FLAG == "LOCAL" & DAY_NAME == "Tuesday") %>% group_by(CONTRACT_KEY)  %>% summarise(local_tuesday_usg = sum(TOTAL_CONSUMPTION), local_tuesday_session_counts = sum(NO_OF_SESSIONS))
roaming_tuesday_usage <- a %>% filter(ROAMING_FLAG == "ROAMING" & DAY_NAME == "Tuesday") %>% group_by(CONTRACT_KEY)  %>% summarise(roaming_tuesday_usg = sum(TOTAL_CONSUMPTION), roaming_tuesday_session_counts = sum(NO_OF_SESSIONS))
test.df <- merge(test.df,local_tuesday_usage, BY = "CONTRACT_KEY",all.x = T)
test.df <- merge(test.df,roaming_tuesday_usage, BY = "CONTRACT_KEY",all.x = T)
test.df <- test.df %>% mutate(local_tuesday_usg = ifelse(is.na(local_tuesday_usg),0,local_tuesday_usg))
test.df <- test.df %>% mutate(roaming_tuesday_usg = ifelse(is.na(roaming_tuesday_usg),0,roaming_tuesday_usg))
test.df <- test.df %>% mutate(local_tuesday_session_counts = ifelse(is.na(local_tuesday_session_counts),0,local_tuesday_session_counts))
test.df <- test.df %>% mutate(roaming_tuesday_session_counts = ifelse(is.na(roaming_tuesday_session_counts),0,roaming_tuesday_session_counts))


local_wednesday_usage <- a %>% filter(ROAMING_FLAG == "LOCAL" & DAY_NAME == "Wednesday") %>% group_by(CONTRACT_KEY)  %>% summarise(local_wednesday_usg = sum(TOTAL_CONSUMPTION), local_wednesday_session_counts = sum(NO_OF_SESSIONS))
roaming_wednesday_usage <- a %>% filter(ROAMING_FLAG == "ROAMING" & DAY_NAME == "Wednesday") %>% group_by(CONTRACT_KEY)  %>% summarise(roaming_wednesday_usg = sum(TOTAL_CONSUMPTION), roaming_wednesday_session_counts = sum(NO_OF_SESSIONS))
test.df <- merge(test.df,local_wednesday_usage, BY = "CONTRACT_KEY",all.x = T)
test.df <- merge(test.df,roaming_wednesday_usage, BY = "CONTRACT_KEY",all.x = T)
test.df <- test.df %>% mutate(local_wednesday_usg = ifelse(is.na(local_wednesday_usg),0,local_wednesday_usg))
test.df <- test.df %>% mutate(roaming_wednesday_usg = ifelse(is.na(roaming_wednesday_usg),0,roaming_wednesday_usg))
test.df <- test.df %>% mutate(local_wednesday_session_counts = ifelse(is.na(local_wednesday_session_counts),0,local_wednesday_session_counts))
test.df <- test.df %>% mutate(roaming_wednesday_session_counts = ifelse(is.na(roaming_wednesday_session_counts),0,roaming_wednesday_session_counts))


local_thursday_usage <- a %>% filter(ROAMING_FLAG == "LOCAL" & DAY_NAME == "Thursday") %>% group_by(CONTRACT_KEY)  %>% summarise(local_thursday_usg = sum(TOTAL_CONSUMPTION), local_thursday_session_counts = sum(NO_OF_SESSIONS))
roaming_thursday_usage <- a %>% filter(ROAMING_FLAG == "ROAMING" & DAY_NAME == "Thursday") %>% group_by(CONTRACT_KEY)  %>% summarise(roaming_thursday_usg = sum(TOTAL_CONSUMPTION), roaming_thursday_session_counts = sum(NO_OF_SESSIONS))
test.df <- merge(test.df,local_thursday_usage, BY = "CONTRACT_KEY",all.x = T)
test.df <- merge(test.df,roaming_thursday_usage, BY = "CONTRACT_KEY",all.x = T)
test.df <- test.df %>% mutate(local_thursday_usg = ifelse(is.na(local_thursday_usg),0,local_thursday_usg))
test.df <- test.df %>% mutate(local_thursday_session_counts = ifelse(is.na(local_thursday_session_counts),0,local_thursday_session_counts))
test.df <- test.df %>% mutate(local_thursday_usg = ifelse(is.na(local_thursday_usg),0,local_thursday_usg))
test.df <- test.df %>% mutate(roaming_thursday_session_counts = ifelse(is.na(roaming_thursday_session_counts),0,roaming_thursday_session_counts))



local_friday_usage <- a %>% filter(ROAMING_FLAG == "LOCAL" & DAY_NAME == "Friday") %>% group_by(CONTRACT_KEY)  %>% summarise(local_friday_usg = sum(TOTAL_CONSUMPTION), local_friday_session_counts = sum(NO_OF_SESSIONS))
roaming_friday_usage <- a %>% filter(ROAMING_FLAG == "ROAMING" & DAY_NAME == "Friday") %>% group_by(CONTRACT_KEY)  %>% summarise(roaming_friday_usg = sum(TOTAL_CONSUMPTION), roaming_friday_session_counts = sum(NO_OF_SESSIONS))
test.df <- merge(test.df,local_friday_usage, BY = "CONTRACT_KEY",all.x = T)
test.df <- merge(test.df,roaming_friday_usage, BY = "CONTRACT_KEY",all.x = T)
test.df <- test.df %>% mutate(local_friday_usg = ifelse(is.na(local_friday_usg),0,local_friday_usg))
test.df <- test.df %>% mutate(roaming_friday_usg = ifelse(is.na(roaming_friday_usg),0,roaming_friday_usg))
test.df <- test.df %>% mutate(local_friday_session_counts = ifelse(is.na(local_friday_session_counts),0,local_friday_session_counts))
test.df <- test.df %>% mutate(roaming_friday_session_counts = ifelse(is.na(roaming_friday_session_counts),0,roaming_friday_session_counts))


local_saturday_usage <- a %>% filter(ROAMING_FLAG == "LOCAL" & DAY_NAME == "Saturday") %>% group_by(CONTRACT_KEY)  %>% summarise(local_saturday_usg = sum(TOTAL_CONSUMPTION), local_saturday_session_counts = sum(NO_OF_SESSIONS))
roaming_saturday_usage <- a %>% filter(ROAMING_FLAG == "ROAMING" & DAY_NAME == "Saturday") %>% group_by(CONTRACT_KEY)  %>% summarise(roaming_saturday_usg = sum(TOTAL_CONSUMPTION), roaming_saturday_session_counts = sum(NO_OF_SESSIONS))
test.df <- merge(test.df,local_saturday_usage, BY = "CONTRACT_KEY",all.x = T)
test.df <- merge(test.df,roaming_saturday_usage, BY = "CONTRACT_KEY",all.x = T)
test.df <- test.df %>% mutate(local_saturday_usg = ifelse(is.na(local_saturday_usg),0,local_saturday_usg))
test.df <- test.df %>% mutate(roaming_saturday_usg = ifelse(is.na(roaming_saturday_usg),0,roaming_saturday_usg))
test.df <- test.df %>% mutate(local_saturday_session_counts = ifelse(is.na(local_saturday_session_counts),0,local_saturday_session_counts))
test.df <- test.df %>% mutate(roaming_saturday_session_counts = ifelse(is.na(roaming_saturday_session_counts),0,roaming_saturday_session_counts))

#sss<- train.df[1,]
#sss<- test.df[1,]
#ss <- sum(sss$local_sunday_usg, sss$local_monday_usg,sss$local_tuesday_usg,sss$local_wednesday_usg,sss$local_thursday_usg, sss$local_friday_usg, sss$local_saturday_usg)

#ssSessionCount <- sum(sss$local_sunday_session_count, sss$local_monday_session_count,sss$local_tuesday_session_count,sss$local_wednesday_session_count,sss$local_thursday_session_count, sss$local_friday_session_count, sss$local_saturday_session_count)
# -----------------------------------------------------------------

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
                 local_thursday_usg+local_saturday_usg+local_sunday_usg+local_monday_usg+local_tuesday_usg+local_wednesday_usg+local_friday_usg+
                 roaming_thursday_usg+roaming_saturday_usg+roaming_sunday_usg+roaming_monday_usg+roaming_tuesday_usg+roaming_wednesday_usg+roaming_friday_usg+
                 local_thursday_session_counts+local_saturday_session_counts+local_sunday_session_counts+local_monday_session_counts+local_tuesday_session_counts+local_wednesday_session_counts+local_friday_session_counts+
                 roaming_thursday_session_counts+roaming_saturday_session_counts+roaming_sunday_session_counts+roaming_monday_session_counts+roaming_tuesday_session_counts+roaming_wednesday_session_counts+roaming_friday_session_counts
                            , data=train.df)
myTarget = predict(myModel, newdata = test.df, type="class")
myResult <- data.frame(CONTRACT_KEY=test.df$CONTRACT_KEY, PREDICTED_TARGET=myTarget)
write.table(myResult, file="aggData.csv", sep =",", row.names= FALSE)
















