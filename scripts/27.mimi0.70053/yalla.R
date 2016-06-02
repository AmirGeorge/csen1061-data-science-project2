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
  RF <- make_Weka_classifier("weka/classifiers/trees/RandomForest")
  NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")

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


  monthly_usage_columns <- c('X206_USAGE','X207_USAGE','X208_USAGE','X209_USAGE','X210_USAGE')
  weekdays <- c('Saturday','Sunday','Monday','Tuesday','Wednesday','Thursday','Friday')

  train.df <- trainRoamDf
  test.df  <- testRoamDf
  mid <- 2200
  train.df <- train.df %>% rowwise() %>% mutate(diff1 = X207_USAGE - X206_USAGE, diff2 = X208_USAGE-X207_USAGE, diff3=X209_USAGE-X208_USAGE,diff4=X210_USAGE-X209_USAGE, always_increasing = (diff1 > 0 & diff2 > 0 & diff3 >0 & diff4 > 0), above_mid = (min(X206_USAGE,X207_USAGE,X208_USAGE,X209_USAGE,X210_USAGE) > mid))
  test.df <- test.df %>% rowwise() %>% mutate(diff1 = X207_USAGE - X206_USAGE, diff2 = X208_USAGE-X207_USAGE, diff3=X209_USAGE-X208_USAGE,diff4=X210_USAGE-X209_USAGE, always_increasing = (diff1 > 0 & diff2 > 0 & diff3 >0 & diff4 > 0), above_mid = (min(X206_USAGE,X207_USAGE,X208_USAGE,X209_USAGE,X210_USAGE) > mid))

  for(name in weekdays){
      # Local Data
      col_name_usage <- paste('local_',name,'_usage',sep='')
      col_name_sessions_count <- paste('local_',name,'_sessions_count',sep='')
      filtered_data <- a %>% filter(ROAMING_FLAG == "LOCAL" & DAY_NAME == name)
      
      exp <- paste("filtered_data %>% group_by(CONTRACT_KEY) %>% 
        summarise(", col_name_usage, "= sum(TOTAL_CONSUMPTION),",col_name_sessions_count,"=sum(NO_OF_SESSIONS) )", sep = "")
      local <- eval(parse(text = exp))
      train.df <- merge(train.df,local, BY = "CONTRACT_KEY",all.x = T)
      test.df <- merge(test.df,local, BY = "CONTRACT_KEY",all.x = T)
      
      ##Remove Na's
      #local usage
      exp <- paste("train.df <- train.df %>% mutate(",col_name_usage,"= ifelse(is.na(",col_name_usage,"),X210_USAGE/7,",col_name_usage,"))", sep= "")
      eval(parse(text = exp))

      #local sessions count
      exp <- paste("train.df <- train.df %>% mutate(",col_name_sessions_count,"= ifelse(is.na(",col_name_sessions_count,"),X210_SESSION_COUNT/7,",col_name_sessions_count,"))", sep= "")
      eval(parse(text = exp))

      ##Remove Na's
      #testing
      exp <- paste("test.df <- test.df %>% mutate(",col_name_usage,"= ifelse(is.na(",col_name_usage,"),X210_USAGE/7,",col_name_usage,"))", sep= "")
      eval(parse(text = exp))

      exp <- paste("test.df <- test.df %>% mutate(",col_name_sessions_count,"= ifelse(is.na(",col_name_sessions_count,"),X210_SESSION_COUNT/7,",col_name_sessions_count,"))", sep= "")
      eval(parse(text = exp))

      #Roaming Data
      col_name_usage <- paste('roaming_',name,'_usage',sep='')
      col_name_sessions_count <- paste('roaming_',name,'_sessions_count',sep='')

      filtered_data <- a %>% filter(ROAMING_FLAG == "ROAMING" & DAY_NAME == name)
      exp <- paste("filtered_data %>% group_by(CONTRACT_KEY) %>% 
        summarise(", col_name_usage, "= sum(TOTAL_CONSUMPTION),",col_name_sessions_count,"=sum(NO_OF_SESSIONS) )", sep = "")
      roaming <- eval(parse(text = exp))
      train.df <- merge(train.df,roaming, BY = "CONTRACT_KEY",all.x = T)
      test.df <- merge(test.df,roaming, BY = "CONTRACT_KEY",all.x = T) 

      ##Remove Na's
      #roamiming usage
      exp <- paste("train.df <- train.df %>% mutate(",col_name_usage,"= ifelse(is.na(",col_name_usage,"),R210_USAGE/7,",col_name_usage,"))", sep= "")
      eval(parse(text = exp))

      #roamiming sessions count
      exp <- paste("train.df <- train.df %>% mutate(",col_name_sessions_count,"= ifelse(is.na(",col_name_sessions_count,"),R210_SESSION_COUNT/7,",col_name_sessions_count,"))", sep= "")
      eval(parse(text = exp))


      ##Remove Na's
      #roamiming usage
      exp <- paste("test.df <- test.df %>% mutate(",col_name_usage,"= ifelse(is.na(",col_name_usage,"),R210_USAGE/7,",col_name_usage,"))", sep= "")
      eval(parse(text = exp))

      #local sessions count
      exp <- paste("test.df <- test.df %>% mutate(",col_name_sessions_count,"= ifelse(is.na(",col_name_sessions_count,"),R210_SESSION_COUNT/7,",col_name_sessions_count,"))", sep= "")
      eval(parse(text = exp))
  }


  ## Train 
  train.df <- train.df %>% rowwise() %>% mutate(
    weekday_usage = sum(c(local_Sunday_usage,roaming_Sunday_usage,local_Monday_usage,roaming_Monday_usage,local_Tuesday_usage,roaming_Tuesday_usage,local_Wednesday_usage,roaming_Wednesday_usage,local_Thursday_usage,roaming_Thursday_usage))
    ,
    weekday_sessions_count = sum(c(local_Sunday_sessions_count,roaming_Sunday_sessions_count,local_Monday_sessions_count,roaming_Monday_sessions_count,local_Tuesday_sessions_count,roaming_Tuesday_sessions_count,local_Wednesday_sessions_count,roaming_Wednesday_sessions_count,local_Thursday_sessions_count,roaming_Thursday_sessions_count))
    ,
    weekend_usage = sum(c(local_Friday_usage,roaming_Saturday_usage,local_Saturday_usage,roaming_Saturday_usage))
    ,
    weekend_sessions_count = sum(c(local_Friday_sessions_count,roaming_Saturday_sessions_count,local_Saturday_sessions_count,roaming_Saturday_sessions_count))
   )

  ## Test
  test.df <- test.df %>% rowwise() %>% mutate(
    weekday_usage = sum(c(local_Sunday_usage,roaming_Sunday_usage,local_Monday_usage,roaming_Monday_usage,local_Tuesday_usage,roaming_Tuesday_usage,local_Wednesday_usage,roaming_Wednesday_usage,local_Thursday_usage,roaming_Thursday_usage))
    ,
    weekday_sessions_count = sum(c(local_Sunday_sessions_count,roaming_Sunday_sessions_count,local_Monday_sessions_count,roaming_Monday_sessions_count,local_Tuesday_sessions_count,roaming_Tuesday_sessions_count,local_Wednesday_sessions_count,roaming_Wednesday_sessions_count,local_Thursday_sessions_count,roaming_Thursday_sessions_count))
    ,
    weekend_usage = sum(c(local_Friday_usage,roaming_Saturday_usage,local_Saturday_usage,roaming_Saturday_usage))
    ,
    weekend_sessions_count = sum(c(local_Friday_sessions_count,roaming_Saturday_sessions_count,local_Saturday_sessions_count,roaming_Saturday_sessions_count))
   )

  train.df <- train.df %>% rowwise() %>% mutate(
    mean_usage = mean(c(X206_USAGE,X207_USAGE,X208_USAGE,X209_USAGE,X210_USAGE)),

    sd_usage =sd(c(X206_USAGE,X207_USAGE,X208_USAGE,X209_USAGE,X210_USAGE)),

    median_usage = median(c(X206_USAGE,X207_USAGE,X208_USAGE,X209_USAGE,X210_USAGE) ),

    mean_sessions_count=mean(c(X206_SESSION_COUNT,X207_SESSION_COUNT,X208_SESSION_COUNT,X209_SESSION_COUNT,X210_SESSION_COUNT)),
    
    median_sessions_count=median(c(X206_SESSION_COUNT,X207_SESSION_COUNT,
      X208_SESSION_COUNT,X209_SESSION_COUNT,X210_SESSION_COUNT)),
    
    sd_sessions_count=sd(c(X206_SESSION_COUNT,X207_SESSION_COUNT,X208_SESSION_COUNT,X209_SESSION_COUNT,X210_SESSION_COUNT))
    )


  test.df <- test.df %>% rowwise() %>% mutate(
    mean_usage = mean(c(X206_USAGE,X207_USAGE,X208_USAGE,X209_USAGE,X210_USAGE)),

    sd_usage =sd(c(X206_USAGE,X207_USAGE,X208_USAGE,X209_USAGE,X210_USAGE)),

    median_usage = median(c(X206_USAGE,X207_USAGE,X208_USAGE,X209_USAGE,X210_USAGE) ),

    mean_sessions_count=mean(c(X206_SESSION_COUNT,X207_SESSION_COUNT,X208_SESSION_COUNT,X209_SESSION_COUNT,X210_SESSION_COUNT)),
    
    median_sessions_count=median(c(X206_SESSION_COUNT,X207_SESSION_COUNT,
      X208_SESSION_COUNT,X209_SESSION_COUNT,X210_SESSION_COUNT)),
    
    sd_sessions_count=sd(c(X206_SESSION_COUNT,X207_SESSION_COUNT,X208_SESSION_COUNT,X209_SESSION_COUNT,X210_SESSION_COUNT))
    )

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
                     local_Thursday_usage+local_Saturday_usage+local_Sunday_usage+local_Monday_usage+local_Tuesday_usage+local_Wednesday_usage+local_Friday_usage+
                     roaming_Thursday_usage+roaming_Saturday_usage+roaming_Sunday_usage+roaming_Monday_usage+roaming_Tuesday_usage+roaming_Wednesday_usage+roaming_Friday_usage+
                     local_Thursday_sessions_count+local_Saturday_sessions_count+local_Sunday_sessions_count+local_Monday_sessions_count+local_Tuesday_sessions_count+local_Wednesday_sessions_count+local_Friday_sessions_count+
                     roaming_Thursday_sessions_count+roaming_Saturday_sessions_count+roaming_Sunday_sessions_count+roaming_Monday_sessions_count+roaming_Tuesday_sessions_count+roaming_Wednesday_sessions_count+roaming_Friday_sessions_count+mean_usage+mean_sessions_count+median_usage+median_sessions_count+sd_usage+sd_sessions_count
                     + weekday_usage+ weekend_usage+ weekday_sessions_count+ weekend_sessions_count
                                , data=train.df)
 

  myTarget = predict(myModel, newdata = test.df, type=c("class"))
  myResult <- data.frame(CONTRACT_KEY=test.df$CONTRACT_KEY, PREDICTED_TARGET=myTarget)
  myResult %>% filter(PREDICTED_TARGET == 1) %>% nrow()
  write.table(myResult, file="yallaWithoutAgg.csv", sep =",", row.names= FALSE)
