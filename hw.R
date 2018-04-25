# EX2 in Data Science 
# Build a Decesion Tree to classify between fraudulent credit card transactions

replace_missing_values <- function (df) {
  # replace each numerical column with it's avergage, and categorical value with it's median
  for (col in 1:NCOL(df)){
    if (class(df[,col]) == 'numeric'){
      df[is.na(df[,col]),col] <- mean(df[,col], na.rm =  TRUE)
    }
    else if (class(df[,col]) == 'factor')  {
      df[is.na(df[,col]),col] <- names(sort(table(df[,col]), decreasing = TRUE)[1])
    }
  }
  df
}

discretization <- function(df, equal_width = 3 , equal_depth = 4){
  #INPUT: german_credit data
  #OUTPUT: create a discretization - 
  #Average_Credit_balance, Over_draft - equal depth
  #CC_age - equal width
  df$over_draft <- cut(df$over_draft, quantile(df$over_draft), include.lowest = TRUE, labels= c('low','meduim-low', 'medium-high', 'high'))
  df$Average_Credit_Balance <- cut(df$Average_Credit_Balance,  quantile(df$Average_Credit_Balance),  include.lowest = TRUE,labels= c('low','meduim-low', 'medium-high', 'high'))
  min_age <- min(df$cc_age)
  max_age <- max(df$cc_age)
  df$cc_age <- cut(df$cc_age,seq(min_age,max_age,(max_age-min_age)/equal_width) ,labels = c('young',' adult', 'Senior'), include.lowest = TRUE)
  df
}

transpose_data <- function(german_credit) {
  # INPUT german credit data where headers & values are row based.
  # OUTPUT german credit df where headers & values are column based.
  headers <- german_credit$X__1
  parsed_rows <- strsplit(german_credit$X__2,',')
  parsed_cols <- NULL
  for (row in parsed_rows){
    row[row==''] <- NA
    parsed_cols <- cbind(parsed_cols,row)
  }
  colnames(parsed_cols) <- headers
  german_credit <- as.data.frame(parsed_cols, stringsAsFactors =  FALSE)
  # change the class of each col according to the Description File
  german_credit <- transform(german_credit, over_draft = as.numeric(over_draft),
                             credit_usage = as.numeric(credit_usage),
                             current_balance = as.numeric(current_balance),
                             Average_Credit_Balance = as.numeric(Average_Credit_Balance),
                             cc_age = as.numeric(cc_age),
                             num_dependents = as.numeric(num_dependents),
                             credit_history  = as.factor(credit_history),
                             purpose = as.factor(purpose),
                             personal_status = as.factor(personal_status),
                             property_magnitude = as.factor(property_magnitude),
                             housing = as.factor(housing),
                             job = as.factor(job),
                             class = as.factor(class)
  )
}



load_and_parse_data <- function(path) {
  # INPUT: path to german credit fraud excel file
  # OUTPUT: parsed data frame
  ## load excel file and split rows to columns, replace empty strings with NA
  library(readxl)
  german_credit <- read_excel(path, sheet =  'Data', col_names = FALSE)
  german_credit <- transpose_data(german_credit)
  german_credit <- replace_missing_values(german_credit)
  german_credit <- discretization(german_credit)
  german_credit
}

split_train_test <- function(df) {
  # INPUT german credit df
  # OUTPUT list separating the df into train and test sets 
  library(caret)
  set.seed(123)
  inTrain <- createDataPartition(y=df$class, p=.8, list=FALSE)
  training <- df[inTrain,]
  testing <- df [-inTrain,]
  list(train = training, test = testing)
}

build_tree <- function(test_train, split, minsplit, cp = 0.05){
  # INPUT german credit divided into train and test, split indicator and min split threshold
  # OUTPUT tree accuracy
  library(rpart)
  model <- rpart(class~., data = test_train$train,
                 parms = list(split = split), control = rpart.control(minsplit = minsplit,
                                                                      cp = cp))
  
  ##calaculate number of leafs
  print(grepl("^<leaf>$", as.character(model$frame$var)))

  print(sum(grepl("^<leaf>$", as.character(model$frame$var))))
  
  pred_train <- as.data.frame(predict(model))
  pred_train$class <- as.factor(ifelse(pred_train$bad <= pred_train$good ,'good', 'bad'))
  pred_test <- as.data.frame(predict(model, newdata = test_train$test))
  pred_test$class <- as.factor(ifelse(pred_test$bad <= pred_test$good ,'good', 'bad'))
  library(RColorBrewer)
  library(rattle)
  library(rpart.plot)
  fancyRpartPlot(model)
  testAcc <- confusionMatrix(pred_test$class, test_train$test$class)$overall
  trainAcc <- confusionMatrix(pred_train$class, test_train$train$class)$overall
  list(trainAcc = trainAcc, testAcc = testAcc)
}


create_final_table <- function(test_train, splits= c('gini', 'information'), min_splits = c(10,100), cp = 0.02)
  {
  results <- data.frame(     Split_Method=character(), 
                             Min_splits=integer(),
                             cp=double(),
                             trainAcc = double(),
                             testAcc = double(),
                             stringsAsFactors=FALSE) 
  for (split in splits) {
    for (min_split in min_splits){
      acc <- build_tree(test_train, split, min_split, cp)
      observation <- data.frame(Split_method = split, Min_splits = min_split,
                       cp = cp, trainAcc = acc$trainAcc[1], testAcc = acc$testAcc[1], stringsAsFactors =  FALSE)
      rownames(observation) <- NULL
      results <- rbind(results, observation)
      }
  }
  results
}

german_credit <- load_and_parse_data('./GermanCredit.xlsx')
test_train <- split_train_test(german_credit)
final_table <- create_final_table(test_train)
