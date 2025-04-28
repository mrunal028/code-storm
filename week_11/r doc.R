library(mlbench)
library(purrr)
library(caret)
library(xgboost)
library(dplyr)


data("PimaIndiansDiabetes2")
ds <- as.data.frame(na.omit(PimaIndiansDiabetes2))

##fit a logistic regression model to obtain a parametric equation
logmodel <- glm(diabetes ~ .,data = ds, family = "binomial")
summary(logmodel)

cfs <- coefficients(logmodel) ## extract the coefficients
prednames <- variable.names(ds)[-9] ## fetch the names of predictors in a vector
prednames

sz <- 100000000 ## to be used in sampling
##sample(ds$pregnant, size = sz, replace = T)
dfdata <- map_dfc(prednames,
                  function(nm){ 
                    eval(parse(text = paste0("sample(ds$",nm,
                                             ", size = sz, replace = T)")))
                  })
## and combine them into a dataframe
names(dfdata) <- prednames
dfdata

class(cfs[2:length(cfs)])

length(cfs)
length(prednames)
## Next, compute the logit values
pvec <- map((1:8),
            function(pnum){
              cfs[pnum+1] * eval(parse(text = paste0("dfdata$",
                                                     prednames[pnum])))
            }) %>% ## create beta[i] * x[i]
  reduce(`+`) + ## sum(beta[i] * x[i])
  cfs[1] ## add the intercept

## exponentiate the logit to obtain probability values of thee outcome variable
dfdata$outcome <- ifelse(1/(1 + exp(-(pvec))) > 0.5,
                         1, 0)


##XGboost direct:
set.seed(123)

## sample sizes
sizes <- c(100, 1000, 10000, 100000, 1000000, 10000000)

##Creating empty lists to store results
results <- data.frame(Size = integer(), Accuracy = numeric(), Time = numeric())

write.csv(dfdata, paste("data_","10000000",".csv",sep=""), row.names = FALSE)

for (sz in sizes) {
  
  dfsmall <- dfdata[sample(nrow(dfdata), sz), ]
  write.csv(dfsmall, paste("data_",sz,".csv",sep=""), row.names = FALSE)
  
  ##Creating train-test split (80% train, 20% test)
  idx <- createDataPartition(dfsmall$outcome, p = 0.8, list = FALSE)
  train <- dfsmall[idx, ]
  test <- dfsmall[-idx, ]
  
  ##Converting to matrix
  train_matrix <- xgb.DMatrix(data = as.matrix(select(train, -outcome)), label = train$outcome)
  test_matrix <- xgb.DMatrix(data = as.matrix(select(test, -outcome)), label = test$outcome)
  
  ##Timing the model fitting
  start_time <- Sys.time()
  
  ##XGBoost model
  model <- xgboost(data = train_matrix,
                   objective = "binary:logistic",
                   nrounds = 50,
                   verbose = 0)
  
  end_time <- Sys.time()
  
  ##Predictions
  preds <- predict(model, test_matrix)
  pred_labels <- ifelse(preds > 0.5, 1, 0)
  
  ##Calculating accuracy
  acc <- mean(pred_labels == test$outcome)
  
  #$Calculating time taken
  time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  ##Saving results
  results <- rbind(results, data.frame(Size = sz, Accuracy = acc, Time = time_taken))
  
  print(paste("Done for size", sz))
}

results


##XGBoost - Caret:
set.seed(123)

sizes <- c(100, 1000, 10000, 100000, 1000000, 10000000)

results <- data.frame(Size = integer(), Accuracy = numeric(), Time = numeric())

for (sz in sizes) {
  
  # Sample data
  dfsmall <- dfdata[sample(nrow(dfdata), sz), ]
  
  # Define training control: 5-fold CV
  trctrl <- trainControl(method = "cv", number = 5)
  
  # Timing
  start_time <- Sys.time()
  
  # Train model using caret with method = "xgbTree"
  model <- train(as.factor(outcome) ~ ., data = dfsmall, method = "xgbTree", trControl = trctrl, verbose = FALSE)
  
  end_time <- Sys.time()
  
  # Get accuracy (caret gives resample accuracies directly)
  acc <- max(model$results$Accuracy)
  
  # Calculate time
  time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Save results
  results <- rbind(results, data.frame(Size = sz, Accuracy = acc, Time = time_taken))
  
  print(paste("Done for size", sz))
}

results
