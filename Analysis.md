Analysis
========

Goals of the analysis
---------------------

The goal of the analysis is to predict how well an experiment
participant is doing an exercise based on a number of variables like
positions and time.

Libraries used
--------------

    set.seed(123)
    suppressMessages(library(caret))
    suppressMessages(library(splines))
    suppressMessages(library(kernlab))
    suppressWarnings(library(data.table))
    suppressMessages(suppressWarnings(library(randomForest)))

There are additional libraries used for each model, so if you are
running this you might have to install additional packages.

Loading the data and splitting it
---------------------------------

We load just the traning data set for now. It is split into training and
testing data set.

    training <- read.csv("Data/pml-training.csv", stringsAsFactors = FALSE)
    #Split it into test and train
    sample_size <- floor(0.7*nrow(training))
    training_indices <- sample(nrow(training),sample_size)

    train_2 <- training[training_indices,]
    test_2 <- training[-training_indices,]

Preprocessing
-------------

In order to make the data set more manageable we:

-   remove the columns which holds row numbers

-   remove all the near zero variables

-   do principal component analysis to lower the number of variables we
    have to deal with

-   transform the timestamp variables from character to timestamp

<!-- -->

    train_2 <- train_2[,-1]

    #Remove near zero 
    nzv <- nearZeroVar(train_2, saveMetrics = TRUE)
    to_remove <- which(nzv$nzv)
    train_2 <- train_2[,-to_remove]
    to_keep <- lapply( train_2, function(x) sum(is.na(x)) / length(x) ) < 0.1
    train_2 <- train_2[ to_keep]

    # Principal Component Analysis
    preProc <- preProcess(train_2, method = "pca", pcaComp = 10)
    todo <- predict(preProc, train_2)

    # Making the variables friendlier
    todo$user_name <- as.factor(todo$user_name)
    todo$classe <- train_2$classe

    #Creating useful timestamp
    todo$cvtd_timestamp <- as.POSIXct(strptime(todo$cvtd_timestamp, format = "%d/%m/%Y %H:%M"))
    todo$classe <- factor(as.character(todo$classe))

Model Selection
---------------

I found 4 models to run, which worked fine with the current selection of
numeric and factor variables. They were run with the idea of later
selecting the best ones. The Rpart method and the Ctree method were the
worst performing ones, thus they were removed from the final prediction.

    todo <- data.table(todo)
    model <- randomForest(as.factor(classe)~., data=todo)
    model2 <- train(classe ~., data = todo, method = "treebag")

    insample <- predict(model, newdata = todo[,-3])
    insample2 <- predict(model2, newdata = todo[,-3])

Initial model evaluation
------------------------

    table(insample==todo$classe)/nrow(todo)

    ## 
    ## TRUE 
    ##    1

    table(insample2==todo$classe)/nrow(todo)

    ## 
    ##        FALSE         TRUE 
    ## 0.0001456134 0.9998543866

Test model preprocessing
------------------------

    test_2 <- test_2[,-1]
    test_2 <- test_2[, -to_remove]
    test_2 <- test_2[,to_keep]
    todo_test <- predict(preProc, newdata = test_2)
    todo_test$user_name <- as.factor(todo_test$user_name)
    todo_test$cvtd_timestamp <- as.POSIXct(strptime(todo_test$cvtd_timestamp, format = "%d/%m/%Y %H:%M"))

Run models and Cross validaton
------------------------------

    firstPrediciton <- predict(model, newdata = todo_test[,-3])
    secondPrediction <- predict(model2, newdata = todo_test[,-3])

    todo_test <- cbind(todo_test, firstPrediciton)
    todo_test <- cbind(todo_test, secondPrediction)

    predDF <- data.frame(firstPrediciton, secondPrediction, classe = todo_test$classe)
    trainControlFunc <- trainControl(method = "cv", number=10)
    suppressWarnings(combModel <- train(classe~., method="C5.0", data=predDF, trainControl = trainControlFunc))
    combPrediction <- predict(combModel, newdata = predDF)
    print(combModel)

    ## C5.0 
    ## 
    ## 5887 samples
    ##    2 predictor
    ##    5 classes: 'A', 'B', 'C', 'D', 'E' 
    ## 
    ## No pre-processing
    ## Resampling: Bootstrapped (25 reps) 
    ## Summary of sample sizes: 5887, 5887, 5887, 5887, 5887, 5887, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   model  winnow  trials  Accuracy   Kappa    
    ##   rules  FALSE    1      0.9744719  0.9677068
    ##   rules  FALSE   10      0.9739351  0.9670279
    ##   rules  FALSE   20      0.9738792  0.9669572
    ##   rules   TRUE    1      0.9744719  0.9677068
    ##   rules   TRUE   10      0.9742499  0.9674261
    ##   rules   TRUE   20      0.9742499  0.9674261
    ##   tree   FALSE    1      0.9744719  0.9677068
    ##   tree   FALSE   10      0.9739351  0.9670279
    ##   tree   FALSE   20      0.9737305  0.9667691
    ##   tree    TRUE    1      0.9744719  0.9677068
    ##   tree    TRUE   10      0.9741942  0.9673555
    ##   tree    TRUE   20      0.9741942  0.9673555
    ## 
    ## Accuracy was used to select the optimal model using  the largest value.
    ## The final values used for the model were trials = 1, model = rules
    ##  and winnow = TRUE.

Cross validation was run on the last model that predicts. From it we can
see that expected accuracy is around 97.5%, meaning the out of sample
error should be a little more than 2.5%. And from the following results
we can see that it shares first place with the first model (random
forest), but I chose it because I expect blends to be better than the
single one in predicting for a new dataset.

    sum(firstPrediciton != todo_test$classe)/nrow(todo_test)

    ## [1] 0.02531001

    sum(secondPrediction != todo_test$classe)/nrow(todo_test)

    ## [1] 0.04229659

    sum(combPrediction != todo_test$classe)/nrow(todo_test)

    ## [1] 0.02531001

Run models on new data set
--------------------------

### Read in

    results <- read.csv("Data/pml-testing.csv", stringsAsFactors = FALSE)

### PreProcess

We do the same things over here that we did on the other two data sets

    results <- results[,-1]
    results <- results[,-to_remove]
    results <- results[,to_keep]
    todo_results <- predict(preProc, newdata = results)
    todo_results$user_name <- as.factor(todo_results$user_name)
    todo_results$cvtd_timestamp <- as.POSIXct(strptime(todo_results$cvtd_timestamp, format = "%d/%m/%Y %H:%M"))

### Model Application

    firstVal <- predict(model, newdata = todo_results)
    secondVal <- predict(model2, newdata = todo_results)
    predVDF <- data.frame(firstPrediciton = firstVal, secondPrediction = secondVal)
    combPredV <- predict(combModel, newdata = predVDF)
    todo_results <- cbind(todo_results, combPredV)
