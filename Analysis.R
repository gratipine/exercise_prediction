# set.seed(123)
# library(caret)
# library(splines)
# library(kernlab)
# library(data.table)
# library(ggplot2)
# library(randomForest)

# Read in data

training <- read.csv("Data/pml-training.csv", stringsAsFactors = FALSE)
#Split it into test and train
sample_size <- floor(0.7*nrow(training))
training_indices <- sample(nrow(training),sample_size)

train_2 <- training[training_indices,]
test_2 <- training[-training_indices,]

#remove all near zero variables
train_2 <- train_2[,-1]
nzv <- nearZeroVar(train_2, saveMetrics = TRUE)
to_remove <- which(nzv$nzv)
train_2 <- train_2[,-to_remove]
to_keep <- lapply( train_2, function(x) sum(is.na(x)) / length(x) ) < 0.1
train_2 <- train_2[ to_keep]

# Principal Component Analysis
preProc <- preProcess(train_2, method = "pca", pcaComp = 4)
todo <- predict(preProc, train_2)

# Making the variables friendlier
todo$user_name <- as.factor(todo$user_name)
todo$classe <- train_2$classe

# Creating Dummy variables out of the factor variables
dummies <- dummyVars(classe ~ user_name, data = todo)
am <- predict(dummies, newdata = todo)
todo <- cbind (todo, data.table(am))
todo <- subset(todo, select = -user_name)

#Creating useful timestamp
todo$cvtd_timestamp <- as.POSIXct(strptime(todo$cvtd_timestamp, format = "%d/%m/%Y %H:%M"))
todo$classe <- factor(as.character(todo$classe))

#The model
todo <- data.table(todo)
model <- randomForest(as.factor(classe)~., data=todo)
model2 <- train(classe ~., data = todo, method = 'rpart')
model3 <- train(classe ~., data = todo, method = "treebag")
model4 <- train(classe ~., data = todo, method = "ctree")
# model5 <- train(classe ~., data = todo, method = "randomGLM")


# Transformations on the test set 
test_2 <- test_2[,-1]
test_2 <- test_2[, -to_remove]
test_2 <- test_2[to_keep]
todo_test <- predict(preProc, newdata = test_2)
todo_test$user_name <- as.factor(todo_test$user_name)
am <- predict(dummies, newdata = todo_test)
todo_test <-cbind(todo_test, data.table(am))
todo_test <- subset(todo_test, select = - user_name)
todo_test$cvtd_timestamp <- as.POSIXct(strptime(todo_test$cvtd_timestamp, format = "%d/%m/%Y %H:%M"))

tuuuduuuu <- predict(model, newdata = todo_test)
secondPred <- predict(model2, newdata = todo_test)
thirdPred <- predict(model3, newdata = todo_test)
fourthPred <- predict(model4, newdata = todo_test)

todo_test <- cbind(todo_test, tuuuduuuu)
todo_test <- cbind(todo_test, secondPred)
todo_test <- cbind(todo_test, thirdPred)
todo_test <- cbind(todo_test, fourthPred)



predDF <- data.frame(tuuuduuuu, secondPred, thirdPred, fourthPred, classe = todo_test$classe)
combModel <- train(classe~., method="C5.0", data=predDF)
combPrediction <- predict(combModel, newdata = predDF)

sum((tuuuduuuu != todo_test$classe))
sum(secondPred != todo_test$classe)
sum(thirdPred != todo_test$classe)
sum(fourthPred != todo_test$classe)
sum(combPrediction != todo_test$classe)/nrow(todo_test)


pimpim <- predict(model, newdata = todo)
pimpim2 <- predict(model2, newdata = todo)
pimpim3 <- predict(model3, newdata = todo)
pimpim4 <- predict(model4, newdata = todo)