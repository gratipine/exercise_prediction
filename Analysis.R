set.seed(123)
library(caret)

# Read in data


training <- read.csv("Data/pml-training.csv", stringsAsFactors = FALSE)
#Split it into test and train
sample_size <- floor(0.7*nrow(training))
training_indices <- sample(nrow(training),sample_size)

train_2 <- training[training_indices,]
test_2 <- training[-training_indices,]
#remove all near zero variables
nzv <- nearZeroVar(train_2, saveMetrics = TRUE)
train_2 <- train_2[,-1]
to_remove <- which(nzv$nzv)
train_2 <- train_2[,-to_remove]

todo <- as.Date.POSIXct(train_2$raw_timestamp_part_1)
POSIX
# try to create some composite variables
# get some covariance?