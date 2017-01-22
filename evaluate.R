library("e1071")
library("rpart")
library("RWeka")
library("C50")
library("TANBayes") # our implementation
library("bnlearn")

# load data sets
tdata = read.csv("data/test.txt", header = FALSE)
names(tdata)[4] <- "Class" # rename class column

chessData = read.csv("data/kr-vs-kp.data", header = FALSE)
names(chessData)[37] <- "Class" # rename class column

letterData = read.csv("data/letter", header = FALSE)
names(letterData)[1] <- "Class" # rename class column

adultData = read.csv("data/adult.data", header = FALSE)
names(adultData)[15] <- "Class" # rename class column

# the Gisette dataset turned out to be too large (5000 attributes)
# gisetteAttributes = read.table("data/gisette_train.data")
# gisetteLabels = read.table("data/gisette_train.labels")
# names(gisetteLabels)[1] <- "Class"
# gisetteData <- cbind(gisetteLabels, gisetteAttributes)


# helper function to measure CPU time
measCPUTime <- function(expr) {
  time <- system.time(expr, gcFirst = TRUE)
  return(time[1] + time[2])
}

# prepare data for use by algorithms
prepData <- function(data, splitRatio = 0.25) {
  ## convert class column to factor (if not factor already)
  #data$Class <- as.factor(data$Class)
  
  # convert all columns to factors (if not factor already) - required by bnlearn
  data <- as.data.frame(Map(function(x) return(as.factor(x)), data)) # all columns to factors
  
  # split data randomly: (1-splitRatio) -> train, splitRatio -> test
  randomSplit <- runif(nrow(data))
  return(list(train = data[randomSplit >= splitRatio,], test = data[randomSplit < splitRatio,]))
}

# runs all algorithms on given train/test data sets
runTest <- function(data) {
  timeTrain <- timeClassify <- error <- c()
  
  # run naive Bayes
  timeTrain[["naiveBayes"]] <- measCPUTime(modB <- naiveBayes(Class ~ ., data$train, laplace = 0))
  timeClassify[["naiveBayes"]] <- measCPUTime(resB <- predict(modB, data$test, type = "class"))
  error[["naiveBayes"]] <- sum(resB != data$test$Class) / nrow(data$test)
  
  timeTrain[["naiveBayes-0.1"]] <- measCPUTime(modB <- naiveBayes(Class ~ ., data$train, laplace = 0.1))
  timeClassify[["naiveBayes-0.1"]] <- measCPUTime(resB <- predict(modB, data$test, type = "class"))
  error[["naiveBayes-0.1"]] <- sum(resB != data$test$Class) / nrow(data$test)
  
  timeTrain[["naiveBayes-1"]] <- measCPUTime(modB <- naiveBayes(Class ~ ., data$train, laplace = 1))
  timeClassify[["naiveBayes-1"]] <- measCPUTime(resB <- predict(modB, data$test, type = "class"))
  error[["naiveBayes-1"]] <- sum(resB != data$test$Class) / nrow(data$test)

  
  # run TAN Bayes
  timeTrain[["tanBayes"]] <- measCPUTime(modT <- tanBayes(Class ~ ., data$train, laplace = 0))
  timeClassify[["tanBayes"]] <- measCPUTime(resT <- predict(modT, data$test, type = "class"))
  error[["tanBayes"]] <- sum(resT != data$test$Class) / nrow(data$test)
  
  timeTrain[["tanBayes-0.1"]] <- measCPUTime(modT <- tanBayes(Class ~ ., data$train, laplace = 0.1))
  timeClassify[["tanBayes-0.1"]] <- measCPUTime(resT <- predict(modT, data$test, type = "class"))
  error[["tanBayes-0.1"]] <- sum(resT != data$test$Class) / nrow(data$test)
  
  timeTrain[["tanBayes-1"]] <- measCPUTime(modT <- tanBayes(Class ~ ., data$train, laplace = 1))
  timeClassify[["tanBayes-1"]] <- measCPUTime(resT <- predict(modT, data$test, type = "class"))
  error[["tanBayes-1"]] <- sum(resT != data$test$Class) / nrow(data$test)

  
  # run TAN Bayes from bnlearn
  timeTrain[["tan-bnlearn"]] <- measCPUTime(modBN <- tree.bayes(data$train, "Class"))
  timeClassify[["tan-bnlearn"]] <- measCPUTime(resBN <- predict(modBN, data$test))
  error[["tan-bnlearn"]] <-sum(resBN != data$test$Class) / nrow(data$test)

  
  # run rpart
  timeTrain[["rpart"]] <- measCPUTime(modR <- rpart(Class ~ ., data$train, method = "class"))
  timeClassify[["rpart"]] <- measCPUTime(resR <- predict(modR, data$test, type = "c"))
  error[["rpart"]] <- sum(resR != data$test$Class) / nrow(data$test)

  
  # run J48 (C4.5)
  timeTrain[["J48"]] <- measCPUTime(modJ48 <- J48(Class ~ ., data$train))
  timeClassify[["J48"]] <- measCPUTime(resJ48 <- predict(modJ48, data$test, type = "c"))
  error[["J48"]] <- sum(resJ48 != data$test$Class) / nrow(data$test)
  
  
  # run C5.0
  timeTrain[["C5.0"]] <- measCPUTime(modC50 <- C5.0(Class ~ ., data$train))
  timeClassify[["C5.0"]] <- measCPUTime(resC50 <- predict(modC50, data$test))
  error[["C5.0"]] <- sum(resC50 != data$test$Class) / nrow(data$test)

  
  return(data.frame(timeTrain, timeClassify, error))
}

# place for test results
results <- list()

# functional test
#results[["tdata"]] <- runTest(prepData(tdata))

# run tests partitioning data into training/test
results[["chess"]] <- runTest(prepData(chessData))
results[["letter"]] <- runTest(prepData(letterData))
#results[["adult"]] <- runTest(prepData(adultData))
#results[["gisette"]] <- runTest(prepData(gisetteData))

print(results)

# prepare results for export
for(n in names(results)) {
  results[[n]] <- as.data.frame(t(results[[n]]))
  results[[n]][,"dataset"] <- n
  results[[n]][,"type"] <- row.names(results[[n]])
  #row.names(results[[n]]) <- NULL
}
resultsTable <- Reduce(rbind, results)

# write to file (select proper line)
write.table(resultsTable, file = "data/results", append = TRUE, row.names = FALSE, col.names = FALSE)
#write.table(resultsTable, file = "data/results-0.1", append = TRUE, row.names = FALSE, col.names = FALSE)
