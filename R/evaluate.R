library("e1071")
library("rpart")
library("RWeka")
library("C50")

# load data sets
tdata = read.csv("data/test.txt", header = FALSE)
names(tdata)[4] <- "Class" # rename class column

chessData = read.csv("data/kr-vs-kp.data", header = FALSE)
names(chessData)[37] <- "Class" # rename class column

letterData = read.csv("data/letter", header = FALSE)
names(letterData)[1] <- "Class" # rename class column

# helper function to measure CPU time
measCPUTime <- function(expr) {
  time <- system.time(expr, gcFirst = TRUE)
  return(time[1] + time[2])
}

# prepare data for use by algorithms
prepData <- function(data, splitRatio = 0.25) {
  # convert class column to factor (if not factor already)
  data$Class <- as.factor(data$Class)
  
  # split data randomly: (1-splitRatio) -> train, splitRatio -> test
  randomSplit <- runif(nrow(data))
  return(list(train = data[randomSplit >= splitRatio,], test = data[randomSplit < splitRatio,]))
}

# runs all algorithms on given train/test data sets
runTest <- function(data) {
  timeTrain <- timeClassify <- error <- c()

  # run naive Bayes
  timeTrain[["naiveBayes"]] <- measCPUTime(modB <- naiveBayes(Class ~ ., data$train))
  timeClassify[["naiveBayes"]] <- measCPUTime(resB <- predict(modB, data$test, type = "class"))
  error[["naiveBayes"]] <- sum(resB != data$test$Class) / nrow(data$test)
  
  # TODO: TAN Bayes
  
  # run rpart
  timeTrain[["rpart"]] <- measCPUTime(modR <- rpart(Class ~ ., data$train, method = "class"))
  timeClassify[["rpart"]] <- measCPUTime(resR <- predict(modR, data$test, type = "c"))
  error[["rpart"]] <- sum(resR != data$test$Class) / nrow(data$test)
  
  # run J48 (C4.5)
  timeTrain[["J48"]] <- measCPUTime(modJ48 <- rpart(Class ~ ., data$train, method = "class"))
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
results[["tdata"]] <- runTest(prepData(tdata))

# run N times, each time randomly partition data into training/test
for(i in 1:3) {
  results[[paste0("chess",i)]] <- runTest(prepData(chessData))
  results[[paste0("letter",i)]] <- runTest(prepData(letterData))
}

print(results)

### scratchpad ###
# b = cbind(results$tdata["error"], results$chess["error"])
# names(b) <- c("tdata", "chess")
# plot.default(b["C5.0",], b["rpart",], asp = 1, xlim = c(0, 0.5), ylim = c(0, 0.5))
#
## filter out only errors
errors <- Reduce(cbind, Map(function(r) return(r["error"]), results))
names(errors) <- names(results)
print(t(errors))
# plot errors between each pair of algorithms
plotFun <- function(x, y, ...) {
  points(x, y, ...)
  lines(c(0, 0.6), c(0, 0.6), lty = 2) # draw line of equal errors (x=y)
}
pairs(t(errors), xlim = c(0, 0.6), ylim = c(0, 0.6), lower.panel = NULL, upper.panel = plotFun)
