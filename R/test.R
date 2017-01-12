library(e1071)
library(TANBayes)
chess = read.csv("./data/Chess/kr-vs-kp.data", header = FALSE)
tdata = read.csv("./data/test.txt", header = FALSE)
download.file('http://www.rdatamining.com/data/titanic.raw.rdata','titanic.raw.rdata')
load('titanic.raw.rdata')

names(tdata)[4] <- "Class" # rename class column
levels(tdata$V2) <- c("c", "d")
levels(tdata$V3) <- c("e", "f")

chessData = read.csv("data/kr-vs-kp.data", header = FALSE)
names(chessData)[37] <- "Class" # rename class column

letterData = read.csv("data/letter", header = FALSE)
names(letterData)[1] <- "Class" # rename class column

irisData <- iris
irisData$Sepal.Length <- as.factor(irisData$Sepal.Length)
irisData$Sepal.Width <- as.factor(irisData$Sepal.Width)
irisData$Petal.Length <- as.factor(irisData$Petal.Length)
irisData$Petal.Width <- as.factor(irisData$Petal.Width)
names(irisData)[5] <- "Class" # rename class column

titanicData = titanic.raw
names(titanicData)[1] <- "c" # rename Class to sth else
names(titanicData)[4] <- "Class" # rename class column


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
tanm <- 1
nbm <- 1
# runs all algorithms on given train/test data sets
runTest <- function(data) {
  timeTrain <- timeClassify <- error <- c()

  # run naive Bayes
  timeTrain[["naiveBayes"]] <- measCPUTime(modB <- naiveBayes(Class ~ ., data$train))
  nbm <<- modB
  timeClassify[["naiveBayes"]] <- measCPUTime(resB <- predict(modB, data$test, type = "class"))
  error[["naiveBayes"]] <- sum(resB != data$test$Class) / nrow(data$test)
  print(table(data$test$Class, resB))
  # TODO: TAN Bayes
  timeTrain[["tanBayes"]] <- measCPUTime(modT <- tanBayes(Class ~ ., data$train))
  tanm<<-modT
  timeClassify[["tanBayes"]] <- measCPUTime(resT <- predict(modT, data$test, type = "class"))
  error[["tanBayes"]] <- sum(resT != data$test$Class) / nrow(data$test)
  print(table(data$test$Class, resT))
  return(data.frame(timeTrain, timeClassify, error))
}


# place for test results
results <- list()

# functional test
results[["tdata"]] <- runTest(prepData(titanicData))

# run N times, each time randomly partition data into training/test
for(i in 1:1) {
  results[[paste0("chess",i)]] <- runTest(prepData(chessData))
  #results[[paste0("letter",i)]] <- runTest(prepData(letterData))
}
print(results)

#MIARA JAKOŚCI - F-MIARA:
fMeasure <- function(cm){
  n = sum(cm) # liczba przykładów
  nc = nrow(cm) # liczba klas
  rowsums = apply(cm, 1, sum) # liczba przykładów każdej klasy
  colsums = apply(cm, 2, sum) # liczba przykładów zaklasyfikowanych do każdej klasy
  diag = diag(cm) # liczba poprawnie zaklasyfikowanych przykładów każdej klasy
  precision = diag / colsums
  recall = diag / rowsums
  f = 2 * precision * recall / (precision + recall)
  return(f) #wektor F-miar dla każdej klasy
}

