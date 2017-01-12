library(e1071)
chess = read.csv("./data/Chess/kr-vs-kp.data", header = FALSE)
tdata = read.csv("./data/test.txt", header = FALSE)
download.file('http://www.rdatamining.com/data/titanic.raw.rdata','titanic.raw.rdata')
load('titanic.raw.rdata')

download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/letter-recognition/letter-recognition.data", "letter")
letter <- read.csv("letter", header = FALSE)


data = letter
set.seed(1235)
sam <- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))
trainData <- data[sam==1,]
testData <- data[sam==2,]
tb <-tanBayes.default(trainData[,-1], trainData[,1], 0.1)
nbClasif <- naiveBayes(trainData[,-1], trainData[,1], 0.1)
testPred <- predict(nbClasif, testData[,-1])
table(testData$V1,testPred)

tantestPred <- predict(tb, testData[,-1])
table(testData$V1,tantestPred)


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

