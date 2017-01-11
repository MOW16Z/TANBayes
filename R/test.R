chess = read.csv("./data/Chess/kr-vs-kp.data", header = FALSE)
tdata = read.csv("./data/test.txt", header = FALSE)
download.file('http://www.rdatamining.com/data/titanic.raw.rdata','titanic.raw.rdata')
load('titanic.raw.rdata')

download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/letter-recognition/letter-recognition.data", "letter")
letter <- read.csv("letter", header = FALSE)

tanBayes(titanic.raw)
data = titanic.raw
data$Survived <- NULL
tanBayes_experimental.default(data, titanic.raw$Survived, 0.1)

tanBayes(letter, 1)
data = letter
data$V1 <- NULL
tanBayes_experimental.default(data, letter$V1, 0.1)
