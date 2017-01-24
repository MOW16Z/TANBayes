# run evaluate.R script n times - cannot do all tests in single R process due to C5.0 memleaks
runEval <- function(n)
  for(i in 1:n) {
    shell(cmd = 'Rscript.exe evaluate.R')
    print(i)
  }

# header for result files
# "naiveBayes"     "naiveBayes-0.1" "naiveBayes-1"   "tanBayes"       "tanBayes-0.1"   "tanBayes-1"     "tan-bnlearn"   \
# "rpart"          "J48"            "C5.0"           "dataset"        "type"

colNames <- c("naiveBayes",
              "naiveBayes-0.1",
              "naiveBayes-1",
              "tanBayes",
              "tanBayes-0.1",
              "tanBayes-1",
              "tan-bnlearn",
              "rpart",
              "J48",
              "C5.0",
              "dataset",
              "type")

results <- read.table("data/results")
results0.1 <- read.table("data/results-0.1")
names(results) <- names(results0.1) <- colNames

# select only rows of particular type
filterValuesByType <- function(res = results, type = "error", cols = NULL)
  if(is.null(cols)) res[res$type == type,] else res[res$type == type, cols]

# calculate average over all samples by dataset
averageValuesByDataset <- function(res) {
  lvls <- levels(res$dataset)
  # filter out and average for each dataset
  averaged <- Reduce(rbind, lapply(lvls, function(ds) {
    sub <- subset(res, dataset == ds, select = -c(dataset, type))
    return(colSums(sub) / nrow(sub))
  }))
  # assign correct row names
  row.names(averaged) <- lvls
  return(averaged)
}

# plot alg1 vs alg2
plotVs <- function(res, alg1, alg2) {
  plot(res[, c(alg1, alg2)] * 100, xlim = c(0, 100), ylim = c(0, 100))
  lines(c(0, 100), c(0, 100), lty = 2) # draw line of equal errors (x=y)
}

es <- filterValuesByType(res = results, type = "error")
esAvg <- averageValuesByDataset(es)

print(t(round(esAvg * 100, digits = 2))) # in % and nicely rounded

plotVs(es, "tanBayes", "naiveBayes")
plotVs(es, "tanBayes", "tan-bnlearn")
plotVs(es, "tanBayes", "tanBayes-1")
plotVs(es, "tanBayes", "rpart")
plotVs(es, "tanBayes", "C5.0")
plotVs(es, "naiveBayes", "naiveBayes-0.1")

print(t(round(averageValuesByDataset(filterValuesByType(res = results, type = "timeTrain")), digits = 2)))
print(t(round(averageValuesByDataset(filterValuesByType(res = results, type = "timeClassify")), digits = 2)))

### scratchpad ###
# b = cbind(results$tdata["error"], results$chess["error"])
# names(b) <- c("tdata", "chess")
# plot.default(b["C5.0",], b["rpart",], asp = 1, xlim = c(0, 0.5), ylim = c(0, 0.5))
#
## filter out only errors
# errors <- Reduce(cbind, Map(function(r) return(r["error"]), results))
# names(errors) <- names(results)
# print(t(errors))
# # plot errors between each pair of algorithms
# plotFun <- function(x, y, ...) {
#   points(x, y, ...)
#   lines(c(0, 1), c(0, 1), lty = 2) # draw line of equal errors (x=y)
# }
# pairs(t(errors), xlim = c(0, 1), ylim = c(0, 1), lower.panel = NULL, upper.panel = plotFun)