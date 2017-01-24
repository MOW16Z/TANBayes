#' TAN Bayes Classifier
#'
#' @description Builds Tree-Augmented-Naive Bayes model from data
#' using calculated conditional mutual information matrix.
#'   
#' @param laplace Laplace smoothing parameter. Default value (0) disables smoothing.
#' @param ... Not used.
#' @return An object of class \code{tanBayes}.
#' 
#' @examples
#' data(iris)
#' model <- tanBayes(Species ~ ., data = iris)
#' table(predict(model, iris), iris$Species)
#' 
#' @export
tanBayes = function(x, ...)
  UseMethod("tanBayes")

#' @describeIn tanBayes TAN Bayes Classifier
#'
#' @param x Input data (a numeric matrix or a data frame of 
#' categorical and/or numeric variables).
#' @param y Class vector.
#' 
#' @export
tanBayes.default <- function(x, y, laplace = 0, ...) {
    call <- match.call()
    Yname <- deparse(substitute(y))
    x <- as.data.frame(x)

    ## estimation-function
    ## same as in naiveBayes
    est <- function(var) {
      if (is.numeric(var)) {
        cbind(tapply(var, y, mean, na.rm = TRUE),
              tapply(var, y, sd, na.rm = TRUE))
      } else {
        tab <- table(y, var)
        (tab + laplace) / (rowSums(tab) + laplace * nlevels(var))
      }
    }

    ## create tables
    apriori <- table(y)
    tables <- lapply(x, est)
    classProb <- apriori/sum(apriori)
    cmitable <- cmitable(x, y)
    mst <- maxSpanningTree(cmitable)

    conditionalProb = list()
    for (j in 1:dim(x)[2]) {
        for (i in 1:dim(x)[2]) {
            if (mst[i, j] == 0) {
                conditionalProb = c(conditionalProb, 0)
            } else {
                Aij = x[, c(i, j)]
                Aijc = table(Aij[, 1], Aij[, 2], y)
                # divide each element by sum of all elements, corresponds to P(Ai| Aj, c)
                PAijc <- sweep(Aijc, c(2,3), margin.table(Aijc, c(2,3)), "/")
                PAijc[is.nan(PAijc)] <- 0
                conditionalProb = c(conditionalProb, list(PAijc))
            }
        }
    }

    # fix dimname names
    for (i in 1:length(tables)) names(dimnames(tables[[i]])) <- c(Yname, colnames(x)[i])
    names(dimnames(apriori)) <- Yname

    # return object of class tanBayes
    structure(list(apriori = apriori, tables = tables, levels = levels(as.factor(y)), classProb = classProb, cmitable = cmitable,
        mst = mst, conditionalProb = conditionalProb, call = call), class = "tanBayes")
}


#' @describeIn tanBayes TAN Bayes Classifier
#'
#' @param formula A formula of the form \code{class ~ x1 + x2 +
#' \dots}. Interactions are not allowed.
#' @param data Input data (a data frame of categorical and/or numeric variables).
#' @param subset Currently not used.
#' @param na.action A function to specify the action to be taken if \code{NA}s are
#' found. The default action is not to count them for the
#' computation of the probability factors. An
#' alternative is na.omit, which leads to rejection of cases
#' with missing values on any required variable. (NOTE: If
#' given, this argument must be named.)
#' 
#' @export
tanBayes.formula <- function(formula, data, laplace = 0, ...,
                               subset, na.action = na.pass) {
  call <- match.call()
  Yname <- as.character(formula[[2]])
  if (is.data.frame(data) || is.array(data)) {
    ## handle formula
    m <- match.call(expand.dots = FALSE)
    m$data <- as.data.frame(data)
    m$... <- NULL
    m$laplace = NULL
    m$na.action <- na.action
    m[[1]] <- as.name("model.frame")
    m <- eval(m, parent.frame())
    Terms <- attr(m, "terms")
    if (any(attr(Terms, "order") > 1))
      stop("TANBayes cannot handle interaction terms")
    Y <- model.extract(m, "response")
    X <- m[,-attr(Terms, "response"), drop = FALSE]
    return(tanBayes(X, Y, laplace = laplace, ...))
  } else stop("TANBayes formula interface handles data frames or arrays only")

}

#' @export
print.tanBayes <- function(x, ...) {
  cat("\nTAN Bayes Classifier for Discrete Predictors\n\n")
  cat("Call:\n")
  print(x$call)
  cat("\nA-priori probabilities:\n")
  print(x$classProb)
  cat("\nConditional probabilities:\n")
  for (i in x$tables) {print(i); cat("\n")}
  cat("\nConditional mutual information table:\n")
  print(x$cmitable)
  cat("\nMaximal spanning tree:\n")
  print(x$mst)
}

#' @describeIn tanBayes Predict function for TAN Bayes
#'
#' @param object An object of class \code{tanBayes}.
#' @param newdata Test data.
#' @param type Currently not used.
#' @param threshold Value replacing cells with probabilities within \code{eps} range.
#' @param eps A double for specifying an epsilon-range to apply Laplace
#' smoothing (to replace zero or close-zero probabilities by \code{theshold}).
#' 
#' @export
predict.tanBayes <- function(object, newdata, type = c("class", "raw"), threshold = 0.001, eps = 0, ...) {

    type <- match.arg(type)
    newdata <- as.data.frame(newdata)
    attribs <- match(names(object$tables), names(newdata))
    isnumeric <- sapply(newdata, is.numeric)
    classC = length(object$levels)
    attrC = ncol(object$mst)
    newdata <- data.matrix(newdata)

    transDirectedTree = t(object$mst)

    L <- sapply(1:nrow(newdata), function(i) {
      ndata <- newdata[i, ]
      L <- log(object$apriori) + apply(log(sapply(seq_along(attribs),
                                                  function(v) {
                                                    nd <- ndata[attribs[v]]
                                                    if (is.na(nd))
                                                      rep(1, length(object$apriori))
                                                    else {
                                                      prob <- if (isnumeric[attribs[v]]) {
                                                        # we dont handle numeric
                                                        # return the same value as naiveBayes
                                                        msd <- object$tables[[v]]
                                                        msd[, 2][msd[, 2] <= eps] <- threshold
                                                        dnorm(nd, msd[, 1], msd[, 2])
                                                      } else{
                                                        if (max(transDirectedTree[v, ]) == 0)
                                                          object$tables[[v]][, nd]
                                                        else{
                                                          # if augmenting edge exists
                                                          # use value from conditionalProb
                                                          # table
                                                          # get parent index
                                                          j = which(transDirectedTree[v, ] > 0)
                                                          # find corresponding table
                                                          pos = (v - 1) * attrC + j
                                                          ndcorr <- ndata[attribs[j]]
                                                          arrProb = object$conditionalProb[[pos]]
                                                          # return corresponding probabilities vector
                                                          arrProb[ndcorr, nd, ]
                                                        }
                                                      }
                                                      prob[prob <= eps] <- threshold
                                                      prob
                                                    }
                                                  })), 1, sum)
      L
    })
    factor(object$levels[apply(L, 2, which.max)], levels = object$levels)
}

