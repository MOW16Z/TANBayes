tanBayes = function(data, ...)
  UseMethod("tanBayes")

tanBayes_experimental.default <- function(x, y, laplace = 0, ...) {

#
#   call <- match.call()
#   # get the name of y argument (class vector)
#   Yname <- deparse(substitute(y))
#   # convert x to data.frame
#   x <- as.data.frame(x)

  #conditional mutual information
  cmi <- function(x, y){
    # table containing all attribute combinations indexes
    attrCombinations <- combn(length(x),2)

    #function applied to all combinations
    cmi_ <- function(indexes, x, y){
      Ai = table(x[,indexes[1]],y)
      Aj = table(x[,indexes[2]],y)
      PAi = t((t(Ai)+laplace)/(colSums(Ai)+1))
      PAi = matrix(as.numeric(unlist(PAi)),nrow=nrow(PAi))
      PAj = t((t(Aj)+laplace)/(colSums(Aj)+1))
      PAj = matrix(as.numeric(unlist(PAj)),nrow=nrow(PAj))

      PAij_mul <- mapply(outer, as.data.frame(PAi), as.data.frame(PAj))

      Aijc <- table(x[,indexes[1]], x[,indexes[2]], y)
      #PAijc <- prop.table(Aijc)
      PAijc <- sweep(Aijc+laplace, 1, margin.table(Aijc)+1, "/")
      dim(PAijc) <- c(nrow(PAi)*nrow(PAj), ncol(PAi))

      PAij_cond <- sweep(Aijc+laplace, 3, margin.table(Aijc, 3)+1, "/")
      dim(PAij_cond) <- c(nrow(PAi)*nrow(PAj), ncol(PAi))

      s <- PAijc * log(PAij_cond / PAij_mul)
      sum(s)
    }

    #apply function cmi_ to all possible combinations
    ip <- apply(attrCombinations, 2, cmi_, x=x, y=y)
  }

  ## estimation-function
  est <- function(var) {
    if (is.numeric(var)) {
      simpleError("You can provide only categorical attributes")
    } else {
      # build contingency table
      tab <- table(y, var)
      # return independent probabilities
      (tab + laplace) / (rowSums(tab) + laplace * nlevels(var))
    }
  }

  cmitable <- cmi(x, y)
  # ## create tables
  # # counts of each class
  # apriori <- table(y)
  # aprioriProb <- apriori / sum(apriori)
  # # apply est function to each column (attribute of input data)
  # # returns conditional probabilities
  # tables <- lapply(x, est)


  # ## fix dimname names
  # for (i in 1:length(tables))
  #   names(dimnames(tables[[i]])) <- c(Yname, colnames(x)[i])
  # names(dimnames(apriori)) <- Yname

  # return object of class tanBayes
  structure(list(cmitable = cmitable),

  class = "tanBayes"
  )
}

# naiveBayes.formula <- function(formula, data, laplace = 0, ...,
#                                subset, na.action = na.pass) {
#   call <- match.call()
#   Yname <- as.character(formula[[2]])
#
#   if (is.data.frame(data)) {
#     ## handle formula
#     m <- match.call(expand.dots = FALSE)
#     m$... <- NULL
#     m$laplace = NULL
#     m$na.action <- na.action
#     m[[1]] <- as.name("model.frame")
#     m <- eval(m, parent.frame())
#     Terms <- attr(m, "terms")
#     if (any(attr(Terms, "order") > 1))
#       stop("naiveBayes cannot handle interaction terms")
#     Y <- model.extract(m, "response")
#     X <- m[,-attr(Terms, "response"), drop = FALSE]
#
#     return(naiveBayes(X, Y, laplace = laplace, ...))
#
#   } else if (is.array(data)) {
#     nam <- names(dimnames(data))
#     ## Find Class dimension
#     Yind <- which(nam == Yname)
#
#     ## Create Variable index
#     deps <- strsplit(as.character(formula)[3], ".[+].")[[1]]
#     if (length(deps) == 1 && deps == ".")
#       deps <- nam[-Yind]
#     Vind <- which(nam %in% deps)
#
#     ## create tables
#     apriori <- margin.table(data, Yind)
#     tables <- lapply(Vind,
#                      function(i) (margin.table(data, c(Yind, i)) + laplace) /
#                        (as.numeric(apriori) + laplace * dim(data)[i]))
#     names(tables) <- nam[Vind]
#
#     structure(list(apriori = apriori,
#                    tables = tables,
#                    levels = names(apriori),
#                    call   = call
#     ),
#
#     class = "naiveBayes"
#     )
#   } else stop("naiveBayes formula interface handles data frames or arrays only")
#
# }

print.tanBayes <- function(object, ...) {
  # cat("\nTAN Naive Bayes Classifier for Discrete Predictors\n\n")
  # cat("Call:\n")
  # print(x$call)
  # cat("\nAttributes\n")
  # print(object$attributes)
  # cat("\nParents\n")
  # print(object$parents)
  # cat("\nA-priori probabilities:\n")
  # print(x$apriori / sum(x$apriori))
  # cat("\nConditional Probabilities\n")
  # print(object$conditionalProbabilities)
  # cat("\nModes\n")
  # print(object$modes)
}

# predict.naiveBayes <- function(object, newdata, type = c("class", "raw"), ...) {
#   type <- match.arg(type)
#   newdata <- as.data.frame(newdata)
#   attribs <- match(names(object$tables), names(newdata))
#
#   L <- sapply(1:nrow(newdata), function(i) {
#     ndata <- newdata[i, ]
#     L <- log(object$apriori) + apply(log(sapply(seq_along(attribs),
#             function(v) {
#               nd <- ndata[attribs[v]]
#               if (is.na(nd)) {
#                 #attribute is missing in test set
#                 rep(1, length(object$apriori))
#               }
#               else {
#                 prob <- if (isnumeric[attribs[v]]) {
#                           msd <- object$tables[[v]]
#                           msd[, 2][msd[, 2] <= eps] <- threshold
#                           dnorm(nd, msd[, 1], msd[, 2])
#                         }
#                         else {
#                           object$tables[[v]][, nd]
#                         }
#                 prob[prob <= eps] <- threshold
#
#                 prob
#               }
#             })), 1, sum)
#     if (type == "class"){
#       L
#     }
#     else {
#       sapply(L, function(lp) {
#         1/sum(exp(L - lp))
#       })
#     }
#   })
#   if (type == "class")
#     factor(object$levels[apply(L, 2, which.max)], levels = object$levels)
#   else t(L)
# }
