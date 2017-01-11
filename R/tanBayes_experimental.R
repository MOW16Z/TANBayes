tanBayes = function(data, ...)
  UseMethod("tanBayes")

tanBayes.default <- function(x, y, laplace = 0, ...) {

  cmitable <- cmitable(x, y, laplace)
  # return object of class tanBayes
  structure(list(cmitable = cmitable),
  class = "tanBayes"
  )
}

