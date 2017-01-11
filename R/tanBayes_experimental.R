tanBayes = function(data, ...)
  UseMethod("tanBayes")

tanBayes_experimental.default <- function(x, y, laplace = 0, ...) {

  #conditional mutual information
  cmi <- function(x, y){
    # table containing all attribute combinations indexes
    attrCombinations <- combn(length(x),2)

    #cmi between two attributes given in indexes variable
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

  cmitable <- cmi(x, y)
  # return object of class tanBayes
  structure(list(cmitable = cmitable),

  class = "tanBayes"
  )
}

