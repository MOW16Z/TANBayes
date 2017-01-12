#conditional mutual information
cmitable <- function(x, y, laplace){

  #cmi between two attributes given in indexes variable
  cmi <- function(indexes, x, y, laplace){
    # attr1 and attr2 contingency tables with class vector y
    Ai = table(x[,indexes[1]],y)
    Aj = table(x[,indexes[2]],y)
    # conditional probabilities of Ai given class y
    # with laplace smoothing
    PAi = t((t(Ai)+laplace)/(colSums(Ai)+laplace*apply(Ai,2,function(v){nlevels(as.factor(v))})))
    # convert to matrix
    PAi = matrix(as.numeric(unlist(PAi)),nrow=nrow(PAi))
    # conditional probabilities of Aj given class y
    # with laplace smoothing
    PAj = t((t(Aj)+laplace)/(colSums(Aj)+laplace*apply(Aj,2,function(v){nlevels(as.factor(v))})))
    # convert to matrix
    PAj = matrix(as.numeric(unlist(PAj)),nrow=nrow(PAj))

    # 2D table of PAi|c * PAj|c with class values as column
    PAij_mul <- mapply(outer, as.data.frame(PAi), as.data.frame(PAj))

    # 3D contingency table of Ai, Aj and class vector
    Aijc <- table(x[,indexes[1]], x[,indexes[2]], y)

    # divide each element by sum of all elements (with smoothing)
    # corresponds to P(Ai, Aj, c)
    PAijc <- sweep(Aijc+laplace, 1, margin.table(Aijc)+laplace*nlevels(as.factor(Aijc)), "/")
    # # without smoothing it would be just:
    # PAijc <- prop.table(Aijc)

    # reshape table to flatten it to 2D
    dim(PAijc) <- c(nrow(PAi)*nrow(PAj), ncol(PAi))

    # same as with PAijc but divides by sums over 3-rd dimension (which is class)
    # this corresponds to P(Ai, Aj|c)
    PAij_cond <- sweep(Aijc+laplace, 3, margin.table(Aijc, 3)+laplace*nlevels(as.factor(Aijc)), "/")
    # reashape
    dim(PAij_cond) <- c(nrow(PAi)*nrow(PAj), ncol(PAi))

    s <- PAijc * log(PAij_cond / PAij_mul)
    sum(s)
  }

  # table containing all attribute combinations indexes
  attrCombinations <- combn(length(x),2)

  table <- matrix(0, ncol(x), ncol(x))
  for(i in 1:ncol(attrCombinations)){
    k = attrCombinations[,i]
    table[k[1], k[2]] <- cmi(k,x, y, laplace)
  }
  table[lower.tri(table)] <- t(table[upper.tri(table)])
  table
}
