tanBayes = function(data, ...)
  UseMethod("tanBayes")

tanBayes.default <- function(x, y, laplace = 0, ...) {
  call <- match.call()
  Yname <- deparse(substitute(y))
  x <- as.data.frame(x)

  ## estimation-function
  est <- function(var){
      tab <- table(y, var)
      (tab + laplace) / (rowSums(tab) + laplace * nlevels(var))
    }

  ## create tables
  apriori <- table(y)
  tables <- lapply(x, est)
  classProb <- apriori / sum(apriori)
  cmitable <- cmitable(x, y, laplace)
  mst <- maxSpanningTree(cmitable)

  conditionalProb = list()
  for (j in 1:dim(x)[2]) {
    for (i in 1:dim(x)[2]) {
      if(mst[i,j] == 0) {
        conditionalProb = c(conditionalProb,0)
      }
      else {
        Aij = x[,c(i,j)]
        Aijc = table(Aij[,1],Aij[,2],y)
        # divide each element by sum of all elements (with smoothing)
        # corresponds to P(Ai, Aj, c)
        PAijc <- sweep(Aijc+laplace, 1, margin.table(Aijc)+laplace*nlevels(as.factor(Aijc)), "/")
        conditionalProb = c(conditionalProb, list(PAijc))
      }
    }
  }

  # fix dimname names
  for (i in 1:length(tables))
    names(dimnames(tables[[i]])) <- c(Yname, colnames(x)[i])
  names(dimnames(apriori)) <- Yname

  # return object of class tanBayes
  structure(list(apriori = apriori,
                 tables = tables,
                 levels = levels(y),
                 classProb=classProb,
                 cmitable = cmitable,
                 mst = mst,
                 conditionalProb = conditionalProb,
                 call = call),
  class = "tanBayes"
  )
}

predict.tanBayes <- function(object,
                               newdata,
                               type = c("class", "raw"),
                               threshold = 0.001,
                               eps = 0,
                               ...) {


  type <- match.arg(type)
  newdata <- as.data.frame(newdata)
  attribs <- match(names(object$tables), names(newdata))
  isnumeric <- sapply(newdata, is.numeric)
  classC = length(object$levels)
  attrC = ncol(object$mst)

  #transponowane drzewo polączeń
  transDirectedTree=t(object$mst)

  dimNum <- function(nameList, name) {
    i=0
    repeat {
      i <- i+1
      if(nameList[i] == name){
        break;
      }
      if(i>=length(nameList)){
        # attr value didnt occure in training data
        i = -1
        break;
      }
    }
    i
  }

  estProb <- function(c,vec) {
    temp = log(object$classProb[c])
    for(i in 1:length(vec)) {
      if(max(transDirectedTree[i,])==0){
        apriori = as.matrix(object$tables[[i]])
        dimNamesI=dimnames(apriori)[[2]] # list of attribute values' names
        ii = dimNum(dimNamesI,vec[i])
        if(ii>0){
          temp = temp + log(apriori[c,ii])
        }
        else{
          temp = temp + log(threshold)
        }
      }
      else {
        j = which(transDirectedTree[i,]>0)
        pos = (i-1)*attrC+j
        arrProb = object$conditionalProb[[pos]]
        dimNamesI=dimnames(arrProb)[[2]]
        dimNamesJ=dimnames(arrProb)[[1]]
        ii = dimNum(dimNamesI,vec[i])
        jj = dimNum(dimNamesJ,vec[j])
        if(ii > 0 && jj >0){
          temp = temp + log(arrProb[jj,ii,c])
        }
        else {
          temp = temp + log(threshold)
        }
      }
    }
    temp

  }

  # vec=x[1,]
  estClass <- function (vec) {
    classProb = array(0,classC)
    for(i in 1:classC) {
      classProb[i] = estProb(i,vec)
    }
    maxi = which(max(classProb)==classProb)
    className = dimnames(object$classProb)[[1]][maxi]
    className
  }
  #ret = estClass(x[1,])
  ret = apply(newdata,1,estClass)
  ret = factor(ret,levels= dimnames(object$tables[[1]])[[1]])
  ret
}

