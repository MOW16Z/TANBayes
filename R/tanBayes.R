
#' Tree Augmented Naive (TAN) Bayes classifier
#'
#' @param data
#' @keywords TAN Bayes
#' @examples
#' tanBayes(data)
tanBayes <- function(data, colClass = -1) {
  n <- nrow(data) # num. of samples
  numAttr <- ncol(data) - 1 # num. of attributes = num. columns minus one for class
  
  if(colClass < 0) colClass <- ncol(data) # default: last column
  
  # separate class column from data
  dataClass <- data[,colClass]
  dataAttr <- data[,-colClass]
  
  # function for conditional mutual information
  condMutualInformation <- function(i, j) {
    sum <- 0
    
    # for each class
    for(c in levels(as.factor(dataClass))) {
      dataSubset <- dataAttr[dataClass == c,]
      n_c <- nrow(dataSubset)
      
      # for each Xi value
      for(xi in levels(as.factor(dataAttr[,i]))) {
        # for each Xj value
        for(xj in levels(as.factor(dataAttr[,j]))) {
          # counting of matching samples
          n_xi_xj_c <- sum(dataSubset[,i] == xi & dataSubset[,j] == xj)
          n_xi_c <- sum(dataSubset[,i] == xi)
          n_xj_c <- sum(dataSubset[,j] == xj)
          
          # probability estimation
          P_xi_xj_c <- (n_xi_xj_c + 0.1) / (n + 1)
          P_xi_xj_cond <- (n_xi_xj_c + 0.1) / (n_c + 1)
          P_xi_cond <- (n_xi_c + 0.1) / (n_c + 1)
          P_xj_cond <- (n_xj_c + 0.1) / (n_c + 1)
          # TODO: better smoothing
          
          sum <- sum + P_xi_xj_c * log(P_xi_xj_cond / P_xi_cond / P_xj_cond)
        }
      }
    }
    return(sum)
  }
  
  # calculate mutual information for each pair of attributes
  attrMutualInformation <- matrix(data = 0, nrow = numAttr, ncol = numAttr)
  for(i in 1:numAttr)
    for(j in 1:i)
      if(i != j)
        attrMutualInformation[i,j] <- attrMutualInformation[j,i] <- condMutualInformation(i,j)
  
  print(attrMutualInformation)
}
