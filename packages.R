# install missing packages
packageNames = c("rpart", "e1071", "C50", "RWeka", "devtools", "igraph")

check.inst <- packageNames %in% row.names(installed.packages())
for(p in packageNames[!check.inst]) {
  install.packages(p)
}

devtools::install("package") # install TANBayes
