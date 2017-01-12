maxSpanningTree <- function(table){

  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Igraph needed for this function to work. Please install it.",
         call. = FALSE)
  }

  toDirected <- function(table,root) {
    n = ncol(table)
    v = array("new", n)
    res = matrix(0, n, n)
    v[root]= "current"
    repeat {
      curr =  which(v=="current")
      for(i in curr) {
        con = which(table[i,]==1)
        for(j in con) {
          if(v[j]=="new") {
            res[i,j] = 1
            v[j] = "current"
          }
        }
        v[i] = "done"
      }
      if(!"new" %in% v) {
        break
      }
    }
    res
  }

  # take negative values to find maximum spanning tree
  graph = igraph::graph.adjacency(-table, mode = "undirected", weighted = TRUE)
  mst <- igraph::mst(graph = graph, algorithm = "prim")
  g <- igraph::as_adjacency_matrix(mst)
  g <- toDirected(g, 1)
}
