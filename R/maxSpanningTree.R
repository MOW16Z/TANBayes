#' Returns directed maximal spanning tree
#' as adjacency matrix
#'
#' @param table
#'    Adjacency matrix containing weights
#' @return
#'    Adjacency matrix representing directed maximal spanning tree
#' @export
#' @examples
maxSpanningTree <- function(table) {

    if (!requireNamespace("igraph", quietly = TRUE)) {
        stop("Igraph needed for this function to work. Please install it.", call. = FALSE)
    }

    toDirected <- function(table, root) {
        n = ncol(table)
        v = array("new", n)
        res = matrix(0, n, n)
        v[root] = "current"
        repeat {
            curr = which(v == "current")
            for (i in curr) {
                con = which(table[i, ] == 1)
                for (j in con) {
                  if (v[j] == "new") {
                    res[i, j] = 1
                    v[j] = "current"
                  }
                }
                v[i] = "done"
            }
            if (!"new" %in% v) {
                break
            }
        }
        res
    }
    # take negative values to find maximum spanning tree
    table <- -table
    table[table==0] <- Inf
    diag(table) <- 0
    graph = igraph::graph.adjacency(table, mode = "undirected", weighted = TRUE)
    mst <- igraph::mst(graph, algorithm = "prim")
    g <- igraph::as_adjacency_matrix(mst)
    g <- toDirected(g, 1)
}
