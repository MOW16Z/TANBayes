#' Conditional mutual information table
#'
#' @param x
#'    Input data
#' @param y
#'    Class vector of the same length as data
#' @param laplace
#'    Laplace smoothing parameter
#' @return
#'    Returns table containing conditional mutial information between each attribute
#' @export
#' @examples
cmitable <- function(x, y) {

    # cmi between two attributes given in indexes variable
    cmi <- function(indexes, x, y) {
        # attr1 and attr2 contingency tables with class vector y
        Ai = table(x[, indexes[1]], y)
        Aj = table(x[, indexes[2]], y)
        # conditional probabilities of Ai given class y
        PAi = t(t(Ai)/colSums(Ai))
        # convert to matrix
        PAi = matrix(as.numeric(unlist(PAi)), nrow = nrow(PAi))

        # conditional probabilities of Aj given class y
        PAj = t(t(Aj)/colSums(Aj))
        # convert to matrix
        PAj = matrix(as.numeric(unlist(PAj)), nrow = nrow(PAj))


        PAj[is.nan(PAj)] <- 0
        PAi[is.nan(PAi)] <- 0

        # 2D table of PAi|c * PAj|c with class values as column
        PAij_mul <- mapply(outer, as.data.frame(PAi), as.data.frame(PAj))

        # 3D contingency table of Ai, Aj and class vector
        Aijc <- table(x[, indexes[1]], x[, indexes[2]], y)

        # divide each element by sum of all elements corresponds to P(Ai, Aj, c)
        PAijc <- sweep(Aijc, 1, margin.table(Aijc), "/")
        # # without smoothing it would be just: PAijc <- prop.table(Aijc)

        # reshape table to flatten it to 2D
        dim(PAijc) <- c(nrow(PAi) * nrow(PAj), ncol(PAi))

        # same as with PAijc but divides by sums over 3-rd dimension (which is class) this corresponds to P(Ai, Aj|c)
        PAij_cond <- sweep(Aijc, 3, margin.table(Aijc, 3), "/")
        # reashape
        dim(PAij_cond) <- c(nrow(PAi) * nrow(PAj), ncol(PAi))

        s <- PAijc * log(PAij_cond/PAij_mul)
        s[is.nan(s)] <- 0
        sum(s)
    }

    # table containing all attribute combinations indexes
    attrCombinations <- combn(length(x), 2)
    table <- matrix(0, nrow=ncol(x), ncol=ncol(x))
    for (i in 1:ncol(attrCombinations)) {
        k = attrCombinations[, i]
        table[k[1], k[2]] <- cmi(k, x, y)
    }
    table <- table + t(table)
    table
}
