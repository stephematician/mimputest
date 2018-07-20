#' Map categorical data to values
#'
#' Map categorical data to specified values of new (or same) type.
#'
#' @param X named list or data.frame;
#'            list or data.frame of factor data
#' @param to_categories named list;
#'            each named item is of the value-type to map the item or column
#'            with the same name \code{X} to, with the names of each value being
#'            the label of the (corresponding) factor to map from, e.g. to map
#'            the factor levels 'A' and 'B' to integers 1 and 2 respectively 
#'            for a column called \code{dummy}, then
#'            \code{to_categories=list(dummy=c(A=1L,B=2L))}
#' @return list or data.frame;
#'            list or data.frame of the same shape as \code{X} with factors
#'            mapped to the values specified in \code{to_categories}.
#'
#' @keywords internal
unmap_categories <- function(X, to_categories) {

    X[names(to_categories)] <- mapply(function(map, x)
                                          setNames(map[x], names(x)),
                                      to_categories,
                                      X[names(to_categories)],
                                      SIMPLIFY=F)
    X

}

