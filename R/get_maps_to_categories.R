#' Map from levels proposed by as.factor to each integer and logical column
#'
#' Creates maps from the levels proposed by \code{\link{as.factor}} when it is
#' applied to each integer and logical column in a data.frame. The maps are
#' given in a list with names given by the names of the integer and logical
#' columns in the data set. Each item is either a named logical or named integer
#' vector, with names given by the unique factor levels, and values given by the
#' unique values of the corresponding column.
#'
#' @inheritParams smirf
#' @return named list;
#'            names are given by the column names of \code{X} which have logical
#'            or integer data, and each item is either a named logical or named
#'            integer vector, with names given by the unique factor levels
#'            due to passing the column to \code{\link{as.factor}} and the
#'            values are given by the corresponding unique values of the column,
#'
#' @seealso \code{\link{as.factor}}
#'
#' @keywords internal
get_maps_to_categories <- function(X) {

    to_categorical <- names(X)[sapply(X, is.integer) | sapply(X, is.logical)]

    lapply(X[to_categorical],
           function(x, y=as.factor(x))
               setNames(x[sapply(levels(y), match, table=y)],
                        levels(y)))

}

