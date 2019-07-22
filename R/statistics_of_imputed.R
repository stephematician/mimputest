#' Calculate statistics of imputed data 
#'
#' Calculates selected statistics (measures) of imputed data
#'
#' Calculates statistics or measures for each imputed variable according to its
#' type, which is one of categorical, ordinal or continuous:
#' \describe{
#'   \item{categorical}{
#'     calculate the \code{\link{entropy}} of non-numeric or integer data;
#'   }
#'   \item{ordinal}{
#'     calculate the dispersion using \code{\link{leiks_D}} of ordered or
#'     integer data;
#'   }
#'   \item{continuous}{
#'     calculate the mean and variance of numeric data;
#'   }
#' }
#'
#' These statistics are intended to be used to monitor convergence of the
#' missForest or MICE procedure.
#'
#' @param imputed named list; each item in the last contains the imputed values
#'            of the named item.
#' @return data.frame; a data.frame with three columns;
#'             \describe{
#'                 \item{\code{variable}}{name of the imputed data as a factor;}
#'                 \item{\code{value}}{value of the measure/statistic;}
#'                 \item{\code{measure}}{the name of the measure as a factor,
#'                     e.g. 'entropy'.}
#'               }
#'
#' @seealso entropy leiks_D
#'
#' @keywords internal
statistics_of_imputed <- function(imputed) {

    aggregate_of_imputed <- function(to_use, aggregator)
        do.call(rbind,
                mapply(data.frame,
                       variable=to_use,
                       value=lapply(imputed[to_use], eval(sym(aggregator))),
                       MoreArgs=list(measure=aggregator),
                       SIMPLIFY=F, USE.NAMES=F))

    cat_data <- names(imputed)[!sapply(imputed, is.numeric) |
                                   !!sapply(imputed, is.integer)]
    ord_data <- names(imputed)[!!sapply(imputed, is.ordered) | 
                                   !!sapply(imputed, is.integer)]
    cts_data <- names(imputed)[!!sapply(imputed, is.numeric) &
                                   !sapply(imputed, is.integer)]

    rbind(aggregate_of_imputed(cat_data, 'entropy'),
          aggregate_of_imputed(ord_data, 'leiks_D'),
          aggregate_of_imputed(cts_data, 'var'),
          aggregate_of_imputed(cts_data, 'mean'))

}

