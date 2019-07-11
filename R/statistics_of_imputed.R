#' Calculate statistics of imputed data 
#'
#' Calculates certain selected statistics (aggregates) of imputed data
#'
#' @param imputed named list;
#'
#' @section To-do:
#' \itemize{
#'   \item fully document \code{statistics_of_imputed}
#'   \item write tests for \code{statistics_of_imputed}
#' }
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

