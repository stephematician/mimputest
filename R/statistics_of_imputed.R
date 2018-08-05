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

    variable_names_ <- setNames(mapply(list,
                                       variable=names(imputed),
                                       SIMPLIFY=F),
                                nm=names(imputed))

    # Aggregation helper - note 
    aggregate_of_imputed <- function(to_use, aggregate_imputed, imputed=imputed)
        do.call(rbind,
                mapply(function(name, x)
                           data.frame(name,
                                      measure=aggregate_imputed,
                                      value=eval(sym(aggregate_imputed))(x)),
                       unname(variable_names_[to_use]),
                       imputed[to_use],
                       SIMPLIFY=F))

    categorical_data <- names(imputed)[!sapply(imputed, is.numeric) |
                                           !!sapply(imputed, is.integer)]
    ordered_data <- names(imputed)[!!sapply(imputed, is.ordered) | 
                                       !!sapply(imputed, is.integer)]
    continuous_data <- names(imputed)[!!sapply(imputed, is.numeric) &
                                          !sapply(imputed, is.integer)]

    rbind(aggregate_of_imputed(categorical_data, 'entropy'),
          aggregate_of_imputed(ordered_data, 'leiks_D'),
          aggregate_of_imputed(continuous_data, 'var'),
          aggregate_of_imputed(continuous_data, 'mean'))

}

