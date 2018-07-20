#' Calculate statistics of imputed data 
#'
#' Calculates certain selected statistics (aggregates) of imputed data
#'
#' @param X_predict named list;
#'
#' @section To-do:
#' \itemize{
#'   \item fully document \code{statistics_of_imputed}
#'   \item write tests for \code{statistics_of_imputed}
#' }
#'
#' @keywords internal
statistics_of_imputed <- function(X_predict) {

    variable_names_ <- setNames(mapply(list,
                                       variable=names(X_predict),
                                       SIMPLIFY=F),
                                nm=names(X_predict))

    # Aggregation helper - note 
    aggregate_of_imputed <- function(X_predict, to_use, aggregate_imputed)
        do.call(rbind,
                mapply(function(name, x)
                           data.frame(name,
                                      measure=aggregate_imputed,
                                      value=eval(sym(aggregate_imputed))(x)),
                       unname(variable_names_[to_use]),
                       X_predict[to_use],
                       SIMPLIFY=F))

    categorical_data <- names(X_predict)[!sapply(X_predict, is.numeric) |
                                             !!sapply(X_predict, is.integer)]
    ordered_data <- names(X_predict)[!!sapply(X_predict, is.ordered) | 
                                         !!sapply(X_predict, is.integer)]
    continuous_data <- names(X_predict)[!!sapply(X_predict, is.numeric) &
                                            !sapply(X_predict, is.integer)]

    rbind(aggregate_of_imputed(X_predict, categorical_data, 'entropy'),
          aggregate_of_imputed(X_predict, ordered_data, 'leiks_D'),
          aggregate_of_imputed(X_predict, continuous_data, 'var'),
          aggregate_of_imputed(X_predict, continuous_data, 'mean'))

}

