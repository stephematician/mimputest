#' Frequency of imputed values of categorical data
#'
#' Frequency of imputed values of categorical data
#'
#' @param X_predict named list;
#' @param to_categories named list;
#'
#' @section To-do:
#' \itemize{
#'   \item fully document \code{frequency_of_imputed}
#'   \item write tests for \code{frequency_of_imputed}
#' }
#'
#' @keywords internal
frequency_of_imputed <- function(X_predict, to_categories=list()) {

    variable_names_ <- unname(mapply(list,
                                     variable=names(X_predict),
                                     SIMPLIFY=F))

    categories <- lapply(X_predict, levels)
    categories[names(to_categories)] <- lapply(to_categories, names)

    no_categories <- names(categories)[sapply(categories, is.null)]
    if (length(no_categories) > 0)
        stop(paste0('no category labels found for\n  - ',
                    paste0(no_categories, collapse=', '), '.'))

    do.call(rbind,
            # make data frame for each variable
            mapply(function(name_, x)
                       data.frame(name_,
                                  setNames(data.frame(table(x)),
                                           nm=c('category',
                                                'frequency'))),
                   variable_names_,
                   mapply(factor, X_predict, levels=categories, SIMPLIFY=F),
                   SIMPLIFY=F))

}

