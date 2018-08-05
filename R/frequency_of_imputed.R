#' Frequency of imputed values of categorical data
#'
#' Frequency of imputed values of categorical data
#'
#' @param imputed named list;
#' @param to_categories named list;
#'
#' @section To-do:
#' \itemize{
#'   \item fully document \code{frequency_of_imputed}
#'   \item write tests for \code{frequency_of_imputed}
#' }
#'
#' @keywords internal
frequency_of_imputed <- function(imputed, to_categories=list()) {

    variable_names_ <- unname(mapply(list,
                                     variable=names(imputed),
                                     SIMPLIFY=F))

    categories <- lapply(imputed, levels)
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
                   mapply(factor, imputed, levels=categories, SIMPLIFY=F),
                   SIMPLIFY=F))

}

