# Post process a single imputation
post_process_missforest <- function(res, to_categories) {

    iterations_ <- unname(mapply(list,
                                 iteration=seq_len(1L + res$iterations) - 1L,
                                 SIMPLIFY=F))

    aggregates <- mapply(post_process_iteration,
                         imputed=res$imputed,
                         iteration=iterations_,
                         MoreArgs=list(to_categories=to_categories),
                         SIMPLIFY=F)

    # bind all the iterations together
    c(res,
      list(frequency=do.call(rbind,
                             lapply(aggregates, getElement, 'frequency')),
           statistics=do.call(rbind,
                              lapply(aggregates, getElement, 'statistics'))))

}

# Post process a single iteration
post_process_iteration <- function(imputed,
                                   to_categories=list(),
                                   iteration=data.frame(iteration=0)) {

    categorical_data <- names(imputed)[!sapply(imputed, is.numeric) |
                                           !!sapply(imputed, is.integer)]

    list(frequency=data.frame(iteration,
                              frequency_of_imputed(imputed[categorical_data],
                                                   to_categories)),
         statistics=data.frame(iteration,
                               statistics_of_imputed(imputed)))

}
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
#'   of the named item.
#' @return data.frame; a data.frame with three columns;
#' \describe{
#'   \item{`variable`}{name of the imputed data as a factor;}
#'   \item{`value`}{value of the measure/statistic;}
#'   \item{`measure`}{the name of the measure as a factor, e.g. 'entropy'.}
#' }
#'
#' @seealso entropy leiks_D
#'
#' @keywords internal
#' @importFrom rlang sym
#' @md
statistics_of_imputed <- function(imputed) {

    vars <- names(imputed)

    aggregate_of_imputed <- function(each_var, aggregator) do.call(
        rbind,
        mapply(data.frame,
               variable=each_var,
               value=lapply(imputed[each_var], eval(rlang::sym(aggregator))),
               MoreArgs=list(measure=aggregator),
               SIMPLIFY=F, USE.NAMES=F)
    )

    not_numeric_var <- !sapply(imputed, is.numeric)
    not_integer_var <- !sapply(imputed, is.integer)

    cat_vars <- vars[not_numeric_var | !not_integer_var]
    ord_vars <- vars[!!sapply(imputed, is.ordered) | !not_integer_var]
    cts_vars <- vars[!not_numeric_var & not_integer_var]

    rbind(aggregate_of_imputed(cat_vars, 'entropy'),
          aggregate_of_imputed(ord_vars, 'leiks_D'),
          aggregate_of_imputed(cts_vars, 'var'),
          aggregate_of_imputed(cts_vars, 'mean'))

}


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

