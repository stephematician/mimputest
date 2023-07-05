# SPDX-FileCopyrightText: 2023 Stephen Wade <stephematician@gmail.com>
# SPDX-License-Identifier: MIT

# post-process the imputed values of a whole chain
post_process <- function(result) {

    each_iteration <- mapply(post_process_iteration,
                             imputed=result$imputed,
                             iteration=seq_len(1L + result$iterations) - 1L)

    apply(each_iteration, 1, do.call, what=rbind, simplify=F)

}

post_process_iteration <- function(imputed, iteration) {

    list(statistics=statistics_of_imputed(imputed=imputed, iteration=iteration),
         frequency=frequency_of_imputed(imputed=imputed, iteration=iteration))

}

statistics_of_imputed <- function(imputed, iteration) {

    vars <- names(imputed)

    aggregate_of_imputed <- function(each_var, aggregator) do.call(
        rbind,
        mapply(data.frame,
               variable=each_var,
               value=lapply(imputed[each_var], match.fun(aggregator)),
               MoreArgs=list(measure=aggregator, iteration=iteration),
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

#' @importFrom stats setNames
#' @noRd
frequency_of_imputed <- function(imputed, iteration=NA_integer_) {

    vars <- names(imputed)

    not_numeric_var <- !sapply(imputed, is.numeric)
    not_integer_var <- !sapply(imputed, is.integer)

    cat_vars <- vars[not_numeric_var | !not_integer_var]
    mapply(data.frame,
           variable=cat_vars,
           lapply(imputed[cat_vars], table) %>%
               lapply(data.frame) %>%
               lapply(setNames, nm=c('category', 'frequency')),
           MoreArgs=list(iteration=iteration),
           SIMPLIFY=FALSE, USE.NAMES=FALSE) %>%
    do.call(what=rbind)

}

