# Check conditions on order.impute variable in call to smirf
is_valid_order_impute <- function(X, order.impute, msgs=list()) {

    if (length(order.impute) == 0)
        msgs <- c(msgs, 'order.impute is empty.')

    not_found <- order.impute[!(order.impute %in% names(X))]
    if (length(not_found) > 0)
        msgs <- c(msgs,
                  paste0('order.impute contains the following name(s) not ',
                         'found in X:\n  - ', paste(not_found, collapse=', '),
                         '.'))

    none_missing <- order.impute[
                        order.impute %in%
                           names(X)[sapply(X, function(x) !any(is.na(x)))]
                    ]
    if (length(none_missing) > 0)
        msgs <- c(msgs,
                  paste0('order.impute specifies the following columns ',
                         'with no missing values X:\n  - ',
                         paste(none_missing, collapse=', '), '.'))

    all_missing <- order.impute[
                       order.impute %in%
                          names(X)[sapply(X, function(x) all(is.na(x)))]
                   ]
    if (length(all_missing) > 0)
        msgs <- c(msgs,
                  paste0('order.impute specifies the following entirely ',
                         'missing column(s) in X:\n  - ',
                         paste(all_missing, collapse=', '), '.'))
    msgs

}

