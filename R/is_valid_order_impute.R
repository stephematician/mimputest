# Check conditions on order.impute variable in call to miForang
is_valid_order_impute <- function(X, order.impute, msgs=list()) {

    if (length(order.impute) == 0)
        msgs <- c(msgs, 'order.impute is empty.')

    
    not_found <- order.impute[order.impute %in% names(X)]
    if (length(not_found) > 0)
        msgs <- c(msgs,
                  paste0('order.impute contains the following name(s) not',
                         'found in X:\n  - ', paste(not_found, collapse=', '),
                         '.'))

    all_missing <- order.impute[order.impute %in%
                                    names(X)[sapply(is.na(X), all)]]
    if (length(all_missing) > 0)
        msgs <- c(msgs,
                  paste0('order.impute specifies the following entirely',
                         'missing column(s) in X:\n  - ',
                         paste(all_missing, collapse=', '), '.'))
    msgs

}

