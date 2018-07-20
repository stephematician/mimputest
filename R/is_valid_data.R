# Check conditions on X argument in call to miForang
is_valid_data <- function(X, msgs=list()) {

    unsupported_type <- names(X)[
                            !sapply(X, is.factor) & !sapply(X, is.numeric) &
                                !sapply(X, is.logical)
                        ]
    if (length(unsupported_type) > 0)
        msgs <- c(msgs,
                  paste0('following column(s) in X are unsupported types:\n',
                         '  - ', paste(unsupported_type, collapse=', '), '.'))

    if (!any(is.na(X)))
        msgs <- c(msgs, 'no missing values in X.')

    if (all(is.na(X)))
        msgs <- c(msgs, 'no non-missing values in X.')

    msgs

}

