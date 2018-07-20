# Check conditions on initial state
is_valid_initial_state <- function(X_init, X, msgs=list()) {

    if (any(is.na(X_init)))
        msgs <- c(msgs, 'initial state contains missing data.')

    attr_equal <- attr.all.equal(X_init, X)
    if (!is.null(attr_equal))
        return(c(msgs,
                 paste0('initial state and data differ in attributes:\n',
                        paste('  - ', attr_equal, '.', collapse='\n'))))

    cols <- intersect(names(X_init), names(X))
    column_attr_equal <- mapply(attr.all.equal, X_init[cols], X[cols])
    if (any(!sapply(column_attr_equal, is.null)))
        msgs <- c(msgs,
                  paste0('initial state has column attributes that do not',
                         'match attributes of same column in data:\n  - ',
                         paste(subset(names(column_attr_equal),
                                      !sapply(column_attr_equal, is.null)),
                               collapse=', '), '.'))

    msgs

}

