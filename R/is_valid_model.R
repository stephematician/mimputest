# Check conditions on model variable in call to smirf
is_valid_model <- function(X, model, msgs=list()) {

    if (nrow(model) == 0)
        msgs <- c(msgs, 'model is empty.')

    all_names <- union(rownames(model), colnames(model))

    not_found <- all_names[!all_names %in% names(X)]
    if (length(not_found) > 0)
        msgs <- c(msgs,
                  paste0('order.impute contains the following name(s) not ',
                         'found in X:\n  - ', paste(not_found, collapse=', '),
                         '.'))

    none_missing <- rownames(model)[
                        rownames(model) %in% names(X)[!apply(is.na(X), 2, any)]
                    ]
    if (length(none_missing) > 0)
        msgs <- c(msgs,
                  paste0('model specifies the following columns with no',
                         'missing values X:\n  - ',
                         paste(none_missing, collapse=', '), '.'))

    all_missing <- all_names[all_names %in% names(X)[apply(is.na(X), 2, all)]]
    if (length(all_missing) > 0)
        msgs <- c(msgs,
                  paste0('model specifies the following entirely missing ',
                         'column(s) in X:\n  - ',
                         paste(all_missing, collapse=', '), '.'))
    msgs

}

