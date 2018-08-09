# Check conditions of clean.step argument
is_valid_clean_step <- function(X, clean.step, msgs=list()) {

    not_found <- names(clean.step)[!(names(clean.step) %in% names(X))]
    if (length(not_found) > 0)
        msgs <- c(msgs,
                  paste0('clean step names following data not found in X:\n',
                         '  - ', paste0(not_found, collapse=','), '.'))

    not_function <- names(clean.step)[!sapply(clean.step, is.function)]
    if (length(not_function) > 0)
        msgs <- c(msgs,
                  paste0('clean step contains non-function items:\n',
                         '  - ', paste0(not_function, collapse=','), '.'))

    clean.step_ <- clean.step[!!sapply(clean.step, is.function) &
                                  names(clean.step) %in% names(X)]

    not_two_args <- names(clean.step_)[
                        !sapply(clean.step_,
                                function(x) '...' %in% names(formals(x))) &
                        !sapply(clean.step_,
                                function(x) length(formals(x)) >= 2)
                    ]

    if (length(not_two_args) > 0)
        msgs <- c(msgs,
                  paste0('clean step has functions that do not take two',
                         'arguments:\n  - ',
                         paste0(not_two_args, collapse=','), '.'))

    clean.step_ <- clean.step_[!(names(clean.step_) %in% not_two_args)]

    not_handle_empty <- list()
    for (fn in names(clean.step_)) {
        tryCatch({
            empty_result <- identical(clean.step_[[fn]](X[0,], X[[fn]][0]),
                                      X[[fn]][0])
            if (!empty_result)
                not_handle_empty <- c(not_handle_empty, fn)
        },
        error=function(...)
                  not_handle_empty <- c(not_handle_empty, fn)
        )
    }

    if (length(not_handle_empty) > 0)
        msgs <- c(msgs,
                  paste0('clean step has functions that do not handle',
                         'empty data:\n  - ',
                         paste0(not_handle_empty, collapse=','), '.'))

    msgs

}

