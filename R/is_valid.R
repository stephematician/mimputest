# Check conditions on X argument in call to smirf
is_valid_data <- function(X, msgs=list()) {

    unsupported_type <- names(X)[
        !sapply(X, is.factor) & !sapply(X, is.numeric) & !sapply(X, is.logical)
    ]

    msgs %<>% c(
        msg_if_not(
            length(unsupported_type) == 0,
            paste0('following column(s) in \'X\' are unsupported types: ',
                   paste(unsupported_type, collapse=', '))
        ),
        msg_if_not(any(is.na(X)), 'no missing values in \'x\''),
        msg_if_not(!all(is.na(X)), 'no non-missing values in \'X\'')
    )

    invisible(msgs)

}

# Check conditions on model variable in call to smirf
#' @importFrom magrittr %<>%
is_valid_model <- function(X, model, msgs=list()) {

    msgs %<>% c(msg_if_not(nrow(model) > 0, '\'model\' is empty (no rows)'))

    all_names <- union(rownames(model), colnames(model))
    not_found <- all_names[!all_names %in% names(X)]
    none_missing <- rownames(model)[
        rownames(model) %in% names(X)[!apply(is.na(X), 2, any)]
    ]
    all_missing <- all_names[all_names %in% names(X)[apply(is.na(X), 2, all)]]

    msgs %<>% c(
        msg_if_not(
            length(not_found) == 0,
            paste0('\'order.impute\' contains the following name(s) not found ',
                   'in \'X\': ', paste(not_found, collapse=', '))
        ), msg_if_not(
            length(none_missing) == 0,
            paste0('\'model\' specifies the following columns with no missing ',
                   'values in \'X\': ', paste(none_missing, collapse=', '))
        ), msg_if_not(
            length(all_missing) == 0,
            paste0('\'model\' specifies the following entirely missing ',
                   'column(s) in \'X\': ', paste(all_missing, collapse=', '))
        )
    )

    invisible(msgs)

}

# Check conditions on arguments passed to initial state function
is_valid_X_init_fn <- function(X.init.fn, msgs=list()) {

    X_init_formals <- formals(X.init.fn)

    if (length(X_init_formals) != 2)
        msgs <- c(msgs,
                  paste0('X.init.fn takes incorrect (', length(X_init_formals),
                         ') number of arguments.'))

    # really simple run-time checks
    if (!is.data.frame(X.init.fn(data.frame())))
        msgs <- c(msgs,
                  paste('X.init.fn(data.frame()) does not return data.frame.'))

    if (!(nrow(X.init.fn(data.frame(x=numeric(0)))) == 0))
        msgs <- c(msgs,
                  paste('X.init.fn(data.frame(x=numeric(0)) does not return an',
                        'empty data.frame.'))

    if (any(is.na(X.init.fn(data.frame(x=c(NA,0))))))
        msgs <- c(msgs,
                  paste('X.init.fn(data.frame(x=c(NA,0)) returns NA values.'))

    msgs

}


# Check conditions on arguments passed to initial state function
is_valid_stop_measure <- function(stop.measure, msgs=list()) {

    stop_measure_formals <- formals(stop.measure)

    if (length(stop_measure_formals) < 4 &
           any(sapply(stop_measure_formals[-1:-4], identical, expr())))
        msgs <- c(msgs,
                  paste0('stop.measure requires more than 4 arguments'))

    # should consider some run-time tests here

    msgs

}


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


# Check conditions on initial state
is_valid_initial_state <- function(X_init, X, msgs=list()) {

    if (any(is.na(X_init)))
        msgs <- c(msgs, 'initial state contains missing data.')

    # FIXME: test that data.frame and tibble ok?
    attr_equal <- attr.all.equal(X_init, X)
    if (!is.null(attr_equal))
        return(c(msgs,
                 paste0('initial state and data differ in attributes:\n',
                        paste0('  - ', attr_equal, '.', collapse='\n'))))

    cols <- intersect(names(X_init), names(X))
    column_attr_equal <- mapply(attr.all.equal, X_init[cols], X[cols])
    if (any(!sapply(column_attr_equal, is.null)))
        msgs <- c(msgs,
                  paste0('initial state has column attributes that do not',
                         'match attributes of same column in data:\n  - ',
                         paste0(subset(names(column_attr_equal),
                                       !sapply(column_attr_equal, is.null)),
                                collapse=', '), '.'))

    msgs

}


