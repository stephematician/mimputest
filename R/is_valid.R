# SPDX-FileCopyrightText: 2023 Stephen Wade <stephematician@gmail.com>
# SPDX-License-Identifier: MIT

is_valid_data <- function(data, msgs=list()) {

    unsupported_type <- names(data)[
        !sapply(data, is.factor) & !sapply(data, is.numeric) &
        !sapply(data, is.logical)
    ]

    msgs %<>% c(
        msg_if_not(
            length(unsupported_type) == 0,
            paste0('The following column(s) in \'data\' are of unsupported ',
                   'type: ', paste(unsupported_type, collapse=', '))
        ),
        msg_if_not(any(is.na(data)), 'No missing values in \'data\''),
        msg_if_not(!all(is.na(data)), 'No non-missing values in \'data\'')
    )

    invisible(msgs)

}


is_valid_model <- function(data, model, msgs=list()) {

    msgs %<>% c(msg_if_not(nrow(model) > 0, '\'model\' is empty (no rows)'))

    nm <- names(data)
    all_names <- union(rownames(model), colnames(model))
    not_found <- all_names[!all_names %in% names(data)]
    none_missing <- rownames(model)[
        rownames(model) %in% nm[!apply(is.na(data), 2, any)]
    ]
    all_missing <- all_names[all_names %in% nm[apply(is.na(data), 2, all)]]

    msgs %<>% c(
        msg_if_not(
            length(not_found) == 0,
            paste0('\'model\' contains the following name(s) not found ',
                   'in \'data\': ', paste(not_found, collapse=', '))
        ), msg_if_not(
            length(none_missing) == 0,
            paste0('\'model\' specifies the following columns with no missing ',
                   'values in \'data\': ', paste(none_missing, collapse=', '))
        ), msg_if_not(
            length(all_missing) == 0,
            paste0('\'model\' specifies the following entirely missing ',
                   'column(s) in \'data\': ', paste(all_missing, collapse=', '))
        )
    )

    invisible(msgs)

}


is_valid_fn_init <- function(fn_init, msgs=list()) {

    fn_formals <- formals(fn_init)
    expected_formals <- c('data', 'indicator')
    n_expected <- length(expected_formals)

    msgs %<>% c(
        msg_if_not(
            length(fn_formals) >= n_expected,
            paste0('\'fn_init\' should take at least ', n_expected,
                   ' arguments (', length(fn_formals), ' found)')
        ), msg_if_not(
            identical(names(fn_formals)[1:n_expected], expected_formals),
            paste('First arguments of \'fn_init\' must have ',
                  'names:', paste0(expected_formals, collapse=', '))
        )
    )
    tryCatch({
        msgs %<>% c(
            msg_if_not(
                is.data.frame(fn_init(data.frame())),
                paste('\'fn_init(data.frame())\' did not return data.frame')
            ), msg_if_not(
                nrow(fn_init(data.frame(x=numeric()))) == 0,
                paste('\'fn_init(data.frame(x=numeric()))\' did not return an',
                      'empty data.frame')
            )
        )
    }, error=function(e)
        msgs %<>% c('\'fn_init\' failed with zero-row data.frame')
    )
    tryCatch({
        msgs %<>% c(msg_if_not(
            all(!is.na(fn_init(data.frame(x=c(NA,0))))),
            paste('\'fn_init(data.frame(x=c(NA,0)))\' returned NA'))
        )
    }, error=function(e)
        msgs %<>% c('\'fn_init\' failed with single column data.frame')
    )

    invisible(msgs)

}


is_valid_stop_measure <- function(stop_measure, msgs=list()) {

    fn_formals <- formals(stop_measure)
    expected_formals <- c('x_sample', 'y_sample', 'data', 'indicator')
    n_expected <- length(expected_formals)

    msgs %<>% c(
        msg_if_not(
            length(fn_formals) >= n_expected,
            paste0('\'stop_measure\' should take at least ', n_expected,
                   ' arguments (', length(fn_formals), ' found)')
        ), msg_if_not(
            identical(names(fn_formals)[1:n_expected], expected_formals),
            paste('First arguments of \'stop_measure\' must have ',
                  'names:', paste0(expected_formals, collapse=', '))
        )
    )

  # attempt at run-time test that stop_measure works
    data_ <- data.frame(
        x0=numeric(1), x1=factor(letters[1]), x2=ordered(letters[1])
    )

    for (j in seq_along(data_)) {
        type_str <- paste0('data of type \'', class(data_[[j]])[1], '\'')
        tryCatch({
            result <- stop_measure(data_x=data_[,j], data_y=data_[,j],
                                   data_init=data_[,j],
                                   indicator=sapply(data_[,j], is.na))
            msgs %<>% c(msg_if_not(
                is.numeric(result),
                paste('Non-numeric result returned by \'stop_measure\' given',
                       type_str))
            )
        }, error=function(e)
            msgs %<>% c(
                paste('\'stop_measure\' failed with single-row data.frame',
                      'given', type_str)
            )
        )
    }

    invisible(msgs)

}


is_valid_clean_step <- function(data, clean_step, msgs=list()) {

    nm <- names(data)
    not_found <- setdiff(names(clean_step), nm)
    not_function <- names(clean_step)[!sapply(clean_step, is.function)]
    msgs %<>% c(
        msg_if_not(
            length(not_found) == 0,
            paste0('\'clean_step\' contains following names not found \'data\'',
                   ': ', paste0(not_found, collapse=','))
        ),
        msg_if_not(
            length(not_function) == 0,
            paste0('\'clean_step\' contains non-function items: ',
                   paste0(not_function, collapse=', '))
        )
    )

    nm_steps <- setdiff(names(clean_step), union(not_function, not_found))

    expected_formals <- c('data', 'imputed')
    fn_formals <- lapply(clean_step[nm_steps], formals)
    n_expected <- length(expected_formals)

    bad_n_formals <- nm_steps[sapply(fn_formals, length) <= n_expected]
    bad_names_formals <- nm_steps[
        lapply(fn_formals, '[', 1:n_expected) %>% lapply(names) %>%
            lapply(identical, expected_formals)
    ]

    msgs %<>% c(
        msg_if_not(
            length(bad_n_formals) == 0,
            paste0('\'clean_step\' has following items with too few formal ',
                   'arguments: ', paste0(bad_n_formals, collapse=', '))
        ),
        msg_if_not(
            length(bad_n_formals) == 0,
            paste0('\'clean_step\' has following items with incorrect first ',
                   'argument names: ', paste0(bad_names_formals, collapse=', '))
        )
    )

    not_handle_empty <- list()
    for (fn in nm_steps) {
        tryCatch({
            empty_result <- identical(clean_step[[fn]](data[0,], data[[fn]][0]),
                                      data[[fn]][0])
            if (!empty_result) not_handle_empty <- c(not_handle_empty, fn)
        }, error=function(...) not_handle_empty <- c(not_handle_empty, fn))
    }

    msgs %<>% c(
        msg_if_not(
            length(not_handle_empty) == 0,
            paste0('\'clean step\' has functions that do not handle empty ',
                   'data: ', paste0(not_handle_empty, collapse=', '))
        )
    )

    invisible(msgs)

}


is_valid_initial_state <- function(data_j, data, msgs=list()) {

  # TODO: I only check non-missing and attributes here; unsure what else to add.
    attr_equal <- attr.all.equal(data_j, data)

    cols <- intersect(names(data_j), names(data))
    column_attr_equal <- mapply(attr.all.equal, data_j[cols], data[cols])
    is_equal_column_attr <- sapply(column_attr_equal, is.null)

    msgs %<>% c(
        msg_if_not(all(!is.na(data_j)),
                   'Initialised data contains NA (missing) values'),
      # FIXME: test that data.frame and tibble ok?
        msg_if_not(is.null(attr_equal),
                   paste0('Initialised data and \'data\' differ in attributes: ',
                          paste0(attr_equal, collapse=', '))),
        msg_if_not(
            all(is_equal_column_attr),
            paste0('Initialised data has column attributes that do not match',
                   'attributes of same column in \'data\': ',
                   paste0(subset(names(column_attr_equal),
                                 !is_equal_column_attr),
                          collapse=', '))
        )
    )

    invisible(msgs)

}

