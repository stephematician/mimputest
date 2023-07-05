# SPDX-FileCopyrightText: 2023 Stephen Wade <stephematician@gmail.com>
# SPDX-License-Identifier: MIT

# Helpers to check arguments in call to mimputest()

check_args <- function(data, model, n, fn_init, stop_measure, loop_limit,
                       overrides, clean_step, keep_forests, verbose) {

    check_arg_types(
        data=data, model=model, n=n, fn_init=fn_init, stop_measure=stop_measure,
        loop_limit=loop_limit, overrides=overrides, clean_step=clean_step,
        keep_forests=keep_forests, verbose=verbose
    )

    msgs <- is_valid_data(data=data)

    if (!is.null(model))
        msgs %<>% is_valid_model(data=data, model=model, msgs=.)

    msgs %<>% c(
        msg_if_not(length(n) == 1, '\'n\' is not a scalar value'),
        msg_if_not(n > 0, '\'n\' must be non-negative')
    )

    msgs %<>%
        is_valid_fn_init(fn_init=fn_init, msgs=.) %>%
        is_valid_stop_measure(stop_measure=stop_measure, msgs=.)

    msgs %<>% c(
        msg_if_not(loop_limit >= 0), msg_if_not(length(keep_forests) == 1)
    )

    msgs %<>% is_valid_clean_step(data=data, clean_step=clean_step, msgs=.)

    msgs %<>% c(
        msg_if_not(length(verbose) == 1), msg_if_not(length(loop_limit) == 1)
    )

    if (length(msgs) > 0)
        stop('The following pre-conditions failed:\n ',
             paste('-   ', msgs[-length(msgs)], collapse=';\n '),
             '-   ', msgs[length(msgs)], '.')

    invisible()

}


check_arg_types <- function(data, model, n, fn_init, stop_measure, loop_limit,
                            overrides, clean_step, keep_forests, verbose) {

    msgs <- c(msg_if_not(is.data.frame(data)), msg_if_not(is.numeric(n)))

    if (!is.null(model))
        msgs %<>% c(msg_if_not(is.logical(model)), msg_if_not(is.matrix(model)))

    msgs %<>% c(
        msg_if_not(is.function(fn_init)), msg_if_not(is.function(stop_measure)),
        msg_if_not(is.numeric(loop_limit)), msg_if_not(is.list(overrides)),
        msg_if_not(is.list(clean_step)), msg_if_not(is.logical(keep_forests)),
        msg_if_not(is.logical(verbose))
    )

    if (length(msgs > 0))
        stop('The following type requirements failed: ',
             paste(msgs, collapse='; '), '.')

    invisible()

}

