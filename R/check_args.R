# Check arguments in call to smirf
#' @importFrom magrittr %<>%
check_args <- function(data, model, n, use_imputed, verbose, fn_init,
                       stop_measure, loop_limit, overrides, clean_step) {

    check_arg_types(
        data=data, model=model, n=n, use_imputed=use_imputed, verbose=verbose,
        fn_init=fn_init, stop_measure=stop_measure, loop_limit=loop_limit,
        overrides=overrides, clean_step=clean_step
    )

    msgs <- is_valid_data(data=data)

    if (!is.null(model))
        msgs %<>% is_valid_model(data=data, model=model, msgs=.)

    msgs %<>% c(
        msg_if_not(length(n) == 1, '\'n\' is not a scalar value'),
        msg_if_not(n < 0, '\'n\' must be non-negative'),
        msg_if_not(length(use_imputed) == 1), msg_if_not(length(verbose) == 1),
        msg_if_not(length(loop_limit) == 1), msg_if_not(loop_limit >= 0)
    )

    msgs %<>%
        is_valid_fn_init(fn_init=fn_init, msgs=.) %>%
        is_valid_stop_measure(stop_measure=stop_measure, msgs=.) %>%
        is_valid_clean_step(data=data, clean_step=clean_step, msgs=.)

    if (length(msgs) > 0)
        stop('The following pre-conditions failed: ',
             paste(msgs, collapse='; '), '.')

    invisible()

}

# Check argument types in call to smirf
check_arg_types <- function(data, model, n, use_imputed, verbose, fn_init,
                            stop_measure, loop_limit, overrides,
                            clean_step) {

    msgs <- c(msg_if_not(is.data.frame(data)), msg_if_not(is.numeric(n)))

    if (!is.null(model))
        msgs %<>% c(msg_if_not(is.logical(model)), msg_if_not(is.matrix(model)))

    msgs %<>% c(
        msg_if_not(is.logical(use_imputed)), msg_if_not(is.logical(verbose)),
        msg_if_not(is.function(fn_init)), msg_if_not(is.function(stop_measure)),
        msg_if_not(is.numeric(loop_limit)), msg_if_not(is.list(overrides)),
        msg_if_not(is.list(clean_step))
    )

    if (length(msgs > 0))
        stop('The following type requirements failed: ',
             paste(type_check_msg, collapse='; '), '.')

    invisible()

}

