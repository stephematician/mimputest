# Check arguments in call to smirf
check_smirf_args <- function(         X,        model,          n,     gibbs,
                               tree.imp,   boot.train,   obs.only,   verbose,
                              X.init.fn, stop.measure, loop.limit, overrides,
                             clean.step) {

    check_smirf_arg_types(         X,        model,          n,     gibbs,
                            tree.imp,   boot.train,   obs.only,   verbose,
                           X.init.fn, stop.measure, loop.limit, overrides,
                          clean.step)

    msgs <- is_valid_data(X)

    if (!is.null(model))
        msgs <- is_valid_model(X, model, msgs)

    if (length(n) != 1)
        msgs <- c(msgs, 'n is not a scalar value.')
 
    if (n < 0)
        msgs <- c(msgs, 'n must be non-negative')

    if (length(gibbs) != 1)
        msgs <- c(msgs, 'gibbs is not a scalar value')

    if (length(tree.imp) != 1)
        msgs <- c(msgs, 'tree.imp is not a scalar value')

    if (length(boot.train) != 1)
        msgs <- c(msgs, 'boot.train is not a scalar value')

    if (length(obs.only) != 1)
        msgs <- c(msgs, 'obs.only is not a scalar value')

    if (length(verbose) != 1)
        msgs <- c(msgs, 'verbose is not a scalar value')

    msgs <- is_valid_X_init_fn(X.init.fn, msgs)

    msgs <- is_valid_stop_measure(stop.measure, msgs)

    if (length(loop.limit) != 1)
        msgs <- c(msgs, 'loop.limit is not a scalar value.')

    if (loop.limit < 0)
        msgs <- c(msgs, 'loop.limit must be non-negative')

    msgs <- is_valid_clean_step(X, clean.step, msgs)

    if (length(msgs) > 0)
        stop(paste(msgs, collapse='\n'))

    NULL

}

