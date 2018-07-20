# Check arguments in call to miForang
check_miForang_args <- function(           X,          n, order.impute,
                                       gibbs,   tree.imp,   boot.train,
                                    obs.only,    verbose,    X.init.fn,
                                stop.measure, loop.limit,    overrides) {

    check_miForang_arg_types(           X,          n, order.impute,
                                    gibbs,   tree.imp,   boot.train,
                                 obs.only,    verbose,    X.init.fn,
                             stop.measure, loop.limit,    overrides)

    msgs <- is_valid_data(X)

    if (length(n) != 1)
        msgs <- c(msgs, 'n is not a scalar value.')

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

    if (!is.null(order.impute))
        msgs <- is_valid_order_impute(X, order.impute, msgs)

    msgs <- is_valid_X_init_fn(X.init.fn, msgs)

    msgs <- is_valid_stop_measure(stop.measure, msgs)

    if (length(loop.limit) != 1)
        msgs <- c(msgs, 'loop.limit is not a scalar value.')

    if (length(msgs) > 0)
        stop(paste(msgs, collapse='\n'))

    NULL

}

