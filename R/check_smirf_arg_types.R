# Check argument types in call to smirf
check_smirf_arg_types <- function(           X,          n, order.impute,
                                         gibbs,   tree.imp,   boot.train,
                                      obs.only,    verbose,    X.init.fn,
                                  stop.measure, loop.limit,    overrides,
                                    clean.step) {

    msg_if_not <- function(condition, msgs=list())
        if (!condition)
            c(msgs,
              paste0('unsatisfied condition: ',
                     quo_name(enquo(condition)),
                     '.'))

    type_check_msg <- c(msg_if_not(is.data.frame(X)), msg_if_not(is.numeric(n)))

    if (!is.null(order.impute))
        type_check_msg <- c(type_check_msg,
                            msg_if_not(is.character(order.impute)))

    type_check_msg <- c(type_check_msg,
                        msg_if_not(is.logical(gibbs)),
                        msg_if_not(is.logical(tree.imp)),
                        msg_if_not(is.logical(boot.train)),
                        msg_if_not(is.logical(obs.only)),
                        msg_if_not(is.logical(verbose)),
                        msg_if_not(is.function(X.init.fn)),
                        msg_if_not(is.function(stop.measure)),
                        msg_if_not(is.numeric(loop.limit)),
                        msg_if_not(is.list(overrides)),
                        msg_if_not(is.list(clean.step)))

    if (length(type_check_msg > 0))
        stop(paste(type_check_msg, collapse='\n'))

    NULL

}

