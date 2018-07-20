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

