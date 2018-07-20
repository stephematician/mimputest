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

