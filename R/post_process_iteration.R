# Post process a single iteration
post_process_iteration <- function(X_predict,
                                   to_categories=list(),
                                   iteration=data.frame(iteration=0)) {

    categorical_data <- names(X_predict)[!sapply(X_predict, is.numeric) |
                                             !!sapply(X_predict, is.integer)]

    list(frequency=data.frame(iteration,
                              frequency_of_imputed(X_predict[categorical_data],
                                                   to_categories)),
         statistics=data.frame(iteration,
                               statistics_of_imputed(X_predict)))

}

