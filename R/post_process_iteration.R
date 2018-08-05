# Post process a single iteration
post_process_iteration <- function(imputed,
                                   to_categories=list(),
                                   iteration=data.frame(iteration=0)) {

    categorical_data <- names(imputed)[!sapply(imputed, is.numeric) |
                                           !!sapply(imputed, is.integer)]

    list(frequency=data.frame(iteration,
                              frequency_of_imputed(imputed[categorical_data],
                                                   to_categories)),
         statistics=data.frame(iteration,
                               statistics_of_imputed(imputed)))

}

