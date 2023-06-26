# Copyright (c) Cancer Council NSW, 2018-2023. All rights reserved.

#' Perform one iteration of the procedure to generate an imputed data set.
#'
#' Each step in the iterative procedure to generate an imputed data set results
#' in a new data set by training and predicting (using random forests) the
#' missing values for each variable.
#'
#' A description of the overall process is given in [sampler_loop()]. This is a
#' function internal to 'smirf' that calls the training and prediction methods
#' from 'literanger'. The arguments are described in greater detail in
#' [sampler_loop()]. At the end of the step the out-of-bag error and the
#' imputed values (per variable) are returned.
#'
#' @inheritParams sampler_loop
#' @returns named list: \describe{
#'   \item{`oob_error`}{data.frame: out-of-bag error estimates for each
#'     modelled variable with columns `iteration` (the identified of the
#'     iteration), `variable` (name), `measure` (the type of OOB-error measure;
#'     'mse' is mean square error, 'pfc' is proportion falsely classified), and
#'     `value` is the estimate of the out-of-bag error.}
#'   \item{`imputed`}{named list: the imputed values of missing cases for each
#'     variable in `data`.}
#' }
#'
#' @seealso [sampler_loop()]
#'
#' @importFrom rlang call_modify
#' @md
sampler_step <- function(data, model, indicator, call_lr_train, sampler,
                         prediction_type, overrides, clean_step) {

    oob_levels <- c('mse', 'pfc')
    oob_error <- list()
    imputed <- list()

    for (response_k in rownames(model)) {

      # missing status for response
        indicator_k <- indicator[[response_k]]
      # indicator of included predictors
        predictors_k <- as.logical(model[response_k, , drop=TRUE])
      # names of variables in (random forest) model
        vars_k <- intersect(
            names(data),
            union(response_k, colnames(model)[predictors_k])
        )
      # fit and predict from model
        rf_fit <- eval(rlang::call_modify(
            call_lr_train,
            data=data[!indicator_k, vars_k, drop=FALSE],
            response_name=response_k,
            !!! overrides[[response_k]])
        )
        if (prediction_type == 'doove') {
            imputed_k <- predict_doove(
                rf_fit, data[, vars_k, drop=FALSE], indicator_k, response_k
            )
        } else {
            imputed_k <- predict(rf_fit,
                                 newdata=data[indicator_k, vars_k, drop=FALSE],
                                 prediction_type=prediction_type)$values
        }
        if (is.ordered(data[[response_k]]))
            imputed_k <- factor(imputed_k,
                                levels=levels(data[[response_k]]),
                                ordered=TRUE)
      # record oob error
        oob_measure_k <- switch(
            rf_fit$tree_type,
            'classification'='pfc',
            'regression'='mse'
        ) %>% factor(levels=oob_levels)
        oob_error[[length(oob_error) + 1]] <- data.frame(
            variable=response_k, measure=oob_measure_k, value=rf_fit$oob_error
        )
      # if doing gibbs sampler - clean imputed and update the training data
        if (sampler == 'gibbs') {
            clean_k <- clean_step[[response_k]]
            if (!is.null(clean_k))
                imputed_k %<>% clean_k(data=data[indicator_k, , drop=FALSE],
                                       imputed=.)
            data[indicator_k,response_k] <- imputed_k
        }

        imputed[[response_k]] <- imputed_k

    }

    if (sampler == 'missforest') {
        for (response_k in rownames(model)) {
            imputed_k <- imputed[[response_k]]
            indicator_k <- indicator[[response_k]]
            clean_k <- clean_step[[response_k]]
            if (!is.null(clean_k))
                imputed_k %<>% clean_k(data=data[indicator_k, , drop=FALSE],
                                       imputed=.)
            imputed[[response_k]] <- imputed_k
        }
    }

    list(oob_error=do.call(rbind, oob_error),
         imputed=imputed)

}

