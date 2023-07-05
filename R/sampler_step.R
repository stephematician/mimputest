# SPDX-FileCopyrightText: 2023 Stephen Wade <stephematician@gmail.com>
# SPDX-License-Identifier: MIT

#' Perform one iteration of the procedure to generate an imputed data set.
#'
#' Each step in the sampling loop for Multiple Imputation by Chained Equations,
#' a.k.a. 'mice' (van Buuren and Groothuis-Oudshoorn, 2011) and estimation by
#' 'missForest' (Stekhoven and Buehlmann, 2012) involves fitting (sequentially)
#' a random forest to each variable and then imputing new values for the missing
#' cases. The differences between the two are the type of prediction drawn for
#' each missing case and whether the latest imputed values are used in training
#' or held back until the next iteration.
#'
#' This is an internal function called by [sampler_loop()]. See [sampler_loop()]
#' for details. This function uses the call supplied by `call_lr_train` to
#' train each random forest, and then uses the generic [stats::predict()] to
#' draw predictions. The implementation when `prediction_type='doove'` mirrors
#' that of the 'mice' package by first using `prediction_type='nodes'` in the
#' prediction call; for the other prediction types the value is passed
#' passively to 'literanger'.
#'
#' The out-of-bag error is calculated for each forest by 'literanger' and
#' returned as a data.frame along with the imputed values and, if requested,
#' the trained random forest objects.
#'
#' @inheritParams sampler_loop
#' @returns list (named): the predicted values from each trained random forest
#' along with the out-of-bag error and the random forests (if requested). The
#' named items are:
#' -   `oob_error` data.frame: the out-of-bag error estimates by iteration,
#'     variable, and measure (i.e. `'mse'` for mean-square error, or `'pfc'` for
#'     proportion falsely classified).
#' -   `imputed` list (named): the imputed values of each missing case for each
#'     variable, in the order that they appear in the data (for each variable).
#' -   `forest` list (named): the trained random forests for each variable if
#'     `keep_forests=TRUE`, otherwise `NULL` (default).
#'
#' @seealso [sampler_loop()]
#'
#' @references
#' -   Stekhoven, D. J. & Buehlmann, P. (2012). MissForest--non-parametric
#'     missing value imputation for mixed-type data. _Bioinformatics_, _28_(1),
#'     112-118. \doi{10.1093/bioinformatics/btr597}.
#' -   Van Buuren, S. & Groothuis-Oudshoorn, K. (2011). mice: Multivariate
#'     Imputation by Chained Equations in R. _Journal of Statistical Software_,
#'     _45_(3), 1-67. \doi{10.18637/jss.v045.i03}.
#'
#' @importFrom rlang call_modify
#' @importFrom stats predict
#' @md
sampler_step <- function(data, model, indicator, call_lr_train, sampler,
                         prediction_type, overrides, clean_step, keep_forests) {

    oob_levels <- c('mse', 'pfc')
    oob_error <- list()
    imputed <- list()
    forests <- if (keep_forests) list() else NULL

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
        if (keep_forests) forests[response_k] <- rf_fit
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
         imputed=imputed,
         forests=forests)

}

