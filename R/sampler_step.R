
sampler_step <- function(data, model, indicator, call_lr_train, sampler,
                         prediction_type, overrides, clean_step, iter_id=1) {

    oob_levels <- c('mse', 'pfc')
    oob_error <- list()
    imputed <- list()

    for (response_k in rownames(model)) {

      # missing status for response
        indicator_k <- indicator[[response_k]]
      # indicator of included predictors
        predictors_k <- model[response_k, , drop=TRUE]
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
        imputed_k <- predict(rf_fit,
                             newdata=data[indicator_k, vars_k, drop=FALSE],
                             prediction_type=prediction_type)$values
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
        oob_error[[iter_id]] <- data.frame(
            iteration=iter_id, variable=response_k,
            measure=oob_measure_k, value=rf_fit$oob_error
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

