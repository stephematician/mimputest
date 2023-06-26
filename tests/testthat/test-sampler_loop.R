
# TODO: update tests?

test_that('dry run iris', {

    set.seed(1)
  # simulate roughly 1/3 missing
    indicator <- lapply(
        setNames(nm=names(iris)),
        function(...)
        sample(x=c(TRUE, rep(FALSE, 2)), size=nrow(iris), replace=TRUE)
    )

    result <- sampler_loop(
        data=iris,
        model=matrix(TRUE, nrow=ncol(iris), ncol=ncol(iris),
                     dimnames=list(names(iris), names(iris))),
        indicator=indicator,
        sampler='gibbs', prediction_type='inbag',
        call_lr_train=rlang::call2(literanger::train),
        loop_limit=0
    )

  # Check presence and type of returned values
    expect_type(result, 'list')
    expect_named(result,
                 c('converged',       'imputed', 'iterations',
                   'oob_error', 'stop_measures'),
                 ignore.order=TRUE)
    expect_false(result$converged)
    expect_identical(result$oob_error,
                     data.frame(iteration=integer(), variable=character(),
                                measure=factor(levels=c('mse', 'pfc')),
                                value=numeric()))
    expect_identical(result$iterations, 0L)
    expect_identical(result$stop_measures, list())

  # Check that initial data was recorded correctly
    expect_named(result$imputed[[1]], names(iris), ignore.order=TRUE)
    for (column_name in names(iris))
        with(result,
             expect_identical(imputed[[1]][[!!column_name]],
                              iris[indicator[[!!column_name]], !!column_name]))

})

test_that('each row of iris has one missing value', {

    set.seed(1)
    missing_column <- sample(names(iris), size=nrow(iris), replace=TRUE)
    indicator <- lapply(
        setNames(nm=names(iris)),
        function(nm) nm == missing_column
    )
    result <- sampler_loop(
        data=iris,
        model=matrix(TRUE, nrow=ncol(iris), ncol=ncol(iris),
                     dimnames=list(names(iris), names(iris))),
        indicator=indicator,
        sampler='gibbs', prediction_type='inbag',
        call_lr_train=rlang::call2(literanger::train),
        loop_limit=10L
    )

  # Check presence and type of returned values
    expect_true(result$converged)

    expect_type(result$iterations, 'integer')
    expect_lte(result$iterations, 10L)

    expect_s3_class(result$oob_error, 'data.frame')
    expect_named(result$oob_error,
                 c('iteration', 'variable', 'measure', 'value'))
    expect_true(all(result$oob_error$iteration %in% seq_len(result$iterations)))
    expect_true(all(result$oob_error$variable %in% names(iris)))
    expect_type(result$oob_error$value, 'double')
    expect_true(all(result$oob_error$value > 0))

    expect_type(result$stop_measures, 'list')
    expect_length(result$stop_measures, result$iterations)

  # Check that initial data was recorded correctly
    expect_named(result$imputed[[1]], names(iris), ignore.order=T)
    for (column_name in names(iris))
        with(result,
             expect_identical(imputed[[1]][[!!column_name]],
                              iris[indicator[[!!column_name]], !!column_name]))

  # Check classes and length of all other imputed data
    for (column_name in names(iris)) {
        for (j in seq_along(result$imputed))
            with(result,
                 expect_identical(attributes(imputed[[!!j]][[!!column_name]]),
                                  attributes(iris[indicator[[!!column_name]],
                                                  !!column_name])))
            with(result,
                 expect_identical(length(imputed[[!!j]][[!!column_name]]),
                                  sum(indicator[[!!column_name]])))
    }

})

