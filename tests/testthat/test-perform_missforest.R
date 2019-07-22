context('perform_missforest')

test_that('dry run iris', {

    set.seed(1)
    # simulate roughly 1/3 missing
    indicator <- lapply(setNames(nm=names(iris)),
                        function(...)
                            sample(x=c(T,F,F), size=nrow(iris), replace=T))
    r <- perform_missforest(X_init=iris,
                            model=matrix(T, nrow=ncol(iris), ncol=ncol(iris),
                                         dimnames=list(names(iris),
                                                       names(iris))),
                            indicator=indicator,
                            ranger_call=rlang::call2(ranger::ranger,
                                                     write.forest=T),
                            loop.limit=0)

    # Check presence and type of returned values
    expect_type(r, 'list')
    expect_named(r,
                 c('converged',       'imputed', 'iterations',
                   'oob_error', 'stop_measures'),
                 ignore.order=T)
    expect_false(r$converged)
    expect_identical(r$oob_error,
                     data.frame(lapply(iris, function(x) numeric(0))))
    expect_identical(r$iterations, 0L)
    expect_identical(r$stop_measures, list())

    # Check that initial data was recorded correctly
    expect_named(r$imputed[[1]], names(iris), ignore.order=T)
    for (column_name in names(iris))
        with(r,
             expect_identical(imputed[[1]][[!!column_name]],
                              iris[indicator[[!!column_name]], !!column_name]))

})

test_that('all but one in a row iris', {

    set.seed(1)
    missing_column <- sample(names(iris), size=nrow(iris), replace=T)
    indicator <- lapply(setNames(nm=names(iris)),
                        function(nm)
                            nm == missing_column)
    r <- perform_missforest(X_init=iris,
                            model=matrix(T, nrow=ncol(iris), ncol=ncol(iris),
                                         dimnames=list(names(iris),
                                                       names(iris))),
                            indicator=indicator,
                            ranger_call=rlang::call2(ranger::ranger,
                                                     write.forest=T),
                            loop.limit=10L)

    # Check presence and type of returned values
    expect_true(r$converged)

    expect_is(r$iterations, 'integer')
    expect_lte(r$iterations, 10L)

    expect_is(r$oob_error, 'data.frame')
    expect_named(r$oob_error, c('iteration', 'variable', 'measure', 'value'))
    expect_true(all(r$oob_error$iteration %in% seq_len(r$iterations)))
    expect_true(all(r$oob_error$variable %in% names(iris)))
    expect_is(r$oob_error$value, 'numeric')
    expect_true(all(r$oob_error$value > 0))

    expect_is(r$stop_measures, 'list')
    expect_length(r$stop_measures, r$iterations)

    # Check that initial data was recorded correctly
    expect_named(r$imputed[[1]], names(iris), ignore.order=T)
    for (column_name in names(iris))
        with(r,
             expect_identical(imputed[[1]][[!!column_name]],
                              iris[indicator[[!!column_name]], !!column_name]))

    # Check classes and length of all other imputed data
    for (column_name in names(iris)) {
        for (j in seq_along(r$imputed))
            with(r,
                 expect_identical(attributes(imputed[[!!j]][[!!column_name]]),
                                  attributes(iris[indicator[[!!column_name]],
                                                  !!column_name])))
            with(r,
                 expect_identical(length(imputed[[!!j]][[!!column_name]]),
                                  sum(indicator[[!!column_name]])))
    }

})

