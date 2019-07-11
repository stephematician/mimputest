context('statistics_of_imputed')

test_that('empty', {

    expect_null(statistics_of_imputed(list()))

})

test_that('numeric data', {

    imputed <- data.frame(x=as.numeric(1:5))

    expect_identical(statistics_of_imputed(imputed),
                     rbind(data.frame(variable='x', value=var(imputed$x),
                                      measure='var'),
                           data.frame(variable='x', value=mean(imputed$x),
                                      measure='mean')))
})

