# SPDX-FileCopyrightText: 2023 Stephen Wade <stephematician@gmail.com>
# SPDX-License-Identifier: MIT

test_that('get NULL when input is empty', {

    expect_null(statistics_of_imputed(list(), numeric()))

})

test_that('mean and variance reported for continuous data', {

    imputed <- list(x=as.numeric(1:5))

    expect_identical(
        statistics_of_imputed(imputed, NA_integer_),
        data.frame(variable='x', value=c(var(imputed$x), mean(imputed$x)),
                   measure=c('var', 'mean'), iteration=NA_integer_)
    )
})

test_that('entropy reported for unordered (categorical) data', {
    imputed_bool <- list(x=c(FALSE, TRUE))
    imputed_char <- list(x=LETTERS[1:2])
    imputed_fctr <- list(x=factor(LETTERS[1:2]))
    expect_identical(
        statistics_of_imputed(imputed_bool, NA_integer_),
        data.frame(variable='x', value=entropy(imputed_bool$x),
                   measure=c('entropy'), iteration=NA_integer_)
    )
    expect_identical(
        statistics_of_imputed(imputed_char, NA_integer_),
        data.frame(variable='x', value=entropy(imputed_char$x),
                   measure=c('entropy'), iteration=NA_integer_)
    )
    expect_identical(
        statistics_of_imputed(imputed_fctr, NA_integer_),
        data.frame(variable='x', value=entropy(imputed_fctr$x),
                   measure=c('entropy'), iteration=NA_integer_)
    )
})

test_that('entropy and Leik\'s D reported for ordered (categorical) data', {
    imputed_ordr <- list(x=ordered(LETTERS[1:2]))
    expect_identical(
        statistics_of_imputed(imputed_ordr, NA_integer_),
        data.frame(variable='x',
                   value=c(entropy(imputed_ordr$x), leiks_D(imputed_ordr$x)),
                   measure=c('entropy', 'leiks_D'), iteration=NA_integer_)
    )
})

