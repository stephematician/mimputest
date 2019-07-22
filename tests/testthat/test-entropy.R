context('entropy')

test_that('empty table', {
    expect_identical(entropy(NA), 0 / 0)
})

test_that('single value', {

    expect_identical(entropy(letters[1]), 0)

    expect_identical(entropy(factor(letters[1], levels=letters[1:3])), 0)

})

test_that('uniform distribution', {

    expect_identical(entropy(letters[1:2]), -sum(rep(log2(1 / 2), 2)) / 2)

    expect_identical(entropy(rep(letters[1:3], each=3)),
                     -sum(3 * rep(log2(1 / 3), 3)) / 9)

})

