context('leiks_D')

test_that('empty table', {
    expect_identical(leiks_D(NA), 0 / 0)
})

test_that('preserves missing levels', {

    expect_identical(leiks_D(factor(letters[1], levels=letters[1:3])), 0)

})

test_that('single value', {

    expect_identical(leiks_D(ordered(letters[1])), 0)

    expect_identical(leiks_D(ordered(letters[1], levels=letters[1:3])), 0)

})

test_that('uniform distribution', {

    expect_identical(leiks_D(factor(letters[1:2])), 2 * sum(c(1) / 2) / 1)

    expect_identical(leiks_D(factor(rep(letters[1:3], each=3))),
                     2 * sum(c(3, 9-6) / 9) / 2)

})

