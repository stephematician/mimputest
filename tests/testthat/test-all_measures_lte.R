
test_that('get true if both null', {
    expect_true(all_measures_lte(NULL, NULL))
})

test_that('get false if before null and now zero', {
    expect_false(all_measures_lte(NULL, 0))
})

test_that('get correct order for single numeric measure', {
    expect_false(all_measures_lte(0, 1))
    expect_true(all_measures_lte(0, 0))
    expect_true(all_measures_lte(1, 0))
})

test_that('get correct result for measures as numeric vector', {
    expect_false(all_measures_lte(c(0, 0), c(1, 1)))
    expect_true(all_measures_lte(c(1, 1), c(1, 1)))
    expect_true(all_measures_lte(c(1, 1), c(0, 0)))
    expect_true(all_measures_lte(c(1, 0), c(0, 0)))
})

test_that('get correct result for named numeric vector', {
    expect_error(all_measures_lte(c(x=1), c(y=1)))
    expect_true(all_measures_lte(c(x=1, y=0), c(x=0, y=0)))
    expect_true(all_measures_lte(c(x=1, y=0), c(y=0, x=0)))
})

test_that('get warning if non-ordered measure', {
    expect_warning(all_measures_lte(NULL, factor('A')))
})

test_that('get correct result for ordered scalar measure', {
    expect_false(
        all_measures_lte(ordered('A', levels=LETTERS),
                         ordered('B', levels=LETTERS))
    )
    expect_true(
        all_measures_lte(ordered(c('B'), levels=LETTERS),
                         ordered(c('B'), levels=LETTERS))
    )
    expect_true(
        all_measures_lte(ordered(c('B'), levels=LETTERS),
                         ordered(c('A'), levels=LETTERS))
    )

})

test_that('get correct result for ordered vector measure', {
    expect_false(
        all_measures_lte(ordered(c('A', 'A'), levels=LETTERS),
                         ordered(c('B', 'B'), levels=LETTERS))
    )
    expect_true(
        all_measures_lte(ordered(c('B', 'B'), levels=LETTERS),
                         ordered(c('B', 'B'), levels=LETTERS))
    )
    expect_true(
        all_measures_lte(ordered(c('B', 'B'), levels=LETTERS),
                         ordered(c('A', 'A'), levels=LETTERS))
    )
    expect_true(
        all_measures_lte(ordered(c('B', 'A'), levels=LETTERS),
                         ordered(c('A', 'A'), levels=LETTERS))
    )

})

