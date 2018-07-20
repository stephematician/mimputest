context('get_maps_to_categories')

test_that('non-integer and non-logical', {
    expect_identical(
        get_maps_to_categories(data.frame(x=1, y=factor('A'), z=ordered('A'))),
        setNames(list(), nm=character(0))
    )
})

test_that('integers', {

    expect_error(get_maps_to_categories(data.frame(x=integer(0))))

    expect_identical(
        get_maps_to_categories(data.frame(x=1L, y=1)),
        list(x=c('1'=1L))
    )
})

test_that('logical', {

    expect_error(get_maps_to_categories(data.frame(x=logical(0))))

    expect_identical(
        get_maps_to_categories(data.frame(x=T, y=1)),
        list(x=c('TRUE'=T))
    )
})

