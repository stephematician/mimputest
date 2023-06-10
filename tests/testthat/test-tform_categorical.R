
test_that('get error for numeric', {
    expect_error(
        make_inv_tform_categorical(1),
        paste('Cannot construct map from continuous data to categorical',
              '(use cut first?)'), fixed=T
    )
})

test_that('correct inverse factor-to-numeric conversion', {

    expect_silent(inv_tform_fac <- make_inv_tform_categorical(factor('A')))

    expect_s3_class(inv_tform_fac(numeric()), 'factor')
    expect_identical(levels(inv_tform_fac(numeric())), levels(factor('A')))
    expect_identical(inv_tform_fac(1), factor('A'))
    expect_identical(inv_tform_fac(2), factor(NA, levels='A'))

})

test_that('correct inverse integer-to-numeric conversion', {

    expect_silent(inv_tform_int <- make_inv_tform_categorical(c(1L, 2L)))

    expect_type(inv_tform_int(numeric()), 'integer')
    expect_identical(inv_tform_int(1), 1L)
    expect_identical(inv_tform_int(2), 2L)
    expect_identical(inv_tform_int(3), NA_integer_)

})

test_that('correct inverse logical-to-numeric conversion', {

    expect_silent(inv_tform_bin <- make_inv_tform_categorical(c(TRUE, FALSE)))

    expect_type(inv_tform_bin(numeric()), 'logical')
    expect_identical(inv_tform_bin(1), FALSE)
    expect_identical(inv_tform_bin(2), TRUE)
    expect_identical(inv_tform_bin(3), NA)

})

test_that('correct inverse character-to-numeric conversion', {

    expect_silent(inv_tform_char <- make_inv_tform_categorical(LETTERS[1:2]))

    expect_type(inv_tform_char(numeric()), 'character')
    expect_identical(inv_tform_char(1), LETTERS[1])
    expect_identical(inv_tform_char(2), LETTERS[2])
    expect_identical(inv_tform_char(3), NA_character_)

})

test_that('names are preserved', {

    inv_tform <- make_inv_tform_categorical(LETTERS[1:3])

    expect_identical(names(inv_tform(c(a=1))), 'a')

})

