context('miForang')

test_that('no imputations', {

    # A minimal run to check types of outputs etc.
    #set.seed(1)

    #res <- miForang(iris, n=0, num.trees=10, loop.limit=1)

})

test_that('no iterations', {

    # A minimal run to check types of outputs etc.
    #set.seed(1)

    # should return a dry run
    #res <- miForang(iris, n=0, num.trees=10, loop.limit=1)

})

test_that('complete data', {

    # A minimal run to check types of outputs etc.
    #set.seed(1)

    #res <- miForang(iris, n=1, num.trees=10, loop.limit=1)

})

test_that('iris data', {

    # A minimal run to check types of outputs etc.
    #set.seed(1)

    #prop_missing <- 0.2
    #data_ <- iris
    #n_prod_m <- prod(dim(data_))
    #data_[arrayInd(sample.int(n_prod_m, size=n_prod_m * prop_missing),
    #               .dim=dim(data_))] <- NA

    #res <- miForang(data_, n=1, num.trees=10, loop.limit=1)

})
