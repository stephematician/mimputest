context('sample_from_ranger')

test_that('whole-of-tree classification', {
    set.seed(1)
    rf_fit <- ranger::ranger(data=iris,
                             num.trees=10,
                             write.forest=T,
                             dependent.variable.name='Species',
                             respect.unordered.factors='order')

    wof_values <- sample_from_ranger(rf_fit, data_=iris, 'Species', tree.imp=F)

    expect_identical(nrow(iris), length(wof_values))
    expect_identical(attributes(iris$Species), attributes(wof_values))
    expect_identical(class(iris$Species), class(wof_values))
 
})

test_that('single classification tree', {
    set.seed(1)
    rf_fit <- ranger::ranger(data=iris,
                             num.trees=1,
                             write.forest=T,
                             dependent.variable.name='Species',
                             respect.unordered.factors='order')

    tree_values <- sample_from_ranger(rf_fit, data_=iris, 'Species', tree.imp=T)
    wof_values <- sample_from_ranger(rf_fit, data_=iris, 'Species', tree.imp=F)

    expect_identical(wof_values, tree_values)
 
})

test_that('tree-sampled classification', {
    set.seed(1)
    rf_fit <- ranger::ranger(data=iris,
                             num.trees=10,
                             write.forest=T,
                             dependent.variable.name='Species',
                             respect.unordered.factors='order')

    tree_values <- sample_from_ranger(rf_fit, data_=iris, 'Species', tree.imp=T)

    expect_identical(nrow(iris), length(tree_values))
    expect_identical(attributes(iris$Species), attributes(tree_values))
    expect_identical(class(iris$Species), class(tree_values))
 
})

test_that('whole-of-tree regression', {
    set.seed(1)
    rf_fit <- ranger::ranger(data=iris,
                             num.trees=10,
                             write.forest=T,
                             dependent.variable.name='Petal.Length',
                             respect.unordered.factors='order')

    wof_values <- sample_from_ranger(rf_fit,
                                     data_=iris,
                                     'Petal.Length',
                                     tree.imp=F)

    expect_identical(nrow(iris), length(wof_values))
    expect_identical(attributes(iris$Petal.Length), attributes(wof_values))
    expect_identical(class(iris$Petal.Length), class(wof_values))

})

test_that('single regression tree', {
    set.seed(1)
    rf_fit <- ranger::ranger(data=iris,
                             num.trees=1,
                             write.forest=T,
                             dependent.variable.name='Petal.Length',
                             respect.unordered.factors='order')

    tree_values <- sample_from_ranger(rf_fit,
                                      data_=iris,
                                      'Petal.Length',
                                      tree.imp=T)
    wof_values <- sample_from_ranger(rf_fit,
                                     data_=iris,
                                     'Petal.Length',
                                     tree.imp=F)

    expect_identical(wof_values, tree_values)

})

test_that('tree-sampled regression', {
    set.seed(1)
    rf_fit <- ranger::ranger(data=iris,
                             num.trees=10,
                             write.forest=T,
                             dependent.variable.name='Petal.Length',
                             respect.unordered.factors='order')

    tree_values <- sample_from_ranger(rf_fit,
                                      data_=iris,
                                      'Petal.Length',
                                      tree.imp=T)

    expect_identical(nrow(iris), length(tree_values))
    expect_identical(attributes(iris$Petal.Length), attributes(tree_values))
    expect_identical(class(iris$Petal.Length), class(tree_values))

})

