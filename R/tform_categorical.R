
# transform numeric/logical to categorical type
tform_categorical <- function(x_j)
    switch(storage.mode(x_j),
           'integer'=as.ordered(x_j),
           'logical'=as.factor(x_j),
           'character'=as.factor(x_j))

# get a function that transforms from categorical type to numeric/logical
#' @importFrom stats setNames
make_inv_tform_categorical <- function(x_j) {

    if (is.numeric(x_j) && !is.integer(x_j))
        stop('Cannot construct map from continuous data to categorical ',
             '(use cut first?)')

    cat_j <- tform_categorical(x_j)
    map_j <- setNames(x_j[sapply(levels(cat_j), match, table=cat_j)],
                      levels(cat_j))
    function(y) setNames(map_j[y], names(y))

}

# apply each transform to numeric/logical to a (imputed) data set
#' @importFrom magrittr %<>%
apply_inv_tform_categorical <- function(data, inv_tform_categorical) {
    to_categorical <- names(inv_tform_categorical)
    data[to_categorical] %<>% mapply(
        function(f, x) f(x), f=inv_tform_categorical, x=., SIMPLIFY=FALSE
    )
    data
}

