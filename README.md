# miForang: Missing data (multiple) imputation using a fast implementation of random forests

_Stephen Wade_

miForang (rhymes with mi goreng) is an implementation of Stekhoven and
B&#252;hlmann's (2012) missForest algorithm. It is an iterative procedure for
imputation of missing data of mixed type via fitting random forests to the
observed data. It uses a fast implementation of random forests supplied by
[`ranger`][ranger_link] (Wright and Ziegler, 2017).

## Example

```r
require(miForang)

# Add some missing values completely at random to iris
set.seed(1)

prop_missing <- 0.2
data_ <- iris
n_prod_m <- prod(dim(data_))
data_[arrayInd(sample.int(n_prod_m, size=n_prod_m * prop_missing),
               .dim=dim(data_))] <- NA

# Impute missing data - here using the original missForest stopping criterion
res <- miForang(data_, stop.measure=measure_stekhoven_2012)
```

## Installation

Installation is easy using [`devtools`][devtools_link]:

```r
library(devtools)
install_github('stephematician/miForang')
```

[`ranger`][ranger_link] and [`rlang`][rlang_link] libraries are also required,
both are available via CRAN:

```r
install.packages(c('ranger', 'rlang'))
```

### Alternatives

For those looking for an alternative to this package:

  1. [`missRanger`][miss_ranger_link] - uses the same underlying forest training
     package ([`ranger`][ranger_link]) as here.
  2. [`missForest`][miss_forest_link] - the original missForest package.

## Details

The original [`missForest`][miss_forest_link] trains and predicts the forests
via [`randomForest`][random_forest_link], the key differences between this
implementation and the original are that;

  - each random forest is fit via [`ranger`][ranger_link] (Wright and Ziegler,
    2017), which is optimised for training on high dimensional data;
  - the stopping criterion has been updated (see
    [Stopping criterion](#stopping-criterion)) or a user may provide their
    own function;
  - a user may specify the initial guess for the missing values in the first
    step of the iterative procedure;
    - by default, the original Stekhoven and B&#252;hlmann (2012) missForest
      approach uses the mean or most frequent value of the complete cases of
      each variable;
    - an alternative is to sample (with replacement) from the complete cases of
      each variable (similar to [`mice`][mice_link]), and;
  - a user may specify to run the algorithm as many times as desired (or as
    the machine can handle!) to perform a kind of pseudo-multiple imputation.

In addition the iterative procedure can be (optionally) modified;

  - instead of using the 'whole-of-forest' prediction for each missing value,
    the prediction a randomly sampled tree in the forest ('tree-sampling') may
    be used throughout the process;
  - 'Gibbs' sampling may be used as new predictions of missing values become
    available for each variable;
  - as in Bartlett (2014) a bootstrap of observed data may be used for
    training each _forest_, and;
  - the forests may be trained on all rows of the data, including predictions
    for missing values (this is experimental - inspired by [MICE][mice_link]).

Combining tree-sampled missing values, Gibbs sampling and a random initial
state, is like the implementation of Multiple Imputation via Chained Equations
(MICE) using random forests proposed by Doove et al (2014) but for one
difference - the predictions from each tree are based on the mean of the
terminal node rather than a sample of the training data that belong to the
terminal node.

## Stopping criterion

The original stopping criterion (Stekhoven and B&#252;hlmann, 2012) is not
location and scale invariant. As an experiment, it has been replaced by a
correlation based calculation by default. The user may specify their own 
stopping criteria, and for this purpose Stekhoven and B&#252;hlmann's (2012)
criteria is included as an example.

By default, at each iteration the (rank) correlation between the current data
and the previous data is estimated for ordered/continuous data. For non-ordered
data, the proportion of stationary values in categorical data is calculated.
When both the mean correlation and proportion of stationary values decreases,
the imputation procedure has converged.

_Author's note and lament:_

I am speculating that this criterion identifies when the unexplained variation
of the random forest model of the (complete) data is dominating, and that
'entropy' (possibly incorrect use of this term) has been optimised. It still
seems unpleasant to have incomparable measures for the different types of data.
I hope that other mathematicians, more savvy than I, can investigate this.

## To-do

Not exhaustive:

  - prepare CRAN submission;
  - provide an argument to impute from most missing column to least, similar to
    [`missForest`][miss_forest_link];
  - evaluation of error given a 'true' data set, similar to
    [`missForest`][miss_forest_link];
  - calculation of a 'mean' OOB error (only variable-wise is currently
    available), similar to [`missForest`][miss_forest_link], and;
  - implement predictive mean matching as in [`missRanger`][miss_ranger_link].

## References

Bartlett, J., 2014. 'Methodology for multiple imputation for missing data in
electronic health record data', presented to _27th International Biometric
Conference_, Florence, July 6-11.

Doove, L.L., Van Buuren, S. and Dusseldorp, E., 2014. Recursive partitioning for
missing data imputation in the presence of interaction effects. _Computational
Statistics & Data Analysis, 72_, pp. 92-104.
[doi.10.1016/j.csda.2013.10.025](https://dx.doi.org/10.1016/j.csda.2013.10.025)

Stekhoven, D.J. and B&#252;hlmann, P., 2012. MissForest&#8212;non-parametric
missing value imputation for mixed-type data. _Bioinformatics, 28_(1), pp.
112-118.
[doi.1.1093/bioinformatics/btr597](https://dx.doi.org/10.1093/bioinformatics/btr597)

Wright, M. N. and Ziegler, A., 2017. ranger: A fast implementation of random
forests for high dimensional data in C++ and R. _Journal of Statistical
Software, 77_(i01), pp. 1-17.
[doi.10.18637/jss.v077.i01](https://dx.doi.org/10.18637/jss.v077.i01).

[devtools_link]: https://cran.r-project.org/package=devtools
[mice_link]: https://cran.r-project.org/package=mice
[miss_forest_link]: https://cran.r-project.org/package=missForest
[miss_ranger_link]: https://cran.r-project.org/package=missRanger
[random_forest_link]: https://cran.r-project.org/package=randomForest 
[ranger_link]: https://cran.r-project.org/package=ranger
[rlang_link]: https://cran.r-project.org/package=rlang

