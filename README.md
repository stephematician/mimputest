smirf: Single or multiple imputation of missing data using random forests
=========================================================================

_Stephen Wade_

🚧 Under construction 🚧

_Yet another_ implementation of multiple imputation using random forests. Like
others, it is based on the Multiple Imputation via Chained equations ([Van
Buuren and Groothuis-Oudshoorn, 2011][vanbuuren2011_doi]), however it supports a
number of variants:

1.  [Doove et al (2014)][doove2014_doi] draws by 'leaf matching', i.e. drawing
    from observed values that are in the same leaf of any tree, this is the
    default in the [`mice`][mice_cran] package.
2.  [Bartlett (2014)][bartlett2014_archive] proposed a modification to [Doove et
    al (2014)][doove2014_doi] by drawing from bootstrapped values from a random
    leaf node instead, which may improve bias and coverage of pooled confidence
    intervals.
3.  Estimating missing values (not imputing!) via the missForest algorithm
    by [Stekhoven and B&#252;hlmann (2012)][stekhoven2012_doi], also implemented
    in the [`missForest`][missforest_cran] package.

Uniquely, this package uses [`literanger`][literanger_github] to train and draw
from random forests. `literanger` is a re-implementation of
[`ranger`][ranger_cran] (copyright M N Wright, 2014) which is a memory and
computationally-efficient implementation of the random forest algorithm
([Wright and Ziegler, 2017][wright2017_doi]). `literanger` enables more
efficient drawing for the imputation algorithms implemented here.

[bartlett2014_archive]: https://web.archive.org/web/20190819140612/http://thestatsgeek.com/wp-content/uploads/2014/09/RandomForestImpBiometricsConf.pdf

[mice_cran]: https://cran.r-project.org/package=mice
[missforest_cran]: https://cran.r-project.org/package=missForest
[ranger_cran]: https://cran.r-project.org/package=ranger

[doove2014_doi]: https://doi.org/10.1016/j.csda.2013.10.025
[stekhoven2012_doi]: https://doi.org/10.1093/bioinformatics/btr597
[vanbuuren2011_doi]: https://doi.org/10.18637/jss.v045.i03
[wright2017_doi]: https://doi.org/10.18637/jss.v077.i01

[literanger_github]: https://github.com/stephematician/literanger


# Example

On the iris dataset with 20% missing, default 'smirf' runs 2.3x as fast as
'mice' with method 'rf', with the additional advantage given to 'smirf' that the
confidence interval coverage performs closer to that proposed by
Bartlett (2014). Comparing like-to-like, e.g. both sampling as per Doove et
al (2014), 'smirf' runs 1.6x faster.

```r
require(microbenchmark)
# Add some missing values completely at random to iris
set.seed(1)
prop_missing <- 0.2
data_ <- iris
n_prod_m <- prod(dim(data_))
data_[arrayInd(sample.int(n_prod_m, size=n_prod_m * prop_missing),
               .dim=dim(data_))] <- NA

microbenchmark(res <- smirf::smirf(data_, n=5L, loop_limit=10L, n_tree=10L))
#       min       lq     mean   median       uq      max neval
#  1.087175 1.153564 1.218621 1.185026 1.247474 1.767124   100
microbenchmark(res <- smirf::smirf(data_, n=5L, loop_limit=10L, n_tree=10L,
                                   prediction_type='doove'))
#       min       lq     mean  median       uq      max neval
#  1.674584 1.716331 1.745929 1.72985 1.773256 1.882677   100
microbenchmark(res <- mice::mice(data_, m=5L, maxit=10L, method='rf',
                                 printFlag=FALSE))
#       min       lq     mean  median       uq      max neval
#  2.693691 2.726094 2.754054 2.74072 2.770071 3.036368   100
```

The benefit on larger datasets is of similar magnitude (approximately 2.1x
faster), e.g.;

```r
# Make a bigger dummy dataset with some relationship between columns
n_variable <- 1E2
n_obs <- 1E4
data_ <- matrix(0, nrow=n_obs, ncol=n_variable)
beta_jm1_j <- 0.2 * rnorm(n_variable) # relationship from one column to next
x_j <- rnorm(n_variable)
for (j in seq_len(n_variable)) {
    x_j <- beta_jm1_j * x_j + rnorm(n=n_obs, mean=0, sd=1)
    data_[,j] <- x_j
}
colnames(data_) <- paste0('...', 1:n_variable)

n_prod_m <- prod(dim(data_))
data_[arrayInd(sample.int(n_prod_m, size=n_prod_m * prop_missing),
               .dim=dim(data_))] <- NA
data_ <- as.data.frame(data_)

system.time(res <- smirf::smirf(data_, n=5L, loop_limit=10L, n_tree=10L))
#  28m 7.64s
system.time(res <- mice::mice(data_, m=5L, maxit=10L, method='rf', printFlag=FALSE))
#  59m 58.5s
```


# Installation

Installation is easy using [`devtools`][devtools_cran]:

```r
library(devtools)
install_github('stephematician/smirf')
```

[devtools_cran]: https://cran.r-project.org/package=devtools


# Details

## Default multiple imputation algorithm

The default algorithm of this package is similar to that of [Bartlett
(2014)][bartlett2014_archive]. The steps are:

-   Specify a random forest model for each variable in the dataset; i.e. the
    predictors (which can include 'response' indicators), number of cases per
    leaf, the splitting rule, and so on.
-   For the first dataset in the sequence, impute missing values with random
    draws from the observed values.
-   Generate the sequence of datasets by (repeating):
    -   For each variable (in a pre-specified order):
        -   Train a random forest using the _observed_ values and the _most
            recently imputed_ value of any missing predictor.
        -   Impute each missing case by:
            -   Traversing a randomly selected tree (using most recently
                imputed predictors) to its leaf.
            -   Draw once from the training data (the 'in bag' cases) that
                belonged to the leaf.
        -   Record the imputed values.


## MICE random forest imputation algorithm

The difference between the above approach and the one implemented in `mice`
([Doove et al, 2014][doove2014_doi]) is the step where a new value is drawn for
each missing case. In `mice` this step is:

-   Find a set of leaves by traversing all trees.
-   For each leaf in the set:
    -    Find the observed values that also belong to the leaf.
-   Pool the observed values (belonging to the leaves) together.
-   Draw once from the pooled values.


## missForest estimation algorithm

If the aim is to estimate the expected missing value, rather than 'impute'
missing values, then missForest ([Stekhoven and B&#252;hlmann,
2012][stekhoven2012_doi]) and missForest-likes (see additional variations
below) might be useful. The steps are:

-   Specify a random forest model for each variable as in MICE.
-   For the first dataset in the sequence, impute missing values via either
    the most frequent value (categorical data) or the mean (continuous data).
-   Generate a sequence of datasets until stopping criteria are satisfied:
    -   For each variable (in a pre-specified order):
        -   Train a random forest using the _observed_ values and the values of
            predictors _from the previous dataset in the sequence_.
        -   Predict each missing case using the bootstrap aggregated prediction
            of the random forest.
    -   Record the dataset.
    -   Update the stopping criteria; which is test of the change in the
        normalised root mean square error or the proportion of stationary values
        from one dataset to the next.

While missForest differs in its initial state, its use of a stopping criterion,
and that the imputed values are not used in training until the next dataset is
to be generated; these are somewhat dwarfed by its use of the bootstrap
aggregated prediction - this means that the values drawn by multiple invocations
of missForest cannot be pooled in the same manner.


## Further variations

### Stopping criterion (missForest)

The original stopping criterion ([Stekhoven and B&#252;hlmann,
2012][stekhoven2012_doi]) is not location and scale invariant. A
correlation-based criterion is offered as an alternative. The correlation-based
criterion is calculated by:

-   For each ordered/continuous variable, calculate the rank correlation between
    the current and previous dataset.
-   For each non-ordered (factor) variable, calculate the Cramer V corrected
    for bias (see <https://en.wikipedia.org/wiki/Cram%C3%A9r%27s_V>).

The stopping criterion is satisfied when the mean of all these values decreases.
We speculate that this identifies when unexplained variation of the random
forest model is dominant, and that 'entropy' is close optimal. _Author's
lament_: there is no clear picture that these two measures provide a sensible
weighting for mixed data types (the range of possible values isn't even the
same between the two), hopefully other savvier mathematicians can find a
solution.


# Alternatives

For those looking for an alternative to this package:

1.  [`mice`][mice_cran] - uses the approach of [Doove et al
    (2014)][doove2014_doi] and the `ranger` package for training.
2.  [`CALIBERrfimpute`][caliberrfimpute_cran] - uses the approach of [Shah et al
    (2014)][shah2014_doi] via the [`randomForest`][randomforest_cran] package
    for training (slower than `ranger`).
2.  [`miceranger`][miceranger_cran] - supports predictive mean matching to
    impute values and uses the `ranger` package for training.
3.  [`missRanger`][missranger_cran] - same algorithm and training as
    `miceranger` preceding.
4.  [`missForest`][missforest_cran] - the original missForest package for
    missing value estimation, uses the `randomForest` package.

[caliberrfimpute_cran]: https://cran.r-project.org/package=CALIBERrfimpute
[miceranger_cran]: https://cran.r-project.org/package=miceRanger
[missranger_cran]: https://cran.r-project.org/package=missRanger
[randomforest_cran]: https://cran.r-project.org/package=randomForest

[shah2014_doi]: https://doi.org/10.1093/aje/kwt312


# To-do

Not exhaustive:

-   prepare CRAN submission;
-   write package doc;
-   write tests for `smirf()`;
-   a return value that can be used by MICE?;
-   provide an argument to impute from most missing column to least, similar to
    `missForest`;
-   evaluation of error given a 'true' data set, similar to
    `missForest`;


# References

-   Bartlett, J. (2014, 6-11 July). _Methodology for multiple imputation for
    missing data in electronic health record data_ [Conference presentation].
    International Biometric Conference, Florence, TOS, Italy.
    [Archived 2019-08-19][bartlett2014_archive].
-   Doove, L.L., Van Buuren, S., & Dusseldorp, E. (2014). Recursive
    partitioning for missing data imputation in the presence of interaction
    effects. _Computational Statistics & Data Analysis_, 72, 92-104.
    [doi:10.1016/j.csda.2013.10.025][doove2014_doi].
-   Shah, A. D., Bartlett, J. W., Carpenter, J., Nicholas, O., & Hemingway, H.
    (2014). Comparison of random forest and parametric imputation models for
    imputing missing data using MICE: a CALIBER study. _American Journal of
    Epidemiology_, _179_(6), 764-774. [doi:10.1093/aje/kwt312][shah2014_doi].
-   Stekhoven, D. J. & Buehlmann, P. (2012). MissForest--non-parametric
    missing value imputation for mixed-type data. _Bioinformatics_, _28_(1),
    112-118. [doi:10.1093/bioinformatics/btr597][stekhoven2012_doi].
-   Van Buuren, S. & Groothuis-Oudshoorn, K. (2011). mice: Multivariate
    Imputation by Chained Equations in R. _Journal of Statistical Software_,
    _45_(3), 1-67. [doi:10.18637/jss.v045.i03][vanbuuren2011_doi].
-   Wright, M. N., & Ziegler, A. (2017). ranger: A fast implementation of
    random forests for high dimensional data in C++ and R. _Journal of
    Statistical Software_, _77_(i01), 1-17.
    [doi:10.18637/jss.v077.i01][wright2017_doi].

