# mimputest 0.0.0.9000

To be released as 0.0.1

This is the initial release of 'mimputest', a package for multiple imputation of
missing data using random forests via the method proposed by Jonathan Bartlett
in 2014  <https://thatstatsgeek.com> or Doove et al, 2014
<https://doi.org/10.1016/j.csda.2013.10.025>.

Forests are trained via the 'literanger' package which is an updated
mplementation of 'ranger' (copyright M N Wright, 2014
<https://doi.org/10.18637/jss.v077.i01>) that reduces the computational effort
in prediction.

Estimation of missing data via the 'missForest' algorithm of Stekhoven and
Buehlmann, 2010 <https://doi.org/10.1093/bioinformatics/btr597>, is also
available.

Main features:

-   Imputation or estimation via `mimputest()` function.
-   Can convert output via `as_mids()` to analyse imputed data via
    `mice::pool`.
-   Improved pooled confidence interval bias and coverage compared to the
    random forest case in `mice`.
-   Roughly 2-2.5x speed up compared to `mice` (w/- random forests) due to
    fast implementation (literanger) for Bartlett prediction step.
-   Supports 'bad'/'wrong'/'quick-n-dirty' analysis of multiply imputed data
    via completion of a data set that can be (weighted-) analysed once.
-   Records additional 'chain' statistics (entropy and Leik's D) not provided in
    'mice'.
-   Additional stopping criterion available for missForest.

