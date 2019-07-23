#' Predict or sample values from fitted ranger object
#'
#' Determines the predicted value using the 'whole-of-forest' prediction, or
#' evaluates the prediction of a selected tree (\code{tree.imp=T}). In the
#' latter case, each row in the data set is predicted using a randomly selected
#' (with replacement) tree in the forest. We call this a 'tree-sampled'
#' prediction and it \emph{somewhat} corresponds to an sample from the
#' distribution of the predicted value or class given the (new) data.
#'
#' Ranger is considered slow at prediction as of 07/2018, particularly for
#' smaller data sets, see issue 133:
#' \url{https://github.com/imbs-hl/ranger/issues/133}
#'
#' @param ranger_fit ranger object;
#'            random forest fitted to data of same format as \code{data_}
#'            argument.
#' @param data_ a data.frame;
#'            data.frame with same variables as the data used in the training
#'            of \code{ranger_fit}.
#' @param response character;
#'            name of the dependent variable i.e. the value of
#'            \code{dependent.variable.name} from the corresponding call to
#'            \code{\link{ranger}} in the fitting step.
#' @param tree.imp logical;
#'            set to \code{T} to impute using a single (randomly selected with
#'            replacement) tree for each row in \code{data_} or \code{F}
#'            (default) to use the aggregated 'whole-of-forest' prediction.
#' @param ... additional arguments passed on to \code{predict.ranger}, not
#'            including the \code{predict.all} or \code{num.trees} argument.
#' @return predicted or sampled values or classes (with original class and
#'         levels)
#'
#' @seealso \code{\link[ranger]{ranger}}
#'
#' @keywords internal
sample_from_ranger <- function(ranger_fit, data_, response, tree.imp=F, ...) {

    predicted <- NA_real_

    if (tree.imp) {
        tree <- sample(seq_len(ranger_fit$num.trees),
                       size=nrow(data_),
                       replace=T)
        # Reduce number of trees needed e.g. when nrow(data) << num.trees
        unique_tree <- unique(tree)
        if (length(unique_tree) < ranger_fit$num.trees)
            tree <- `[<-`(numeric(ranger_fit$num.trees),
                          unique_tree,
                          seq_along(unique_tree))[tree]
        predicted <- predict(ranger_fit,
                             data=data_,
                             num.trees=length(unique_tree),
                             predict.all=T,
                             ...)$predictions[cbind(1:nrow(data_),
                                                    tree)] 

        if (is.ordered(data_[[response]]) | is.factor(data_[[response]]))
            predicted <- ranger_fit$forest$levels[predicted]

    } else
        predicted <- predict(   ranger_fit, data=data_,
                             predict.all=F,        ...)$predictions

    # return with original factor levels 
    if (is.ordered(data_[[response]])) {
        ordered(predicted, levels=levels(data_[[response]]))
    } else if (is.factor(data_[[response]])) {
        factor(predicted, levels=levels(data_[[response]]))
    } else
        predicted

}

