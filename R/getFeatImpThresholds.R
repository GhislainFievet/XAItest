#' The getFeatImpThresholds function identifies the minimum level of feature importance required to exceed
#' a specified significance threshold, which is determined by the p-value.
#'
#' @details 
#' The reference p-value column can be given by the refPvalColumn argument. If not provided,
#' the function will search for the first df column name containing "pval".
#' The feature importance columns can be given by the featImpColumns argument. If not provided,
#' the function will search for all df column names containing "feat".
#' 
#' It then selects feature importance values of features with p-values under the specified 
#' threshold and returns the lowest.
#'
#' This is useful for identifying the most significant features in a dataset based on statistical
#' testing, aiding in the interpretation of machine learning models and exploratory data analysis.
#' 
#' @param df A dataframe containing p-value columns and feature importance columns.
#' @param refPvalColumn Optional; the name of the column containing the reference p-values.
#'   If not provided, the function will search for a column name containing "adjpval", if not
#'      existing a column name containing "pval" (case insensitive).
#' @param featImpColumns Optional; a vector of column names containing the feature importance values.
#'   If not provided, the function will search for column names containing "feat" (case insensitive).
#' @param refPval The reference p-value threshold for filtering features. Defaults to 0.05.
#' 
#'
#' @return A named vector of minimum feature importance values for each feature passing the p-value filter.
#'   The names of the vector elements correspond to the feature importance columns in `df`.
#'
#' @examples 
#' # Assuming `df` is a dataframe with columns `feature1_pval`, `feature2_pval`, `feature1_imp`, `feature2_imp`
#' df <- data.frame(pval = c(0.04, 0.02, 0.06, 0.8),
#'                  adjPval = c(0.01, 0.03, 0.05, 0.9),
#'                  feat_imp_1 = c(0.2, 0.3, 0.1, 0.6),
#'                  feat_imp_2 = c(0.4, 0.5, 0.3, 0.6))
#' thresholds <- getFeatImpThresholds(df)
#' print(thresholds)
#'
#' @export


getFeatImpThresholds <- function(df, refPvalColumn=NULL, featImpColumns=NULL, refPval=0.05){
    if (is.null(refPvalColumn)){
        refPvalColumn <- grep("adjpval", colnames(df), ignore.case=TRUE, value=TRUE)
        if (length(refPvalColumn) > 0){
            refPvalColumn <- refPvalColumn[1]
        } else {
            refPvalColumn <- grep("pval", colnames(df), ignore.case=TRUE, value=TRUE)[1]
        }
    }
    if (is.null(featImpColumns)){
        featImpColumns <- grep("feat", colnames(df), ignore.case=TRUE, value=TRUE)
    }
    results <- sapply(featImpColumns, function(x){
        featImps <- df[[x]][df[[refPvalColumn]] <= refPval]
        featImps <- featImps[!is.na(featImps)]
        return (min(featImps))
    })
    return(results)
}