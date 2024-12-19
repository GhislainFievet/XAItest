#' The mapPvalImportance function displays a datatable with color-coded cells
#' based on significance thresholds for feature importance and p-value columns.
#'
#'
#' @param objXAI An object of class ObjXAI.
#' @param refPvalColumn Optional; the name of the column containing reference
#'      p-values for feature importance.
#'      If not provided, the function will attempt to auto-detect.
#' @param featImpColumns Optional; a vector of column names containing feature
#'      importance values.
#'      If not provided, the function will attempt to auto-detect.
#' @param pvalColumns Optional; a vector of column names containing p-values.
#'     If not provided, the function searches for columns containing "pval"
#'      (case insensitive).
#' @param refPval The reference p-value threshold used for filtering.
#'      Defaults to 0.05.
#'
#' @details
#' The function first identifies the relevant p-value columns and feature
#' importance columns, if not explicitly provided. It then calculates feature
#' importance thresholds based on the specified p-value threshold.
#' Then the dataframe is displaid with color-coded cells based on significance
#' thresholds for feature importance and p-value columns.
#'
#' @return A dataframe and a datatable object with color-coded cells based on significance
#' thresholds for feature importance and p-value columns.
#'
#' @examples
#' 
#' df <- data.frame(
#'   feature1 = rnorm(10),
#'   feature2 = rnorm(10, mean = 5),
#'   feature3 = runif(10, min = 0, max = 10),
#'   feature4 = c(rnorm(5), rnorm(5, mean = 5)),
#'   categ = c(rep("Cat1",5), rep("Cat2", 5))
#' )
#' 
#' results <- XAI.test(df, y = "categ", simData = TRUE)
#' 
#' my_map <- mapPvalImportance(results)
#' my_map$df
#' my_map$dt
#'
#' @export

mapPvalImportance <- function(objXAI, refPvalColumn="adjpval",
        featImpColumns="feat", pvalColumns = NULL, refPval=0.05){
    if (!is(objXAI, "ObjXAI")){
        stop("objXAI must be an object of class ObjXAI")
    }
    if(!is.numeric(refPval)){
        stop("refPval must be a numeric value")
    }
    df <- getMetricsTable(objXAI)
    if (is.null(pvalColumns)){
        pvalColumns <- grep("pval", colnames(df), ignore.case=TRUE, value=TRUE)
    }
    fiThresholds <- getFeatImpThresholds(df, refPvalColumn,
                                            featImpColumns, refPval)
    xaiResultsRound <- data.frame(lapply(df, function(x) {
        if(is.numeric(x)) signif(x, digits = 3) else x
    }))
    rownames(xaiResultsRound) <- rownames(df)
    df4raw <- xaiResultsRound
    dt <- DT::datatable(xaiResultsRound)
    columnsOrder <- c()

    for (col in pvalColumns){
        df4raw[[paste0("isSign_",col)]] <- 0
        df4raw[[paste0("isSign_",col)]][df4raw[[col]] < refPval] <- 1
        columnsOrder <- c(columnsOrder, col, paste0("isSign_",col))
        dt <- DT::formatStyle(dt, 
          columns = col,
          backgroundColor = DT::styleInterval(c(refPval),
                                          c('cornflowerblue', 'white'))
        )
    }

    for (col in names(fiThresholds)) {
        df4raw[[paste0("isSign_",col)]] <- 0
        thresh <- fiThresholds[[col]] * 0.99
        df4raw[[paste0("isSign_",col)]][df4raw[[col]] > thresh] <- 1
        columnsOrder <- c(columnsOrder, col, paste0("isSign_",col))
        dt <- DT::formatStyle(dt, 
          columns = col,
          backgroundColor = DT::styleInterval(c(thresh, thresh*2),
                                          c('white', 'lightgreen', 'green'))
        )
    }
    return( list(df=df4raw[,columnsOrder], dt=dt) )
}