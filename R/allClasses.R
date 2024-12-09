#' ObjXAI class
#' 
#' \code{ObjXAI} is a class used to store the output values
#' of the \code{XAI.test} function.
#' @rdname ObjXAI
#' @return A ObjXAI object
#' @examples
#' 
#' obj <- new("ObjXAI", 
#'            data = data.frame(), 
#'            dataSim = data.frame(), 
#'            metricsTable = data.frame(Metric = c("Accuracy", "Precision"), 
#'                                        Value = c(0.95, 0.89)), 
#'            map = list(), 
#'            models = list(), 
#'            modelPredictions = list(), 
#'            args = list())
#' 

.objXAI <- setClass(
  "ObjXAI",
  slots = list(
    data = "data.frame",
    dataSim = "data.frame",
    metricsTable = "data.frame",
    map = "ANY",
    models = "list",
    modelPredictions = "list",
    args = "list"
  )
)

#' Get the Metrics Table
#'
#' This method retrieves the metrics table from an ObjXAI object.
#' @param object An ObjXAI object.
#' @rdname getMetricsTable
#' @aliases getMetricsTable,ObjXAI-method
#' @return A data frame containing the metrics.
#' @examples
#' 
#' obj <- new("ObjXAI", 
#'            data = data.frame(), 
#'            dataSim = data.frame(), 
#'            metricsTable = data.frame(Metric = c("Accuracy", "Precision"), 
#'                                        Value = c(0.95, 0.89)), 
#'            map = list(), 
#'            models = list(), 
#'            modelPredictions = list(), 
#'            args = list())
#' getMetricsTable(obj)
#' 
#' @export
setGeneric("getMetricsTable", function(object) standardGeneric("getMetricsTable"))
setMethod("getMetricsTable", "ObjXAI", function(object) {
  object@metricsTable
})

#' Set the Metrics Table
#'
#' This method sets the metrics table for an ObjXAI object.
#' @param object An ObjXAI object.
#' @param value A data frame to set as the metrics table.
#' @rdname setMetricsTable
#' @aliases setMetricsTable,ObjXAI-method
#' @return The modified ObjXAI object.
#' @examples
#' 
#' obj <- new("ObjXAI", 
#'            data = data.frame(), 
#'            dataSim = data.frame(), 
#'            metricsTable = data.frame(Metric = c("Accuracy", "Precision"), 
#'                                        Value = c(0.95, 0.89)), 
#'            map = list(), 
#'            models = list(), 
#'            modelPredictions = list(), 
#'            args = list())
#'
#' setMetricsTable(obj, data.frame(Metric = c("Accuracy", "Precision", "Recall"),
#'                                Value = c(0.95, 0.89, 0.91)))
#' 
#' @export
setGeneric("setMetricsTable", function(object, value) standardGeneric("setMetricsTable"))
setMethod("setMetricsTable", "ObjXAI", function(object, value) {
  validObject(object)
  if (!inherits(value, "data.frame")) {
    stop("Value must be a data frame")
  }
  object@metricsTable <- value
  object
})

#' Show Method for ObjXAI
#'
#' Prints the first 5 rows of the metrics table from an ObjXAI object.
#' @param object An ObjXAI object.
#' @return The first 5 rows of the metrics table.
#' @export
setMethod(
  "show", 
  "ObjXAI",
  function(object) {
    cat("Metrics Table (first 5 rows):\n")
    print(getMetricsTable(object)[1:min(5, nrow(getMetricsTable(object))), ])
  }
)

