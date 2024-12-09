#' ObjXAI class
#' 
#' \code{ObjXAI} is a class used to store the output values
#' of the \code{XAI.test} function.
#' @rdname ObjXAI
#' @return A ObjXAI object
#' @examples
#' 
#' 
# .objXAI <- methods::setClass("ObjXAI",
#        slots = list(
#        data = "data.frame",
#        dataSim = "data.frame",
#        metricsTable = "data.frame",
#        map = "ANY",
#        models = "list",
#        modelPredictions = "list",
#        args = "list"
#        ))
#setMethod("show", "ObjXAI",
#        function(object) {
#            cat(object@metricsTable[1:min(5, nrow(object@metricsTable)),])
#        })

        # Define the S4 class 'ObjXAI'
.objXAI <- setClass(
  "ObjXAI",
  slots = list(
    data = "data.frame",
    dataSim = "data.frame",
    metricsTable = "data.frame",
    map = "ANY",  # 'ANY' is very generic; consider specifying a more precise class if possible.
    models = "list",
    modelPredictions = "list",
    args = "list"
  )
)

# Accessor for the 'metricsTable' slot
setGeneric("getMetricsTable", function(object) standardGeneric("getMetricsTable"))
setMethod("getMetricsTable", "ObjXAI", function(object) {
  object@metricsTable
})

# Setter for the 'metricsTable' slot
setGeneric("setMetricsTable", function(object, value) standardGeneric("setMetricsTable"))
setMethod("setMetricsTable", "ObjXAI", function(object, value) {
  validObject(object)  # Check if object is valid
  if (!inherits(value, "data.frame")) {
    stop("Value must be a data frame")
  }
  object@metricsTable <- value
  object
})


# Define a 'show' method for objects of class 'ObjXAI'
setMethod(
  "show", 
  "ObjXAI",
  function(object) {
    cat("Metrics Table (first 5 rows):\n")
    print(getMetricsTable(object)[1:min(5, nrow(getMetricsTable(object))), ])
  }
)

