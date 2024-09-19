#' objXAI class
#' 
#' \code{objXAI} is a class used to store the output values
#' of the \code{XAI.test} function.
#' @rdname objXAI
#' @return A objXAI object
#' @examples
#' 
#' a=1
contXAI <- methods::setClass("objXAI",
        slots = list(
        data = "data.frame",
        dataSim = "data.frame",
        metricsTable = "data.frame",
        map = "ANY",
        models = "list",
        modelPredictions = "list",
        args = "list"
        ))
setMethod("show", "objXAI",
        function(object) {
            print(object@metricsTable[1:min(5, nrow(object@metricsTable)),])
        })