#' contXAI class
#' 
#' \code{contXAI} is a class used to store the output values
#' of the \code{XAI.test} function.
#' @rdname contXAI
#' @return A contXAI object
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
            print(object@metricsTable[1:min(5, nrow(object@metricsTable)),
                                    1:min(7, ncol(object@metricsTable))])
        })