#' Plot the model
#' 
#' This function plots the model.
#' 
#' @param objXAI The ObjXAI object created with the XAItest function
#' @param modelName The name of the model, can be found in
#'      'names(objXAI@models)'
#' @param xFeature The x feature
#' @param yFeature The y feature
#' 
#' @return A plot
#' 
#' @examples
#' 
#' data(iris)
#' iris = subset(iris, Species == "setosa" | Species == "versicolor")
#' iris$Species = as.character(iris$Species)
#' objXAI <- XAI.test(iris, y = "Species")
#' plotModel(objXAI, "RF_feat_imp", "Sepal.Length", "Sepal.Width")
#' 
#' @export
plotModel <- function(objXAI, modelName, xFeature, yFeature=""){
    if(!is(objXAI, "ObjXAI")){
        stop("The object must be a ObjXAI class")
    }
    if(!is.character(modelName)){
        stop("The modelName must be a character")
    }
    if(!is.character(xFeature)){
        stop("The xFeature must be a character")
    }
    if(!is.character(yFeature)){
        stop("The yFeature must be a character")
    }
    if(!modelName %in% c(names(objXAI@models),names(objXAI@modelPredictions))){
        stop("The modelName is not in the models list")
    }
    if(!objXAI@args$simData && !xFeature %in% colnames(objXAI@data)){
        stop("The xFeature is not in the data")
    }
    if(objXAI@args$simData && !xFeature %in% colnames(objXAI@dataSim)){
        stop("The xFeature is not in the data with added simulated feature")
    }
    if(!objXAI@args$simData && !yFeature %in% colnames(objXAI@data) &&
        objXAI@args$modelType == "classification"){
        stop("The yFeature is not in the data")
    }
    if(objXAI@args$simData && !yFeature %in% colnames(objXAI@dataSim) &&
        objXAI@args$modelType == "classification"){
        stop("The yFeature is not in the data")
    }
    model <- objXAI@models[[modelName]]
    predictions = NULL
    if (objXAI@args$modelType == "classification"){
        if ( modelName %in% names(objXAI@modelPredictions)){
            pred <- objXAI@modelPredictions[[modelName]]
            if ( objXAI@args$simData){
                truth <- objXAI@dataSim[[objXAI@args$y]]
                x <- objXAI@dataSim[, xFeature]
                y <- objXAI@dataSim[, yFeature]
            } else {
                truth <- objXAI@data[[objXAI@args$y]]
                x <- objXAI@data[, xFeature]
                y <- objXAI@data[, yFeature]
            }
        } else {
            if (objXAI@args$simData) {
                pred <- predict(model, newdata = objXAI@dataSim)
                # If 'pred' is numeric, this indicates that the categorical
                # target was converted to 0 and 1. Therefore, we transform
                # them back to categories.
                if (is.numeric(pred)){
                    tempPred <- pred
                    a_a <- unique(objXAI@dataSim[[objXAI@args$y]])[1]
                    pred[tempPred < 0.5] <- a_a
                    a_a <- unique(objXAI@dataSim[[objXAI@args$y]])[2]
                    pred[tempPred >= 0.5] <- a_a
                }
                truth <- objXAI@dataSim[[objXAI@args$y]]
                x <- objXAI@dataSim[, xFeature]
                y <- objXAI@dataSim[, yFeature]
            } else {
                pred <- predict(model, newdata = objXAI@data)
                if (is.numeric(pred)){
                    tempPred <- pred
                    a_a <- unique(objXAI@data[[objXAI@args$y]])[1]
                    pred[tempPred < 0.5] <- a_a
                    a_a <- unique(objXAI@data[[objXAI@args$y]])[2]
                    pred[tempPred >= 0.5] <- a_a
                }
                truth <- objXAI@data[[objXAI@args$y]]
                x <- objXAI@data[, xFeature]
                y <- objXAI@data[, yFeature]
            }
        }
        df2plot <- data.frame(x = x, y = y,
                    predictions = paste0("Truth: ",truth,", Predicted: ",pred))
        p <- ggplot2::ggplot(data=df2plot,
        		ggplot2::aes(x = x, y = y, color = predictions)) +
            ggplot2::geom_point() + ggplot2::theme_bw() +
            ggplot2::xlab(xFeature) + ggplot2::ylab(yFeature) +
            ggplot2::ggtitle(paste("Model: ", modelName))
    }
    if (objXAI@args$modelType == "regression"){
        if ( modelName %in% names(objXAI@modelPredictions)){
            pred <- objXAI@modelPredictions[[modelName]]
            if ( objXAI@args$simData){
                x <- objXAI@dataSim[, xFeature]
                y <- objXAI@dataSim[[objXAI@args$y]] 
            } else {
                x <- objXAI@data[, xFeature]
                y <- objXAI@data[[objXAI@args$y]]
            }
        } else {
            if (objXAI@args$simData) {
                pred <- predict(model, newdata = objXAI@dataSim)
                x <- objXAI@dataSim[, xFeature]
                y <- objXAI@dataSim[[objXAI@args$y]] 
            } else {
                pred <- predict(model, newdata = objXAI@data)
                x <- objXAI@data[, xFeature]
                y <- objXAI@data[[objXAI@args$y]]
            }
        }
        
        df2plot <- data.frame(x = c(x, x), y = c(y, pred),
                predictions = c(rep("Truth", length(x)),
                                rep("Predicted",length(pred))))
        p <- ggplot2::ggplot(data=df2plot,
        	ggplot2::aes(x = x, y = y, color = predictions)) +
            ggplot2::geom_point() + ggplot2::theme_bw() +
            ggplot2::xlab(xFeature) + ggplot2::ylab(objXAI@args$y) +
            ggplot2::ggtitle(paste("Model: ", modelName))
    }
    return(p)
}
