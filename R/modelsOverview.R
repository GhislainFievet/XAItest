#' Models Overview
#' 
#' Returns **mse**, **rmse**, **mae** and **r2** of regression models or
#' **accuracy**, **precision**, **recall** and **f1_score** of classification models.
#' 
#' @param objXAI An object of class objXAI.
#' 
#' 

#' @export
modelsOverview <- function(objXAI, verbose=FALSE){
    if(!is(objXAI, "objXAI")){
        stop("The object must be a objXAI class")
    }
    if(!is.logical(verbose)){
        stop("verbose must be a logical value")
    }
    if (verbose){
        print(paste("The models are: ", names(objXAI@models)))
    }
    df2predict <- objXAI@data
    if (objXAI@args$simData){
        df2predict <- objXAI@dataSim
    }
    if ( objXAI@args$modelType == "classification"){
        predRows <- expand.grid(unique(df2predict[[objXAI@args$y]]),
            unique(df2predict[[objXAI@args$y]]))
        predRows <- paste0("truth_", predRows[,1], "__predicted_", predRows[,2])
        predRows <- predRows[order(predRows)]
        results <- sapply(unique(c(names(objXAI@models), names(objXAI@modelPredictions))), function(x){
            if ( x %in% names(objXAI@modelPredictions)){
                pred <- objXAI@modelPredictions[[x]]
            } else {
                model <- objXAI@models[[x]]
                pred <- predict(model, newdata = df2predict)
                # If 'pred' is numeric, this indicates that the categorical
                # y was converted to 0 and 1. Therefore, we transform them back
                # to categories.
                if (is.numeric(pred)){
                    tempPred <- pred
                    pred[tempPred < 0.5] <- unique(objXAI@dataSim[[objXAI@args$y]])[1]
                    pred[tempPred >= 0.5] <- unique(objXAI@dataSim[[objXAI@args$y]])[2]
                }
            }
            predAcc <- paste0("truth_", df2predict[[objXAI@args$y]], "__predicted_", pred)
            rp1 <- sum(predAcc == predRows[1])
            rp2 <- sum(predAcc == predRows[2])
            rp3 <- sum(predAcc == predRows[3])
            rp4 <- sum(predAcc == predRows[4])
            confMatrix <- table(Predicted = pred, Actual = df2predict[[objXAI@args$y]])
            accuracy <- sum(diag(confMatrix)) / sum(confMatrix)
            precisionA <- confMatrix[1,1] / sum(confMatrix[,1])
            recallA <- confMatrix[1,1] / sum(confMatrix[1,])
            precisionB <- confMatrix[2,2] / sum(confMatrix[,2])
            recallB <- confMatrix[2,2] / sum(confMatrix[2,])
            f1ScoreA <- 2 * (precisionA * recallA) / (precisionA + recallA)
            f1ScoreB <- 2 * (precisionB * recallB) / (precisionB + recallB)
            c(rp1, rp2, rp3, rp4, accuracy, precisionA, recallA, precisionB,
                recallB, f1ScoreA, f1ScoreB)
        })
        categA <- unique(df2predict[[objXAI@args$y]])[1]
        categB <- unique(df2predict[[objXAI@args$y]])[2]
        rownames(results) <- c(predRows, "accuracy", paste0("precision_", categA),
                paste0("recall_", categA), paste0("precision_", categB),
                paste0("recall_", categB), paste0("f1_score_", categA),
                paste0("f1_score_", categB))
    }
    if ( objXAI@args$modelType == "regression"){
        results <- sapply(unique(c(names(objXAI@models), names(objXAI@modelPredictions))), function(x){
            if ( x %in% names(objXAI@modelPredictions)){
                pred <- objXAI@modelPredictions[[x]]
            } else {
                model <- objXAI@models[[x]]
                pred <- predict(model, newdata = df2predict)
            }
            mse <- mean((df2predict[[objXAI@args$y]] - pred)^2)
            rmse <- sqrt(mse)
            mae <- mean(abs(df2predict[[objXAI@args$y]] - pred))
            r2 <- 1 - sum((df2predict[[objXAI@args$y]] - pred)^2) / sum((df2predict[[objXAI@args$y]] - mean(df2predict[[objXAI@args$y]]))^2)
            c(mse, rmse, mae, r2)
        })
        rownames(results) <- c("mse", "rmse", "mae", "r2")
    }
    return(results)
}