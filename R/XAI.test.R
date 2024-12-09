#' The XAI.test function complements t-test and correlation analyses in feature
#' discovery by integrating eXplainable AI techniques such as feature
#' importance, SHAP, LIME, or custom functions. It provides the option of
#' automatic integration of simulated data to facilitate matching significance
#' between p-values and feature importance.
#' 
#' @details
#' The XAI.test function is designed to extend the capabilities of conventional
#' statistical analysis methods for feature discovery, such as t-tests and
#' correlation, by incorporating techniques from explainable AI (XAI), such as
#' feature importance, SHAP, LIME, or custom functions.
#' This function aims at identifying significant features that influence a
#' given target variable in a dataset, supporting both categorical and
#' numerical target values.
#' A key feature of XAI.test is its ability to automatically incorporate
#' simulated data into the analysis. This simulated data is specifically
#' designed to establish significance thresholds for feature importance values
#' based on the p-values. This capability is useful for reinforcing the
#' reliability of the feature importance metrics derived from machine learning
#' models, by directly comparing them with established statistical significance
#' metrics.

#' @param data SummarizedExperiment or dataframe containing the data. If
#'      dataframe rows are samples and columns are features.
#' @param y Name of the SummarizedExperiment metadata or column of the
#'      dataframe containing the target variable. Default to "y".
#' @param featImpAgr Can be "mean" or "max_abs". It defines how the feature
#'      importance is aggregated.
#' @param simData If TRUE, a simulated feature column is added to the dataframe
#'      to target a defined p-value that will serve as a benchmark for
#'      determining the significance thresholds of feature importances.
#' @param simMethod Method used to generate the simulated data. Can be
#'      "regrnorm" or "rnorm", "regnorm" by default.
#'      "regrnorm" creates simulated data points that match specific
#'      percentiles within a normal distribution, defined by a given mean and
#'      standard deviation. "rnorm" creates simulated data points that follow a
#'      normal distribution.
#'      "regrnorm is more accurate in targeting the specified p-value.
#' @param simPvalTarget Target p-value for the simulated data. It is used to
#'      determine the significance thresholds of feature importances.
#' @param adjMethod Method used to adjust the p-values. "bonferroni" by
#'      default, can be any other method available in the p.adjust function.
#' @param customPVals List of custom functions that compute p-values. The
#'      functions must take the dataframe and the target variable as arguments
#'      and return a names list with:
#' - 'pvals' => a dataframe with the p-values.
#' - 'adjPVal' => a dataframe with the adjusted p-values. Optional.
#' - 'model' => the prediction model object. Optional.
#' @param customFeatImps List of custom functions that compute feature
#'      importances. The functions must take the dataframe and the target
#'      variable as arguments and return a names list with:
#' - 'featImps' => a dataframe with the feature importances. The names of the
#'      functions will be used as the column names in the output dataframe.
#'      Mandatory.
#' - 'model' => the predictionmodel object. Optional.
#' @param modelType Type of the model. Can be "classification", "regression" or
#'      "default". If "default", the function will try to infer the model type
#'      from the target variable. If the target variable is a character, the
#'      model type will be "classification". If the target variable is numeric,
#'      the model type will be "regression".
#' @param corMethod Method used to compute the correlation between the features
#'      and the target variable. "pearson" by default, can be any other method
#'      available in the cor.test function.
#' @param defaultMethods List of default p-values and feature importances
#'      methods to compute. By default "ttest", "ebayes", "cor", "lm", "rf",
#'      "shap" and "lime".
#' @param caretMethod Method used by the caret package to train the model.
#'      "rf" by default.
#' @param caretTrainArgs List of arguments to pass to the caret::train
#'      function. Optional.
#' @param verbose If TRUE, the function will print messages to the console.
#' @return A dataframe containing the pvalues and the feature importances of
#'      each features computed by the different methods.
#' @examples 
#' 
#' library(S4Vectors)
#' library(SummarizedExperiment)
#' 
#' # With a dataframe
#' data <- data.frame(
#'   feature1 = rnorm(100),
#'   feature2 = rnorm(100, mean = 5),
#'   feature3 = runif(100, min = 0, max = 10),
#'   feature4 = c(rnorm(50), rnorm(50, mean = 5)),
#'   y = c(rep("Cat1", 50), rep("Cat2", 50))
#' )
#' 
#' results <- XAI.test(data, y = "y", verbose = TRUE)
#' results
#' 
#' # With a SummarizedExperiment
#' assays <- SimpleList(counts = as.matrix(t(data[, 1:4])))
#' colData <- DataFrame(y = data[,"y"])
#' se <- SummarizedExperiment(assays = assays,
#'                           colData = colData)
#' results <- XAI.test(se, y = "y", verbose = TRUE)
#' results
#' 
#' @export 
XAI.test <- function(data, y="y", featImpAgr="mean", simData=FALSE,
                    simMethod="regrnorm",
                    simPvalTarget=0.045, adjMethod="bonferroni", 
                    customPVals=NULL, customFeatImps=NULL,
                    modelType="default", corMethod="pearson",
                    defaultMethods=c("ttest", "ebayes", "cor", "lm",
                            "rf", "shap", "lime"),
                    caretMethod="rf", caretTrainArgs=NULL, verbose=FALSE){
    
    # test the inputs
    if (!is.data.frame(data) & !is(data, "SummarizedExperiment")){
        stop("The data must be a dataframe or a SummarizedExperiment object")
    }
    if ( is(data, "SummarizedExperiment") ){
        data_matrix <- assay(data, "counts")
        data_matrix <- t(data_matrix)
        metadata <- as.data.frame(colData(data))
        if ( is.factor(metadata[[y]]) ){
            metadata[[y]] <- as.character(metadata[[y]])
        }
        data <- as.data.frame(cbind(data_matrix, y = metadata[[y]]))
        colnames(data)[ncol(data)] <- y
        for (col in names(data)) {
            if (col != y && !is.numeric(data[[col]])) {
                data[[col]] <- as.numeric(as.character(data[[col]]))
            }
        }
    }
    if (!is.character(y)){
        stop("The y must be a character")
    }
    if (! featImpAgr %in% c("mean", "max_abs")){
        stop("The featImpAgr must be a 'mean' or 'max_abs'")
    }
    if (!is.logical(simData)){
        stop("The simData must be a logical")
    }
    if (! simMethod %in% c("regrnorm", "rnorm")){
        stop("The simMethod must be a 'regrnorm' or 'rnorm'")
    }
    if (!is.numeric(simPvalTarget)){
        stop("The simPvalTarget must be a numeric")
    }
    if (!is.character(adjMethod)){
        stop("The adjMethod must be a character")
    }
    if (!modelType %in% c("classification", "regression", "default")){
        stop("The modelType must be a 'classification',
                    'regression' or 'default'")
    }
    if (!is.character(corMethod)){
        stop("The corMethod must be a character")
    }
    if (!is.character(caretMethod)){
        stop("The caretMethod must be a character")
    }
    if (!is.logical(verbose)){
        stop("The verbose must be a logical")
    }

    if (modelType == "default"){
        if (is(data[[y]], "character")){
            modelType <- "classification"
        }
        if (is.numeric(data[[y]])){
            modelType <- "regression"
        }
    }

    if (modelType == "classification" ){
        results <- .XAIclassif(data, y, featImpAgr=featImpAgr,
                            simData=simData, simMethod=simMethod,
                            simPvalTarget=simPvalTarget,
                            adjMethod=adjMethod,
                            customPVals=customPVals,
                            customFeatImps=customFeatImps,
                            defaultMethods=defaultMethods,
                            caretMethod=caretMethod,
                            caretTrainArgs=caretTrainArgs,
                            verbose=verbose)
    }

    if (modelType == "regression" ){
        results <- .XAIregress(data, y, featImpAgr=featImpAgr,
                            simData=simData,simMethod=simMethod,
                            adjMethod=adjMethod,
                            customPVals=customPVals,
                            customFeatImps=customFeatImps,
                            defaultMethods=defaultMethods,
                            caretMethod=caretMethod,
                            caretTrainArgs=caretTrainArgs,
                            verbose=verbose)
    }

    argsList <- list(y=y, featImpAgr=featImpAgr, simData=simData,
                    simMethod=simMethod,
                    simPvalTarget=simPvalTarget, adjMethod=adjMethod,
                    customPVals=customPVals, customFeatImps=customFeatImps,
                    modelType=modelType, corMethod=corMethod,
                    defaultMethods=defaultMethods, caretMethod=caretMethod,
                    caretTrainArgs=caretTrainArgs)

    if (simData){

        results <- new("ObjXAI", data=data,
            dataSim=results$dataSim,
            metricsTable=results$metricsTable,
            models=results$models,
            modelPredictions=results$modelPredictions,
            args=argsList
        )

    } else {

        results <- new("ObjXAI", data=data,
            metricsTable=results$metricsTable,
            models=results$models,
            modelPredictions=results$modelPredictions,
            args=argsList
        )

    }
    return(results)
}

.XAIclassif <- function(data, y="y", featImpAgr="mean", simData=FALSE,
                    simMethod="regnorm", simPvalTarget=0.045,
                    adjMethod="bonferroni", customPVals=NULL,
                    customFeatImps=NULL, defaultMethods=c("ttest",
                                "ebayes", "cor", "lm", "rf", "shap", "lime"),
                    caretMethod="rf", caretTrainArgs=NULL,
                    verbose=FALSE){

    listModels <- list()
    listModelPredictions <- list()
    data <- data[order(data[[y]]),]
    # Simulated data is added to target a defined p-value that will serve as a
    # benchmark for determining the significance thresholds of feature
    # importances.

    if (simData){
        data <- cbind(data,
                    genSimulatedFeatures(data, y,
                                         method=simMethod,
                                         pvalTarget=simPvalTarget))
    }
    # Compute pval statistics
    if ( "ttest" %in% defaultMethods || ! "ebayes" %in% defaultMethods){
        results <- pValTTest(data, y, adjMethod)
        results <- results[order(results$ttest_pval),]
    } else {
        results <- pValEBayes(data, y, adjMethod)
        results <- results[order(results$ebayes_pval),]
    }

    
    if("ebayes" %in% defaultMethods && "ttest" %in% defaultMethods){
        results <- cbind(results,
                        pValEBayes(data, y, adjMethod)[rownames(results),])
    }

    if ( "lm" %in% defaultMethods ){
        pvlm <- pValLM(data, y, adjMethod=adjMethod)
        listModels[["lm_pval"]] <- pvlm$model
        results <- cbind(results, pvlm$pvals[rownames(results),])
    }

    # Compute features importance
    if("rf" %in% defaultMethods){
        firf <- featureImportanceRF(data, y)
        listModels[["RF_feat_imp"]] <- firf$model
        results[["RF_feat_imp"]] <- firf$featImps[rownames(results),]
    }

    if("shap" %in% defaultMethods){
        featImpSHAP <- featureImportanceShap(data, y, featImpAgr=featImpAgr,
        caretMethod=caretMethod, caretTrainArgs=caretTrainArgs)
        # listModels[["SHAP_feat_imp"]] <- featImpSHAP$model
        listModelPredictions[["SHAP_feat_imp"]] <- featImpSHAP$modelPredictions
        results[["SHAP_feat_imp"]] <- featImpSHAP$featImps[rownames(results)]
    }

    if("lime" %in% defaultMethods){
        featImpLime <- featureImportanceLime(data, y, featImpAgr=featImpAgr,
        caretMethod=caretMethod, caretTrainArgs=caretTrainArgs)
        listModels[["LIME_feat_imp"]] <- featImpLime$model
        results[["LIME_feat_imp"]] <- featImpLime$featImps[rownames(results)]
    }

    # Add the feature importance from the custom functions
    for (custFI in names(customFeatImps)){
        if (verbose){ message("Add custom feature importance:",custFI)}
        cfi <- customFeatImps[[custFI]](data,y,featImpAgr=featImpAgr)
        if(length(grep("_feat_imp$", custFI)) == 0){
            custFI <- paste0(custFI, "_feat_imp")
        }
        if ("model" %in% names(cfi)){
            listModels[[custFI]] <- cfi$model
        }
        if ("modelPredictions" %in% names(cfi)){
            listModelPredictions[[custFI]] <- cfi$modelPredictions
        }
        results[[custFI]] <- cfi$featImps[rownames(results)]
    }
    for (custPV in names(customPVals)){
        if (verbose){ message("Add custom p-values:",custPV)}
        cpv <- customPVals[[custPV]](data,y)
        results[[paste0(custPV,"_pval")]] <- cpv$pvals[rownames(results)]
        if (paste0(custPV,"_adjPval") %in% names(cpv)){
            results[[paste0(custPV,"_adjPval")]] <-
                cpv$adjPVal[rownames(results)]
        }
    }
    if(simData){
        return(list(dataSim=data,
            metricsTable=results,
            models=listModels,
            modelPredictions=listModelPredictions))
    }
    return(list(metricsTable=results,
        models=listModels,
        modelPredictions=listModelPredictions))
}


.XAIregress <- function(data, y="y", featImpAgr="mean", simData=FALSE,
                            simMethod="regrnorm",
                            adjMethod="bonferroni",
                            customPVals=NULL, customFeatImps=NULL,
                            corMethod="pearson", simPvalTarget=0.01,
                            defaultMethods=c("ttest", "ebayes", "cor",
                                    "lm", "rf", "shap", "lime"),
                            tolerance=0.0005,
                            caretMethod="rf", caretTrainArgs=NULL,
                            verbose=NULL){
    listModels <- list()
    listModelPredictions <- list()
    if (simData){
        data <- cbind(data,
                    genSimulatedFeaturesRegr(data, y,
                                        method=simMethod,
                                        pvalTarget=simPvalTarget,
                                        tolerance=tolerance))
    }

    if("cor" %in% defaultMethods || ! "lm" %in% defaultMethods){
        results <- pValCor(data, y, adjMethod=adjMethod, corMethod=corMethod)
    } else {
        pvlm <- pValLM(data, y, adjMethod=adjMethod)
        listModels[["lm_pval"]] <- pvlm$model
        results <- pvlm$pvals
    }
  
    if ("lm" %in% defaultMethods && "cor" %in% defaultMethods){
        pvlm <- pValLM(data, y, adjMethod=adjMethod)
        listModels[["lm_pval"]] <- pvlm$model
        results <- cbind(results, pvlm$pvals[rownames(results),])
    }
    
    if ("rf" %in% defaultMethods){
        firf <- featureImportanceRF(data, y, modelType="regression")
        listModels[["RF_feat_imp"]] <- firf$model
        results[["RF_feat_imp"]] <- firf$featImps[rownames(results),]
    }
    if ("shap" %in% defaultMethods){
        featImpSHAP <- featureImportanceShap(data, y, featImpAgr=featImpAgr,
                            modelType="regression")
        listModels[["SHAP_feat_imp"]] <- featImpSHAP$model
        results[["SHAP_feat_imp"]] <- featImpSHAP$featImps[rownames(results)]
    }
    if ("lime" %in% defaultMethods){
        featImpLime <- featureImportanceLime(data, y, featImpAgr=featImpAgr,
                            modelType="regression")
        listModels[["LIME_feat_imp"]] <- featImpLime$model
        results[["LIME_feat_imp"]] <- featImpLime$featImps[rownames(results)]
    }
    
#   Add the feature importance from the custom functions
    for (custFI in names(customFeatImps)){
        if (verbose){ message("Add custom feature importance:",custFI)}
        cfi <- customFeatImps[[custFI]](data, y, featImpAgr=featImpAgr)
        if(length(grep("_feat_imp$", custFI)) == 0){
            custFI <- paste0(custFI, "_feat_imp")
        }
        if ("model" %in% names(cfi)){
            listModels[[custFI]] <- cfi$model
        }
        if ("modelPredictions" %in% names(cfi)){
            listModelPredictions[[custFI]] <- cfi$modelPredictions
        }
        results[[custFI]] <- cfi$featImps[rownames(results)]
    }
    if ( length(grep("pval", colnames(results))) > 0){
        results <- results[order(results[[grep("pval",
                                colnames(results), value=TRUE)[1]]]),]
    }
    if(simData){
        return(list(dataSim=data,
            metricsTable=results,
            models=listModels,
            modelPredictions=listModelPredictions))
    }
    return(list(metricsTable=results,
        models=listModels,
        modelPredictions=listModelPredictions))
}
genSimulatedFeaturesRegr <- function(data, y="y",
                                    method="regrnorm",
                                    pvalTarget=0.01,
                                    tolerance=0.0005){
    newSD <- .findNoiseSD(data[[y]], ncol(data)-1, sdSup=100,
                            pvalTarget=pvalTarget,
                            tolerance=tolerance)
    if (method == "regrnorm"){
        values <- data[[y]] + .regRNorm(length(data[[y]]), 0, newSD)
    }
    if (method == "rnorm"){
        values <- data[[y]] + rnorm(length(data[[y]]), 0, newSD)
    }
    dfSimu <- as.data.frame(values)
    colnames(dfSimu) = "simFeat"
    return(dfSimu)
}

.findNoiseSD <- function(values, nf,
                    pvalTarget=0.01,
                    tolerance=0.0005,
                    sdInf=0,
                    sdSup=10){
    newSD <- (sdSup + sdInf)/2
    tempPval <- cor.test(values, values +
                    .regRNorm(length(values), 0, newSD))$p.value * (nf+1)
    #if (abs(tempPval - pvalTarget) < tolerance){
    if (pvalTarget - tempPval < tolerance && pvalTarget - tempPval > 0){
        return (newSD)
    }
    if (tempPval - pvalTarget > 0){
        return (.findNoiseSD(values, nf, pvalTarget, tolerance,
                                sdInf=sdInf, sdSup=newSD))
    }
    tempPval <- cor.test(values, values +
                    .regRNorm(length(values), 0, sdSup))$p.value * (nf+1)
    if (tempPval - pvalTarget < 0){
        return (.findNoiseSD(values, nf, pvalTarget, tolerance,
                                sdInf=sdSup, sdSup=sdSup*2))
    } else {
        return (.findNoiseSD(values, nf, pvalTarget, tolerance,
                                sdInf=newSD, sdSup=sdSup))
    }
}

pValCor <- function(data, y="y", adjMethod='bonferroni', corMethod="pearson"){
    outputs <- data[[y]]
    featCols <- colnames(data)[colnames(data) != y]
    results <- as.data.frame(t(apply(t(data[,featCols]), 1, function(x) {
        corTest <- cor.test(x, outputs)
        pval <- corTest$p.value
        featCor <- corTest$estimate
        c(cor=featCor, cor_pval=pval)
    })))
    results$cor_adjPval <- p.adjust(results$cor_pval, method=adjMethod)
    colnames(results) <- c("cor","cor_pval","cor_adjPval")
    results
}

pValLM <- function(data, y="y", adjMethod='bonferroni'){
    if (is(data[[y]], "character")){
        data[[y]][data[[y]] == unique(data[[y]])[1]] <- 0
        data[[y]][data[[y]] == unique(data[[y]])[2]] <- 1
    }
    featCols <- colnames(data)[colnames(data) != y]
    myFormula <- as.formula(paste0(y, " ~ ."))
    model <- lm(myFormula, data=data)
    summaryModel <- summary(model)$coefficient
    
    keptFeatCols <- intersect(featCols, rownames(summaryModel))
    lm_coef <- summaryModel[keptFeatCols, "Estimate"]
    lm_pval <- summaryModel[keptFeatCols, "Pr(>|t|)"]
    lm_adjPval <- p.adjust(lm_pval, method=adjMethod)
    results <- data.frame(lm_coef=lm_coef,
                          lm_pval=lm_pval,
                          lm_adjPval=lm_adjPval)
    # Add not kept features
    notKeptFeatCols <- setdiff(featCols, rownames(summaryModel))
    resNotKepts <- data.frame(lm_coef=rep(NA, length(notKeptFeatCols)),
                          lm_pval=rep(NA, length(notKeptFeatCols)),
                          lm_adjPval=rep(NA, length(notKeptFeatCols)))
    rownames(resNotKepts) <- notKeptFeatCols
    results <- rbind(results, resNotKepts)
    results <- results[featCols,]
    return(list(pvals=results, model=model))
}


pValEBayes <- function(data, y="y", adjMethod='bonferroni'){
    data[[y]] <- as.factor(data[[y]])
    myFormula <- as.formula(paste0("~ 0 + ", y))
    design <- model.matrix(myFormula, data=data)
    colnames(design) <- levels(data[[y]])
    data <- t(data[, names(data)[names(data) != y]])
    data <- as.data.frame(data)
    fit <- limma::lmFit(data, design)

    matContrast = data.frame(myContrast=c(1,-1))
    rownames(matContrast) = c(unique(data[[y]])[1], unique(data[[y]])[2])
    matContrast = as.matrix(matContrast)

    fit <- limma::contrasts.fit(fit, matContrast)
    fit <- limma::eBayes(fit)
    results <- limma::topTable(fit, coef=1, n=Inf, adjust=adjMethod)
    results <- results[,c("P.Value","adj.P.Val")]
    colnames(results) <- c("ebayes_pval", "ebayes_adjPval")
    results
}

pValTTest <- function(data, y="y", adjMethod='bonferroni'){
    featCols <- colnames(data)[colnames(data) != y]
    ttestPvals <- apply(t(data[,featCols]), 1, function(x) {
        valCateg1 <- x[data[[y]] == unique(data[[y]])[1]]
        valCateg2 <- x[data[[y]] == unique(data[[y]])[2]]
        if (length(unique(c(valCateg1, valCateg2))) == 1){
            return(1)
        }
        if ( length(unique(valCateg1)) == 1 & length(unique(valCateg2)) == 1){
            return(0)
        }
        pval <- t.test(valCateg1, valCateg2)$p.value
        pval
    })
    adjPvals <- p.adjust(ttestPvals, method=adjMethod)
    results <- data.frame(ttest_pval=ttestPvals, ttest_adjPval=adjPvals)
    rownames(results) <- featCols
    results
}

featureImportanceRF <- function(data, y="y", modelType = "classification") {
    if (modelType == "classification") {
        data[[y]] <- as.factor(data[[y]])
    }
    
    myFormula <- as.formula(paste0(y, " ~ ."))
    rfModel <- randomForest::randomForest(myFormula, data = data, ntree = 50)
    results <- randomForest::importance(rfModel)
    list(featImps = results, model = rfModel)
}

featureImportanceShap <- function(data, y="y", featImpAgr="mean",
    modelType="classification",
    caretMethod='rf', caretTrainArgs=NULL){

    if (modelType == "classification"){
        dfOr <- data
        myA <- unique(data[[y]])[1]
        myB <- unique(data[[y]])[2]
        data = data[order(data[[y]]),]
        n1 = sum(data[[y]] == unique(data[[y]])[1])
        n2 = sum(data[[y]] == unique(data[[y]])[2])
        sampaTrain <- sample(1:n1, round(n1/2))
        sampaTest <- setdiff(1:n1, sampaTrain)
        sampbTrain <- sample((n1 + 1):(n1 + n2), round(n2/2))
        sampbTest <- setdiff((n1 + 1):(n1 + n2), sampbTrain)
        dfTrain <- data[c(sampaTrain, sampbTrain),]
        dfTrain[[y]][dfTrain[[y]] == myA] <- 0
        dfTrain[[y]][dfTrain[[y]] == myB] <- 1
        dfTrain[[y]] <- as.numeric(dfTrain[[y]])
        dfTest <- data[c(sampaTest, sampbTest),]
        dfTest[[y]][dfTest[[y]] == myA] <- 0
        dfTest[[y]][dfTest[[y]] == myB] <- 1
        dfTest[[y]] <- as.numeric(dfTest[[y]])
    }
    if (modelType == "regression"){
        sampTrain <- sample(1:nrow(data), round(nrow(data)/2))
        sampTest <- setdiff( 1:nrow(data), sampTrain)
        dfTrain <- data[sampTrain, ]
        dfTest <- data[sampTest, ]
    }

    myFormula <- as.formula(paste0(y, " ~ ."))
    carArgs1 <- list(form=myFormula,
              method=caretMethod,
              data = dfTrain)
    fit <- do.call(caret::train, c(carArgs1, caretTrainArgs))
    # fit <- caret::train(myFormula, method=caretMethod, data = dfTrain)

    bg_X <- dfTest[,c(colnames(dfTest)[colnames(dfTest) != y], y)]
    # bg_X$y[bg_X$y == unique(bg_X$y)[1]] <- 0
    # bg_X$y[bg_X$y == unique(bg_X$y)[2]] <- 1

    featImps <- kernelshap::kernelshap(fit,
                        X = dfTrain[, colnames(dfTrain) != y],
                        bg_X = bg_X,
                        verbose=FALSE)

    if (length(names(featImps$S)) == 0){
        featImps <- as.data.frame(featImps$S)
    } else {
        featImps <- as.data.frame(featImps$S[[names(featImps$S)[1]]])
    }
    
    results <- unlist(lapply(colnames(featImps), function(x){
        if (featImpAgr == "mean"){
            return(mean(unlist(abs(featImps[[x]]))))
        }
        if (featImpAgr == "max_abs"){
            return(max(unlist(abs(featImps[[x]]))))
        }
    }))
    names(results) <- colnames(data)[colnames(data) != y]
    if (modelType == "regression"){
        return(list(featImps = results, model = fit))
    } else {
        myPredictions = predict(fit, dfOr)
        tempMyPreds <- myPredictions
        myPredictions[tempMyPreds < 0.5] = myA
        myPredictions[tempMyPreds >= 0.5] = myB
        return(list(featImps = results, modelPredictions = myPredictions))
    }
}

featureImportanceLime <- function(data, y="y", featImpAgr = "mean",
                                    modelType = "classification",
                                    caretMethod="rf",
                                    caretTrainArgs=NULL) {
    matX <- as.data.frame(data[, colnames(data) != y])
    vecY <- data[[y]]
    
    carArgs1 <- list(x=matX, y=vecY, method=caretMethod)
    model <- do.call(caret::train, c(carArgs1, caretTrainArgs))
    # model <- caret::train(x=matX, y=vecY, method=caretMethod)
    
    explainer <- lime::lime(matX, model)
    
    if (modelType == "classification") {
        explanation <- lime::explain(matX, explainer,
                                        n_labels = 1, n_features = ncol(matX))
    } else {
        explanation <- lime::explain(matX, explainer, n_features = ncol(matX))
    }
    
    results <- unlist(lapply(colnames(matX), function(x) {
        myMask <- explanation$feature == x
        if (sum(myMask) == 0) {
            return(0)
        } else {
            featWeights <- explanation[myMask, "feature_weight"]
            if (featImpAgr == "mean") {
                return(mean(unlist(abs(featWeights))))
            }
            if (featImpAgr == "max_abs") {
                return(max(unlist(abs(featWeights))))
            }
        }
    }))
    
    names(results) <- colnames(data)[colnames(data) != y]
    list(featImps = results, model = model)
}

.findGM <- function(n1, n2, nf, pvalTarget=0.01,
                    tolerance=0.0005, gmInf=0, gmSup=0.5){
    newGM <- (gmSup + gmInf)/2
    newDistrib <- .distribFromGM(newGM, 10)
    vec1 <- .regRNorm(n1, 10 - newDistrib$deltMean, newDistrib$newSD)
    vec2 <- .regRNorm(n2, 10 + newDistrib$deltMean, newDistrib$newSD)
    tempPval <- t.test(vec1, vec2)$p.value * (nf + 1)
    if (pvalTarget - tempPval < tolerance && pvalTarget - tempPval > 0){
        return (newGM)
    }
    if (tempPval - pvalTarget < 0){
        return (.findGM(n1, n2, nf, pvalTarget, tolerance,
                        gmInf=newGM, gmSup=gmSup))
    }
    if (tempPval - pvalTarget > 0){
        return (.findGM(n1, n2, nf, pvalTarget, tolerance,
                        gmInf=gmInf, gmSup=newGM))
    }
}

.distribFromGM <- function(gm, sd) {
  x <- 4 * sd * gm - 2 * sd
  y <- -4 * sd * (gm - 0.5)^2 + sd
  return(list(deltMean = unname(x), newSD = unname(y)))
}

.regRNorm <- function(n, mean, sd){
    percentiles <- (1:n)/(n+1)
    percentileValues <- qnorm(percentiles, mean, sd)
    return(percentileValues)
}

# Add simulated data targeting a pvalue target
genSimulatedFeatures <- function(data, y="y", method="regrnorm",
                                    pvalTarget=0.01){
    categ1 <- unique(data[[y]])[1]
    categ2 <- unique(data[[y]])[2]
    n1 <- sum(data[[y]] == categ1)
    n2 <- sum(data[[y]] == categ2)
    nFeatures <- length(colnames(data)) - 1
    features <- colnames(data)[colnames(data) != y]
    mean <- mean(unlist(apply(t(data[,features]), 1, mean)))
    sd <- mean(unlist(apply(t(data[,features]), 1, sd)))
    gm <- .findGM(n1, n2, nFeatures, pvalTarget=pvalTarget)
    newDistrib <- .distribFromGM(gm, sd)
    if (method == "regrnorm"){
        vec1 <- .regRNorm(n1, mean - newDistrib$deltMean, newDistrib$newSD)
        vec2 <- .regRNorm(n2, mean + newDistrib$deltMean, newDistrib$newSD)
    }
    if (method == "rnorm"){
        vec1 <- rnorm(n1, mean - newDistrib$deltMean, newDistrib$newSD)
        vec2 <- rnorm(n2, mean + newDistrib$deltMean, newDistrib$newSD)
    }
    dfSimu <- as.data.frame(c(vec1,vec2))
    colnames(dfSimu) = "simFeat"
    return(dfSimu)
}