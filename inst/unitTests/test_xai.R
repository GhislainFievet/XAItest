library(RUnit)
library(XAItest)

runTests <- function() {
    test_XAItest()
}

test_XAItest <- function(){
    df <- data.frame(
    feature1 = rnorm(10),
    feature2 = rnorm(10, mean = 5),
    feature3 = runif(10, min = 0, max = 10),
    feature4 = c(rnorm(5), rnorm(5, mean = 5)),
    categ = c(rep("Cat1",5), rep("Cat2", 5))
    )

    results <- XAI.test(df, y = "categ")
    checkTrue(nrow(results@data) == 10)
    checkTrue(ncol(results@data) == 5)
    checkTrue(all(colnames(results@metricsTable) == c("ttest_pval",
        "ttest_adjPval", "ebayes_pval", "ebayes_adjPval", "lm_coef",
        "lm_pval","lm_adjPval", "RF_feat_imp", "SHAP_feat_imp",
        "LIME_feat_imp")))    
    p1 <- plotModel(results, "lm_pval", "feature1", "feature2")
    checkTrue("ggplot" %in% class(p1))
    checkTrue('data' %in% names(p1))
    checkTrue('scales' %in% names(p1))
    checkTrue(length(p1)>0)

    df <- data.frame(
    feature1 = rnorm(10),
    feature2 = rnorm(10, mean = 5),
    feature3 = runif(10, min = 0, max = 10),
    feature4 = c(rnorm(5), rnorm(5, mean = 5)),
    categ = c(rep(1,5), 1:5)
    )
    results <- XAI.test(df, y = "categ")
    p1 <- plotModel(results, "lm_pval", "feature1", "feature3")
    checkTrue(nrow(results@data) > 0)
    checkTrue(length(p1)>0)
    p1 <- plotModel(results, "RF_feat_imp", "feature1", "categ")
    checkTrue(length(p1)>0)
    results <- setMetricsTable(results, data.frame(a=1:2, b=2:3))
    checkTrue(all(colnames(getMetricsTable(results)) == c("a", "b")))
}