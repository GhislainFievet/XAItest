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
    checkTrue(all(colnames(results@metricsTable) == c("ttest_pval", "ttest_adjPval", "ebayes_pval",
        "ebayes_adjPval", "lm_coef","lm_pval","lm_adjPval", "RF_feat_imp", "SHAP_feat_imp",
        "LIME_feat_imp")))
}