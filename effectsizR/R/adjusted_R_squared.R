#' @title Calculation of adjusted R squared for univariate models
#'
#' @description <R2 represents the variance of the data that is accounted for by
#' a particular model and should be used for predictors in univariate models only
#' (for a particular predictor in multivariate models, we recommend effect sizes
#' estimated from t values or raw data). Often, the squareroot of R2 is used as an
#' effect statistic in meta-analysis when models include one predictor, and even if
#' they include more than one predictor. As the square-root of R2 provides a biased
#'  effect-size estimate of a predictor of interest and as this bias is particularly
#'   severe if sample size is small, we corrected for this bias.>
#' @param N number of subjects in the tested model
#' @param R_squared variance of data which is accounted for by tested model
#' @param k_predictors number of predictors in the model (not including the intercept)
#' @return adjusted R squared for univariate models
#' @export

r_squared <- function(model,
                      N,
                      R_squared,
                      k_predictors
){
  r <- round(sqrt((1)-((N-1)*(1-R_squared))/(N-k_predictors-1)),5)
  cat(model$data.name, "Effect Size, r = ", r)
}
