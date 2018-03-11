#' @title Calculation of the effect size r for continuous predictors in GLM and multiple regression models
#'
#' @description <ES for continuous predictors in general linear and multiple
#' regression models, calculated using t values, which are generally obtained from
#'  a difference between estimates (e.g. means or slopes) divided by the standard
#'  error of the differences - almost all statistical software provides t values
#'  when a statistical model is constructed. The t values obtained for a continuous
#'   predictor variable can be used for calculating r.>
#' @param model tested general linear or multiple-regression models with continuous predictors
#' @param t t-value of the continuous predictor d is to be calculated for
#' @param df degrees of freedom in the model
#' @return r statistics for continuous predictors in GLM and multiple regression models
#' @export

multiple_r <- function(model,
                   t,
                   df
){
  r <- round(sqrt(t^2/(t^2+df)),5)

  cat(model$data.name, "Effect Size, r = ", r)
}
