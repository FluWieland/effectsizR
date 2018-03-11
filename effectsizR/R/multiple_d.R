#' @title Calculation of the effect size Hedges's d for continuous and categorical predictors in GLM and multiple linear regression models
#'
#' @description <ES for continuous and categorical predictors in general linear and
#' multiple regression models, calculated using t-values (obtained from a difference between estimates,
#' e.g. means or slopes, divided by the standard error of the differences - almost all statistical software
#' provides t values when a statistical model is constructed). The t-values obtained for groups or
#' categories in a predictor variable can be used for calculating d.>
#' @param model a list of values as returned testing a general linear or multiple-regression model
#' @param t t-value of the respective predictor
#' @param df degrees of freedom in the model
#' @param n_experimental number of participants in the experimental group
#' @param n_control number of participants in the control group
#' @return Hedge's d for continuous and categorical predictors in GLM and multiple
#'  linear regression models
#' @export

multiple_d <- function(model,
                       t,
                       df,
                       n_experimental,
                       n_control
){
  d <- round((t*(n_experimental-n_control)/(sqrt(n_experimental*n_control))
        *sqrt(df)),5)
  cat(model$data.name, "Effect Size, d = ", d)
}
