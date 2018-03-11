#' @title Calculation of the effect size Hedge's d for categorical predictors in mixed effects models
#'
#' @description <ES for mixed-effects models including only predictors which are categorical:
#'  For mixed-effects models as used if data is hierarchical and/or includes repeated-measures,
#'   t-values can be used to approximate d statistics.>
#' @param cat_mixed_effects_model tested mixed effects model including only categorial predictors
#' @param t t-value of the predictor d is to be calculated for
#' @param n_observations_exp number of observations in the experimental group
#' @param n_observations_ctrl number of observations in the control group
#' @param n_experimental_group number of subjects in the experimental group
#' @param n_control_group number of subjects in the control group
#' @param k_parameters number of parameters (including the intercept)
#' @param sB  between subject's or between group variance
#' @param sE  within individual or group variance
#' @return hedge's d for categorical predictors in mixed effects models
#' @export

mixed_d <- function(catecorical_mixed_effects_model,
                            t,
                            n_observations_exp,
                            n_observations_ctrl,
                            n_experimental_group,
                            n_control_group,
                            k_parameters,
                            sB,
                            sE
){
  R <- (sB^2/(sB^2+sE^2))
  d <- round((t*(1+(n_experimental_group/n_control_group)*R)*(sqrt(1-R)*(n_observations_exp+
  n_observations_ctrl))/(sqrt(n_observations_exp*n_observations_ctrl)*
  sqrt(n_control_group-k_parameters))),5)
  cat(catecorical_mixed_effects_model$data.name, "Effect Size, d = ", d)
}
