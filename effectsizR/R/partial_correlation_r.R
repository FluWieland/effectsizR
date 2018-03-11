#' @title Calculation of a partial correlation coefficient
#'
#' @description <Partial correlation coefficient: The partial correlation between y
#' and x1, controlling for x2, as numerical equivalent to the correlation between
#' the residuals of the regression of y on x2 and the residuals of the regression of
#'  x1 on x2. The partial coefficient for a given predictor removes the variance
#'  explained by other predictor variables from both variables, and then quantifies
#'   the remaining correlation.>
#' @param model tested general linear or multiple-regression models with continuous
#' @param r_1_2 correlation between variable 1 and variable 2
#' @param r_2_3 correlation between variable 2 and variable 3
#' @param r_1_3 correlation between variable 1 and variable 3
#' @return Partial correlation coefficient
#' @export

partial_r <- function(model,
                        r_1_2,
                        r_2_3,
                        r_1_3
){
  r <- round((r_1_2-(r_1_3*r_2_3)/(sqrt((1-r_1_3^2)*(1-r_2_3^2)))),5)
  cat(model$data.name, "Effect Size, r = ", r)
}
