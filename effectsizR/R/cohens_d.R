#' @title Calculation of the effect size Cohen's d for group comparisons with small samples
#'
#' @description <Effect size for group designs including very small samples (N < 20), comparing two independent
#'  or dependent groups: Standardized mean difference calculated using the difference between post-test means in the
#'   numerator of the equation and the standard deviation units in the denominator (pooled SDs are used to correct
#'   for possible upwards bias as problematic in very small samples). Standardisation permits direct comparisons across
#'   studies using the same index of effect.>
#' @param data a list of values as returned by group comparison procedures (e.g. t-test)
#' @param N number of total subjects tested
#' @param mean_experimental mean of the values observed in the tested experimental group
#' @param mean_control mean of the values observed in the tested control group
#' @param sd_experimental standard deviation of the values observed in the tested experimental group
#' @param sd_control standard deviation of the values observed in the tested control group
#' @return Cohens' d after group comparisons
#' @export

cohens_d <- function(data,
                     N,
                     mean_experimental,
                     mean_control,
                     sd_experimental,
                     sd_control
){
  sd_pooled <- sqrt(((sd_experimental^2)+(sd_control^2))/2)
  d <- round(((mean_experimental-mean_control)/(sd_pooled))*((N-3)/(N-2.25))*(sqrt((N-2)/N)),5)
  cat(data$data.name, "Effect Size, d = ", d)
}

