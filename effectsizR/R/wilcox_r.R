#' @title Calculation of the effect size r after group comparisons with Wilcoxon rank-sum/signed-rank test
#'
#' @description <Effect size for group designs comparing two independent or dependent groups concerning their
#' medians, calculated by converting z-values into an effect size estimate r.
#'#' @param wilcoxModel a list of values as returned by Wilcoxon test
#' @param N number of total subjects tested
#' @return Effect size estimate r calculated by converting z-values
#' @export

wilcox_r <- function(wilcoxModel, N){
  z <- qnorm(wilcoxModel$p.value/2)
  r <- round(z/sqrt(N),5)
  cat(wilcoxModel$data.name, "Effect Size, r = ", r)
}
