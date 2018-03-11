#' @title Wizard for the selection of an adequate effect size
#'
#' @description <This calculation wizard guides the user through the selection of the adequate effect size estimate.
#' The adequate efect size is selected based on several questions asked via dialog boxes. Subsequently, the
#' selected effect size function included in the package is prompted and/or the adequate function recommended.
#' The functions for calculating the effect sizes contain themselves a wizard that inquires the necessary data to
#' calculate the effect size.
#' @export
#'
run_wizard <- function(

){library(utils)

question1 <- winDialog("yesno", "This package will provide you with an appropriate effect size for your statistical analysis. You will just have to answer a few questions. If you agree, please press YES, or press NO otherwise.")
if (question1 == 'YES'){question2 <- winDialog("yesno", "Do you want to calculate an effect size estimate for a test comparing the means of two groups?")
} else if (question1 == 'NO'){winDialog("ok", "If you wish to directly use the functions provided in this package, feel free to do so. Also, see the according help page. If you have any questions, do not hesitate to contact the authors <chris.sigrist@bluewin.ch>.")
  rm(list=ls())
  question2 <- 0
  question3 <- 0
  question4 <- 0
  question5 <- 0
  question6 <- 0
  question7 <- 0
  question8 <- 0
  question9 <- 0
}
if (question2 == 'YES'){question3 <- winDialog("yesno", "Is your total sample size smaller than 20?")
} else if (question2 == 'NO'){question4 <- winDialog("yesno", "Do you want to calculate an effect size estimate for a Wilcoxon rank-sum or signed-rank test?")
question3 <- 2
}
if (question3 == 'YES'){winDialog("ok", "We recommend to calculate the bias-corrected effect size estimate Cohen's d to avoid upward bias due to a very small sample size. The calculation will be run automatically, however, you may also use the function cohens_d().")
  rm(list=ls())
  question3 <- 0
  question4 <- 0
  question5 <- 0
  question6 <- 0
  question7 <- 0
  question8 <- 0
  question9 <- 0
 run_cohens_d()
} else if (question3 == 'NO'){winDialog("ok", "We recommend to calculate the effect size estimate Hedge's g for your group comparison. The calculation will be run automatically, however, you may also use the function hedges_g().")
  rm(list=ls())
  question3 <- 0
  question4 <- 0
  question5 <- 0
  question6 <- 0
  question7 <- 0
  question8 <- 0
  question9 <- 0
  run_hedges_g()
}else if (question3 == 2){
}
if (question4 == 'YES'){winDialog("ok", "We recommed to calculate the effect size estimate r as a conversion of the z-value which resulted from the Wilcoxon test. The according function is wilcox_r().")
  rm(list=ls())
  question4 <- 0
  question5 <- 0
  question6 <- 0
  question7 <- 0
  question8 <- 0
  question9 <- 0
} else if (question4 == 'NO'){question5 <- winDialog("yesno", "Do you want to calculate an effect size estimate for the parameters of a GLM (e.g. ANOVA, ANCOVA, linear regression) or a multiple regression model?")
}
if (question5 == 'YES'){question6 <- winDialog("yesno", "Does your model contain only continuous predictors?")
} else if (question5 == 'NO'){question7 <- winDialog("yesno", "Do you want to calculate an effect size estimate for the predictors in a mixed-effects model?")
question6 <- 2
}
if (question6 == 'YES'){winDialog("ok", "We recommend to calculate the effect size estimate r for GLM and multiple regression models. The calculation will be run automatically, however, you may also use the function multiple_r().")
  rm(list=ls())
  question6 <- 0
  question7 <- 0
  question8 <- 0
  question9 <- 0
 run_multiple_r()
} else if (question6 == 'NO'){winDialog("ok", "We recommend to calculate the effect size estimate Hedge's d as applicable for both categorical as well as continuous variables. The calculation can be run automatically, however, you may also use the function multiple_d().")
  rm(list=ls())
  question6 <- 0
  question7 <- 0
  question8 <- 0
  question9 <- 0
  run_multiple_r()
}else if (question6 ==2){
}
if (question7 == 'YES'){winDialog("ok", "On time, we can calculate an effect size estimate only for categorial predictors in your mixed-effects model.  The calculation can be run automatically, however, you may also use the function mixed_d().")
  rm(list=ls())
  question7 <- 0
  question8 <- 0
  question9 <- 0
  run_mixed_d()
} else if (question7 == 'NO'){question8 <- winDialog("yesno", "Do you want to calculate an effect size estimate for partial correlation?")
}
if (question8 == 'YES'){winDialog("ok", "We recommend to calculate the partial correlation coefficient r. The according function is partial_r().")
  rm(list=ls())
  question8 <- 0
  question9 <- 0
  run_partial_r()
} else if (question8 == 'NO'){question9 <- winDialog("yesno", "Do you want to calculate an adjusted R squared for a tested GLM or multiple regression model?")
}
if (question9 == 'YES'){winDialog("ok", "Based on your selections, your adjusted R squared will be calculated.  The calculation can be run automatically, however, you may also use the function r_squared().")
  rm(list=ls())
  question9 <- 0
  run_adjusted_r_squared()
} else if (question9 == 'NO'){winDialog("ok", "We are sorry to inform you that on time, we cannot provide an effect size estimate for your statistical analysis. Please inform us for which statistical procedure our package is currently lacking an appropriate function <chris.sigrist@bluewin.ch>.")
  rm(list=ls())
}
rm(list=ls())
}
