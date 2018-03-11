#' @title Wizard assisted calculation of adjusted R squared for univariate models
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
#' @param adjusted_r_squared variance of data which is accounted for by tested model
#' @param k_predictors number of predictors in the model (not including the intercept)
#' @return adjusted R squared for univariate models
#' @export

run_adjusted_r_squared <- function(
){

  #Function definition to calculate the adjusted R squareddw

  adjusted_r_squared <- function(N,
                                 r_squared,
                                 k_parameters
  ){
    r <- round(sqrt((1)-((N-1)*(1-r_squared))/(N-k_parameters-1)),5)
  }

  #the remaining code contains the wizard to automatically inquire the necessary data
  #and calculate the effect size estimate

  #the remaining code contains the wizard to automatically inquire the necessary data
  #and calculate the effect size estimate

  N <- winDialogString("Please enter the number of participants in both groups together as a number","")
  N <- as.numeric(N)
  if (is.na(N) == TRUE){winDialog("ok", "Please type in a number")
    repeat { N <- winDialogString("Type in the length of your N as a number","")
    N <- as.numeric(N)
    if (is.numeric(N) == TRUE & is.na(N)==FALSE){
      break
    }
    }
  }

  #Prompting the user to insert the r squared of the model as a number. If not a number is inserted,
  #the user is asked to insert a number until he complies or cancels.

  r_squared <- winDialogString("Please enter the r squared of the model as a number","")
  r_squared <- as.numeric(r_squared)
  if (is.na(r_squared) == TRUE){winDialog("ok", "please type in a number")
    repeat { r_squared <- winDialogString("type in the r squared of the model as a number","")
    r_squared <- as.numeric(r_squared)
    if (is.numeric(r_squared) == TRUE & is.na(r_squared)==FALSE){
      break
    }
    }
  }

  #Prompting the user to insert the number of parameters (without the intercept). If not a number is inserted,
  #the user is asked to insert a number until he complies or cancels.

  k_parameters <- winDialogString("Please enter the number of parameters (without the intercept) as a number","")
  k_parameters <- as.numeric(k_parameters)
  if (is.na(k_parameters) == TRUE){winDialog("ok", "Please type in a number")
    repeat { k_parameters <- winDialogString("Type in the number of parameters (without the intercept) as a number","")
    k_parameters <- as.numeric(k_parameters)
    if (is.numeric(k_parameters) == TRUE & is.na(k_parameters)==FALSE){
      break
    }
    }
  }

  adjusted_r_squared <- adjusted_r_squared(N,
                                           r_squared,
                                           k_parameters)

  winDialog("ok","The value for your effect size estimate result will be displayed in the environment.")

  cat("Your adjusted R squared estimate is", adjusted_r_squared)

}

