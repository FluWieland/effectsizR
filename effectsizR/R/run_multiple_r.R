#' @title Wizard assisted calculation of the effect size r for continuous predictors in GLM and multiple regression models
#'
#' @description <ES for continuous predictors in general linear and multiple
#' regression models, calculated using t values, which are generally obtained from
#'  a difference between estimates (e.g. means or slopes) divided by the standard
#'  error of the differences - almost all statistical software provides t values
#'  when a statistical model is constructed. The t values obtained for a continuous
#'   predictor variable can be used for calculating r.>
#' @param t t-value of the continuous predictor d is to be calculated for
#' @param df degrees of freedom in the model
#' @return r statistics for continuous predictors in GLM and multiple regression models
#' @export

run_multiple_r <- function(
){

  #Function definition to calculate r for continuous predictors in GLMs and multiple regeression models

multiple_r <- function(t,
                       df
  ){
    round(r <- sqrt((t^2)/(t^2+df)),5)
  }

  #Prompting the user to insert the t-value of the respective parameter. If not a number is inserted,
  #the user is asked to insert a number until he complies or cancels.

t <- winDialogString("Please enter the t-value of the respective parameter as a number","")
t <- as.numeric(t)
  if (is.na(t) == TRUE){winDialog("ok", "Please type in a number")
    repeat { t <- winDialogString("Type in the t-value of the respective parameter as a number","")
    t <- as.numeric(t)
    if (is.numeric(t) == TRUE & is.na(t)==FALSE){
      break
    }
    }
}

  #Prompting the user to insert the degrees of freedom in the model. If not a number is inserted,
  #the user is asked to insert a number until he complies or cancels.

  df <- winDialogString("Please enter the degrees of freedom in the model as a number","")
  df <- as.numeric(df)
  if (is.na(df) == TRUE){winDialog("ok", "Please type in a number")
    repeat { df <- winDialogString("Type in the degrees of freedom in the model as a number","")
    df <- as.numeric(df)
    if (is.numeric(df) == TRUE & is.na(df)==FALSE){
      break
    }
    }
  }
  multiple_r  <- multiple_r(t,
                            df)


  winDialog("ok","The value for your effect size estimate result will be displayed in the environment.")

  cat("Your effect size estimate r is", multiple_r)

}


