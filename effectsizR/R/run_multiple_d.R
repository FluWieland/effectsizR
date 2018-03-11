#' @title Wizard assisted calculation of the effect size Hedges's d for continuous and categorical predictors in GLM and multiple linear regression models
#'
#' @description <ES for continuous and categorical predictors in general linear and
#' multiple regression models, calculated using t-values (obtained from a difference between estimates,
#' e.g. means or slopes, divided by the standard error of the differences - almost all statistical software
#' provides t values when a statistical model is constructed). The t-values obtained for groups or
#' categories in a predictor variable can be used for calculating d.>
#' @param t t-value of the respective predictor
#' @param df degrees of freedom in the model
#' @param n_experimental number of participants in the experimental group
#' @param n_control number of participants in the control group
#' @return Hedge's d for continuous and categorical predictors in GLM and multiple
#'  linear regression models
#' @export

run_multiple_d <- function(
){

  #Function definition to calculate Hedge's d for GLMs and multiple regression models

  multiple_d <- function(t,
                         df,
                         n_experimental,
                         n_control
  ){
    d <- round((t*(n_experimental-n_control)/(sqrt(n_experimental*n_control))*
            sqrt(df)),5)
  }

  #the remaining code contains the wizard to automatically inquire the necessary data
  #and calculate the effect size estimate

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

  #Prompting the user to insert the number of participants in the experimental group. If not a number is inserted,
  #the user is asked to insert a number until he complies or cancels.

  n_experimental <- winDialogString("Please enter the number of participants in the experimental group as a number","")
  n_experimental <- as.numeric(n_experimental)
  if (is.na(n_experimental) == TRUE){winDialog("ok", "Please type in a number")
    repeat { n_experimental <- winDialogString("Type in the number of participants in the experimental group as a number","")
    n_experimental <- as.numeric(n_experimental)
    if (is.numeric(n_experimental) == TRUE & is.na(n_experimental)==FALSE){
      break
    }
    }
  }

  #Prompting the user to insert the number of participants in the experimental group. If not a number is inserted,
  #the user is asked to insert a number until he complies or cancels.

  n_control <- winDialogString("Please enter the number of participants in the control group as a number","")
  n_control <- as.numeric(n_control)
  if (is.na(n_control) == TRUE){winDialog("ok", "Please type in a number")
    repeat { n_control <- winDialogString("Type in the number of participants in the control group as a number","")
    n_control <- as.numeric(n_control)
    if (is.numeric(n_control) == TRUE & is.na(n_control)==FALSE){
      break
    }
    }
  }

  multiple_d  <- multiple_d(t,
                            df,
                            n_experimental,
                            n_control)

  winDialog("ok","The value for your effect size estimate result will be displayed in the console.")

  cat("Your Hedge's d effect size estimate is", multiple_d)

}
