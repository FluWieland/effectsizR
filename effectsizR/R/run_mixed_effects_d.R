#' @title Wizard assisted calculation of the effect size Hedge's d for categorical predictors in mixed effects models
#'
#' @description <ES for mixed-effects models including only predictors which are categorical:
#'  For mixed-effects models as used if data is hierarchical and/or includes repeated-measures,
#'   t-values can be used to approximate d statistics.>
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

run_mixed_d <- function(
){

  #Function definition to calculate Hedge's d for categorial predictors in mixed-effects models

  mixed_d <- function(t,
                      n_observations_exp,
                      n_observations_ctrl,
                      n_experimental,
                      n_control,
                      k_parameters,
                      R,
                      sB,
                      sE
  ){
    R <- (sB^2)/(sB^2+sE^2)
    d <- round((t*(1+(n_experimental/n_control)*R)*(sqrt(1-R)*(n_observations_exp+
                                                           n_observations_ctrl))/(sqrt(n_observations_exp*n_observations_ctrl)*sqrt(n_control-k_parameters))),5)
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

  #Prompting the user to insert the number of observations in the experimental group. If not a number is inserted,
  #the user is asked to insert a number until he complies or cancels.

  n_observations_exp <- winDialogString("Please enter the number of observations (e.g. repeated-measures times participants) in the experimental group as a number","")
  n_observations_exp <- as.numeric(n_observations_exp)
  if (is.na(n_observations_exp) == TRUE){winDialog("ok", "Please type in a number")
    repeat { n_observations_exp <- winDialogString("Type in the number of observations in the experimental group as a number","")
    n_observations_exp <- as.numeric(n_observations_exp)
    if (is.numeric(n_observations_exp) == TRUE & is.na(n_observations_exp)==FALSE){
      break
    }
    }
  }

  #Prompting the user to insert the number of observations in the control group. If not a number is inserted,
  #the user is asked to insert a number until he complies or cancels.

  n_observations_ctrl <- winDialogString("Please enter the number of observations (e.g. repeated-measures times participants) in the control group","")
  n_observations_ctrl <- as.numeric(n_observations_ctrl)
  if (is.na(n_observations_ctrl) == TRUE){winDialog("ok", "Please type in a number")
    repeat { n_observations_ctrl <- winDialogString("Type in the number of observations in the control group as a number","")
    n_observations_ctrl <- as.numeric(n_observations_ctrl)
    if (is.numeric(n_observations_ctrl) == TRUE & is.na(n_observations_ctrl)==FALSE){
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

  #Prompting the user to insert the number of parameters (including the intercept). If not a number is inserted,
  #the user is asked to insert a number until he complies or cancels.

  k_parameters <- winDialogString("Please enter the number of parameters (including the intercept) as a number","")
  k_parameters <- as.numeric(k_parameters)
  if (is.na(k_parameters) == TRUE){winDialog("ok", "Please type in a number")
    repeat { k_parameters <- winDialogString("Type in the number of parameters (including the intercept) as a number","")
    k_parameters <- as.numeric(k_parameters)
    if (is.numeric(k_parameters) == TRUE & is.na(k_parameters)==FALSE){
      break
    }
    }
  }

  #Prompting the user to insert the between subject's or between group variance. If not a number is inserted,
  #the user is asked to insert a number until he complies or cancels.

  sB <- winDialogString("Please enter the betweeb subject's or between group variance of the respective parameter as a number","")
  sB <- as.numeric(sB)
  if (is.na(sB) == TRUE){winDialog("ok", "Please type in a number")
    repeat { sB <- winDialogString("Type in the between subject's or between group variance of the respective parameter as a number","")
    sB <- as.numeric(sB)
    if (is.numeric(sB) == TRUE & is.na(sB)==FALSE){
      break
    }
    }
  }

  #Prompting the user to insert the within subject's or within group variance. If not a number is inserted,
  #the user is asked to insert a number until he complies or cancels.

  sE <- winDialogString("Please enter the within subject's or within group variance of the respective parameter as a number","")
  sE <- as.numeric(sE)
  if (is.na(sE) == TRUE){winDialog("ok", "Please type in a number")
    repeat { sE <- winDialogString("Type in the within subject's or within group variance of the respective parameter as a number","")
    sE <- as.numeric(sE)
    if (is.numeric(sE) == TRUE & is.na(sE)==FALSE){
      break
    }
    }
  }

  mixed_d <- mixed_d(t,
                     n_observations_exp,
                     n_observations_ctrl,
                     n_experimental,
                     n_control,
                     k_parameters,
                     R,
                     sB,
                     sE)

  winDialog("ok","The value for your effect size estimate result will be displayed in the console.")

  cat("Your Hedge's d effect size estimate is", mixed_d)

}
