#' @title Wizard assisted calculation of the effect size Hedge's g for group comparisons
#'
#' @description <ES estimate for group designs with a total sample larger than 20, comparing
#' two independent or dependent groups: Standardised mean difference calculated by
#' using the difference between the post-test means in the numerator of the equation
#'  and the standard deviation units in the denominator (pooled SDs are used to correct for possible
#'   upwards bias as especially problematic in samples smaller than 50). Standardisation
#'    permits direct comparisons across studies using the same index of effect.>
#' @param N number of total participants
#' @param n_experimental number of participants in the experimental group
#' @param n_control number of participants in the control group
#' @param mean_experimental mean of the values of the experimental group
#' @param mean_control mean of the values of the control group
#' @param sd_experimental standard deviation of the values of the experimental group
#' @param sd_control standard deviation of the values of the control group
#' @return Hedge's g for group comparisons
#' @export

run_hedges_g <- function(
){

  #Function definition to calculate Hedge's g for group designs with samples >= 50

  hedges_g <- function(N,
                       n_experimental,
                       n_control,
                       mean_experimental,
                       mean_control,
                       sd_experimental,
                       sd_control
  ){
    N = n_experimental + n_control
    sd_pooled <- sqrt((((sd_experimental)^2*(n_experimental-1))+
                         ((sd_control^2)*(n_control-1)))/
                        ((n_experimental+n_control)-2))
    g <- round((((mean_experimental-mean_control)/sd_pooled))*((N-3)/(N-2.25))*
      (sqrt((N-2)/N)),5)
  }

  #the remaining code contains the wizard to automatically inquire the necessary data
  #and calculate the effect size estimate

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

  #Prompting the user to insert the mean of the values of the experimental group. If not a number is inserted,
  #the user is asked to insert a number until he complies or cancels.

  mean_experimental <- winDialogString("Please enter the mean of the values / scores of the experimental group as a number","")
  mean_experimental <- as.numeric(mean_experimental)
  if (is.na(mean_experimental) == TRUE){winDialog("ok", "Please type in a number")
    repeat { mean_experimental <- winDialogString("Please enter the mean of the values / scores of the experimental group as a number","")
    mean_experimental <- as.numeric(mean_experimental)
    if (is.numeric(mean_experimental) == TRUE & is.na(mean_experimental)==FALSE){
      break
    }
    }
  }

  #Prompting the user to insert the mean of the values of the control group. If not a number is inserted,
  #the user is asked to insert a number until he complies or cancels.

  mean_control <- winDialogString("Please enter the mean of the values / scores of the control group as a number","")
  mean_control <- as.numeric(mean_control)
  if (is.na(mean_control) == TRUE){winDialog("ok", "Please type in a number")
    repeat { mean_control <- winDialogString("Please enter the mean of the values / scores of the experimental group as a number","")
    mean_control <- as.numeric(mean_control)
    if (is.numeric(mean_control) == TRUE & is.na(mean_control)==FALSE){
      break
    }
    }
  }

  #Prompting the user to insert the standard deviation of the values of the experimental group. If not a number is inserted,
  #the user is asked to insert a number until he complies or cancels.

  sd_experimental <- winDialogString("Please enter the standard deviation of the values / scores of the experimental group as a number","")
  sd_experimental <- as.numeric(sd_experimental)
  if (is.na(sd_experimental) == TRUE){winDialog("ok", "Please type in a number")
    repeat { sd_experimental <- winDialogString("Please enter the standard deviation of the values / scores of the experimental group as a number","")
    sd_experimental <- as.numeric(sd_experimental)
    if (is.numeric(sd_experimental) == TRUE & is.na(sd_experimental)==FALSE){
      break
    }
    }
  }

  #Prompting the user to insert the standard deviation of the values of the control group. If not a number is inserted,
  #the user is asked to insert a number until he complies or cancels.

  sd_control <- winDialogString("Please enter the standard deviation of the values / scores of the control group as a number","")
  sd_control <- as.numeric(sd_control)
  if (is.na(sd_control) == TRUE){winDialog("ok", "Please type in a number")
    repeat { sd_control <- winDialogString("Please enter the standard deviation of the values / scores of the control group as a number","")
    sd_control <- as.numeric(sd_control)
    if (is.numeric(sd_control) == TRUE & is.na(sd_control)==FALSE){
      break
    }
    }
  }

hedges_g  <- hedges_g(N,
                        n_experimental,
                        n_control,
                        mean_experimental,
                        mean_control,
                        sd_experimental,
                        sd_control)

winDialog("ok","The value for your effect size estimate will be displayed in the console.")

cat("Your Hedge's g effect size estimate is", hedges_g)
}

