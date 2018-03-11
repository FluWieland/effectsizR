#' @title Wizard assisted calculation of a partial correlation coefficient
#'
#' @description <Partial correlation coefficient: The partial correlation between y
#' and x1, controlling for x2, as numerical equivalent to the correlation between
#' the residuals of the regression of y on x2 and the residuals of the regression of
#'  x1 on x2. The partial coefficient for a given predictor removes the variance
#'  explained by other predictor variables from both variables, and then quantifies
#'   the remaining correlation.>
#' @param r_1_2 correlation between variable 1 and variable 2
#' @param r_2_3 correlation between variable 2 and variable 3
#' @param r_1_3 correlation between variable 1 and variable 3
#' @return Partial correlation coefficient
#' @export

run_partial_r <- function(
){

  #function definition for calculating the partial correlation coefficient r

  partial_r <- function(r_1_2,
                        r_2_3,
                        r_1_3
  ){
    r <- round((r_1_2-(r_1_3*r_2_3)/(sqrt((1-r_1_3^2)*(1-r_2_3^2)))),5)
  }

  #the remaining code contains the wizard to automatically inquire the necessary data
  #and calculate the effect size estimate

  #Prompting the user to insert the correlation between variables 1 and 2. If not a number is inserted,
  #the user is asked to insert a number until he complies or cancels.

  r_1_2 <- winDialogString("Please enter the correlation between variables 1 and 2 as a number","")
  r_1_2 <- as.numeric(r_1_2)
  if (is.na(r_1_2) == TRUE){winDialog("ok", "Please type in a number")
    repeat { r_1_2 <- winDialogString("Type in the correlation between variables 1 and 2 as a number","")
    r_1_2 <- as.numeric(r_1_2)
    if (is.numeric(r_1_2) == TRUE & is.na(r_1_2)==FALSE){
      break
    }
    }
  }

  #Prompting the user to insert the correlation between variables 1 and 3. If not a number is inserted,
  #the user is asked to insert a number until he complies or cancels.

  r_1_3 <- winDialogString("Please enter the correlation between variables 1 and 3 as a number","")
  r_1_3 <- as.numeric(r_1_3)
  if (is.na(r_1_3) == TRUE){winDialog("ok", "Please type in a number")
    repeat { r_1_3 <- winDialogString("Type in the correlation between variables 1 and 3 as a number","")
    r_1_3 <- as.numeric(r_1_3)
    if (is.numeric(r_1_3) == TRUE & is.na(r_1_3)==FALSE){
      break
    }
    }
  }

  #Prompting the user to insert the correlation between variables 2 and 3. If not a number is inserted,
  #the user is asked to insert a number until he complies or cancels.

  r_2_3 <- winDialogString("Please enter the correlation between variables 2 and 3 as a number","")
  r_2_3 <- as.numeric(r_2_3)
  if (is.na(r_2_3) == TRUE){winDialog("ok", "Please type in a number")
    repeat { r_2_3 <- winDialogString("Type in the correlation between variables 2 and 3 as a number","")
    r_2_3 <- as.numeric(r_2_3)
    if (is.numeric(r_2_3) == TRUE & is.na(r_2_3)==FALSE){
      break
    }
    }
  }

  partial_r <-partial_r(r_1_2,
                        r_2_3,
                        r_1_3)

  winDialog("ok","The value for your effect size estimate result will be displayed in the console.")

  cat("Your correlation coefficient estimate is", partial_r)

}
