#' create a list of phases in any project
#'
#' @param file file with public holiday information
#' @return A reference table for phases 
#' @keywords internal

set_public_holidays <- function(date, name){
  public_holidays <- holi(date = date, 
                          name = name)
  return(public_holidays)
}
