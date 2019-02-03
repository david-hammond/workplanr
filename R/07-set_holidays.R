#' create a list of phases in any project
#'
#' @param file file with public holiday information
#' @return A reference table for phases 
#' @keywords internal

set_public_holidays <- function(public_holidays){
  public_holidays <- holi(date = public_holidays$date, 
                          name = public_holidays$name)
  return(public_holidays)
}
