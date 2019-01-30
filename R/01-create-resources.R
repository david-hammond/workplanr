create.resources <- function(n = 5){
  require(randomNames)
  resources <- data.frame(staff = randomNames(n), 
                          capacity = sample(c(50, 100, 100, 100, 100)/100, size = n, replace = T),
                          team = sample(LETTERS[1:3], size = n, replace = T),
                          contract = sample(c("FTE", "INTERN"), size = n, replace = T))
  return(resources)                       
}