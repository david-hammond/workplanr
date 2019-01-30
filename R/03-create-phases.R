create.phases <- function(){
  
  phases <- data.frame(phase = c("research", "drafting", "editing", "design", "print", "launch-events"), 
                       sequencing = 1:6) %>% mutate(phase = factor(phase, phase[sequencing], ordered = T))
  return(phases)                       
}