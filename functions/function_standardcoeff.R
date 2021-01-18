#Function for standardized coefficients for all continuous variables, but not binary variables.
#adapted from Ben Bolker (https://stackoverflow.com/questions/25142901/standardized-coefficients-for-lmer-model)

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  
  binary <- apply(getME(object, "X"), 2, FUN = function(x) length(unique(x))) == 2
  sdx <- numeric(length(binary))
  
  for (i in seq(1:length(binary))){
    if(binary[i] == FALSE){
      sdx[i] <- apply(getME(object, "X"), 2, sd)[i]}
    else{
      sdx[i] <- 1}
  }
  
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se, treatedAsBinary=binary))
} 
