#######################################################################################################
## Stefan Gehrig, 2019-04-30
## This function performs a permutation test / randomisation test
## see for example:
##
## Ernst, M. D. (2004). Permutation methods: a basis for exact inference. 
## Statistical Science, 19(4), 676-685.
##
## It provides a two-sided p-value for the difference in means between two groups, 
## as well as a histogram that visualizes the permutation distribution vs. observed value. 
## If there are more than two groups, the function  will select the 1st and 2nd level only. Only complete cases are used.
## Further, it calculates 95%-CIs following:
##
## Garthwaite, P. H. (1996). Confidence intervals from randomization tests. 
## Biometrics, 1387-1393.
##
## For the 95%-CIs, diagnostic plots for the search processes are displayed.
#######################################################################################################

perm_test <- function(outcome,       # name of numerical variable that encodes the outcome which is compared 
                      group,         # name of numerical, factor, or categorical variable that encodes the two groups
                      data,          # dataframe containing variables
                      conf = TRUE,   # whether 95%CIs for the difference in means should be computed
                      perms = 5000){ # number of permutations (default: 5,000)
  
  # only working with data frames, not tibbles
  data <- data.frame(data)
  
  # remove rows with NA
  data <- data[!is.na(data[,outcome]) & !is.na(data[,group]),]
  
  # create shortcuts
  out  <- as.numeric(as.character(data[,outcome]))
  gro  <- as.factor(data[,group])
  gro1 <- levels(gro)[1]
  gro2 <- levels(gro)[2]
  
  # calculate observed empirical differnece
  emp.diff  <- mean(out[gro==gro1])-mean(out[gro==gro2])
  
  # simulate differences via permutation
  sim.diff  <- rep(NA, perms)
  for(i in seq(1:perms)){                            
    shuffled_gro       <- sample(gro, nrow(data), replace=F)
    sim.diff[i]        <- mean(out[shuffled_gro==gro1])-mean(out[shuffled_gro==gro2])
  }
  
  if (conf == FALSE){
  
  # plot distribution and observed value
  # hist(sim.diff, main = "Histogram of simulated differences", xlab = "Simulated mean diff.",
  #      xlim=c(min(c(emp.diff,sim.diff)), max(c(emp.diff,sim.diff))))
  # abline(v=emp.diff, lwd = 2, col = "red")
  # text(emp.diff, 0, "observed mean diff.", col = "red")
  
  # calculate two-sided p-value
  p <- sum(abs(sim.diff) >= abs(emp.diff))/perms
  
  # return(cat(paste("Mean difference (", gro1, " - ", gro2, ") = ", round(emp.diff,4), "\n",
  #              "P-value (", perms, " permutations) = ", round(p,4), sep="")))
  
  return(round(p,4))
  
  }
  
  if (conf == TRUE){
    
    # plot distribution and observed value
    par(mfrow=c(1,3))
    hist(sim.diff, main = "Histogram of\n simulated differences", xlab = "Simulated mean diff.",
         xlim=c(min(c(emp.diff,sim.diff)), max(c(emp.diff,sim.diff))))
    abline(v=emp.diff, lwd = 2, col = "red")
    text(emp.diff, 0, "Observed mean diff.", col = "red")
    
    # calculate two-sided p-value
    p <- sum(abs(sim.diff) >= abs(emp.diff))/perms
    
    # calcultate 95%CIs (for details on procedure, see reference cited in header)
    # define parameters
    z <- 1.96
    k <- 2/(z * (2*pi)^(-1/2) * exp((-z^2)/2))
    m <- round(0.3*(2-0.025)/(0.025),0)
    n <- perms * 2
    
    # created shifted group mean such that means are equal
    shifted_mean <- emp.diff + mean(out[gro==gro2])
    
    # define starting points for upper and lower search for CI endpoints
    sim.diff<-NULL
    shuffled_gro <- NULL
    for (i in seq(1,(2-0.025)/0.025,1)){
      shuffled_gro       <- sample(gro, nrow(data), replace=F)
      sim.diff[i]        <- mean(out[shuffled_gro==gro1])-shifted_mean}
      
    startsearch.lower <- emp.diff+(min(sim.diff[sim.diff!=min(sim.diff)]) - 
                           max(sim.diff[sim.diff!=max(sim.diff)]))/2
    startsearch.upper <- emp.diff-(min(sim.diff[sim.diff!=min(sim.diff)]) - 
                           max(sim.diff[sim.diff!=max(sim.diff)]))/2 
      
    # initiate vectors for search results
    U <- NULL
    L <- NULL
    
    # search lower CI
    L[1] <- startsearch.lower
    for(i in seq(1,n-1,1)){
      
      emp_delta    <- L[i] + out[gro==gro1]
      emp_selmean  <- mean(sample(c(emp_delta, out[gro==gro2]),
                                  length(out[gro==gro1]), replace=F))
      
      c <- k*(emp.diff - L[i])
      
      if (emp_selmean < mean(out[gro==gro1])){
        
        L[i+1] <- L[i] + c*0.025/(i+m-1)}
      
      else{
        L[i+1] <- L[i] - c*(1-0.025)/(i+m-1)}
    }
    
    # save final value and plot convergence diagnostic
    result.lower<-round(tail(L,1),4)
    plot(L, type="l", ylab="95%-CI limit", xlab = "Search step", main = "Search diagnostics of\n lower limit")
    abline(h = tail(L,1))
      
    # search upper CI
    U[1] <- startsearch.upper
    for(i in seq(1,n-1,1)){
      
      emp_delta    <- U[i] + out[gro==gro1]
      emp_selmean  <- mean(sample(c(emp_delta, out[gro==gro2]),
                                  length(out[gro==gro1]), replace=F))
      
      c <- k*(U[i] - emp.diff)
      
      if (emp_selmean > mean(out[gro==gro1])){
        
        U[i+1] <- U[i] - c*0.025/(i+m-1)}
      
      else{
        
        U[i+1] <- U[i] + c*(1-0.025)/(i+m-1)}
    }
    
    # save final value and plot convergence diagnostic
    result.upper <- round(tail(U,1),4)
    plot(U, type="l", ylab="95%-CI limit", xlab = "Search step", main = "Search diagnostics of\n upper limit")
    abline(h = tail(U,1))
    
    return(cat(paste("Mean difference (", gro1, " - ", gro2, ") = ", round(emp.diff,4), "\n",
                     "P-value (", perms, " permutations) = ", round(p,4), "\n",
                     "95%CI: ", result.lower, "; ", result.upper,
                     sep="")))
    }
  }

#### EXAMPLE ####
# set.seed(132)
# treatment <- sample(c("control","intervention"), 500, replace = T)
# endpoint <- round(rnorm(mean = 5, sd = 2, n = 500),2)
# endpoint[treatment=="intervention"] <- abs(endpoint[treatment=="intervention"]+0.3)
# df1       <- data.frame(cbind(treatment, endpoint))
# perm_test("endpoint", "treatment", df1, conf = FALSE, perms = 3500)
# perm_test("endpoint", "treatment", df1)
# dev.off()
