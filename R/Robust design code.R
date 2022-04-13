# Robust design in RMark
# Anna Moeller 
# 3/30/2022

library(RMark)
library(tidyverse)

# Bring in long data
nind <- 3
nocc <- 10
longdat <- data.frame(
  indiv = rep(1:nind, each = nocc),
  occ = rep(1:nocc,nind),
  eh = rbinom(nocc*nind,1,0.5)
)

# Transform long data to wide 
widedat <- longdat %>% 
  pivot_wider( names_from = occ, values_from = eh) %>% 
  # squash columns together
  unite(col = ch, 2:11, sep = "")

# Specify closed and open periods (length nocc-1)
# Need at least 3 primary occasions 
intvl <- c(0,0,1,0,0,0,1,0,0)

# Process data for RMark
rd <- process.data(data = widedat,
                   model = "Robust",
                   time.intervals = intvl)

# Define a function to run all your models 
run.robust <- function() {

  # Define biological models - covariates on survival  
  S.dot <- list(formula = ~1) # Apparent survival same across years
  S.time <- list(formula =  ~time) # Apparent survival varies by year
  
  # Define detection models - covariates on detection probability
  p.dot <- list(formula = ~1, share = TRUE) # detection probability same always
  p.session <- list(formula =  ~session, share = TRUE)# p varies by primary session but not among secondaries within primary
  
  # emigration is 0
  GammaDoublePrime.zeroTE <- list(formula = ~ 1,
                                  fixed = 0)
  
  GammaPrime.zero <- list(formula = ~ 1,
                          fixed = 0)
  
  # Define model combinations
  mod.constant <- mark(data = rd, 
                    model.parameters = list(
                      S = S.dot,
                      p = p.dot,
                      GammaDoublePrime = GammaDoublePrime.zeroTE,
                      GammaPrime = GammaPrime.zero
                      )
                    )
  mod.ZeroTE <- mark(data = rd, 
                     model.parameters = list(
                       S = S.time,
                       p = p.session,
                       GammaDoublePrime = GammaDoublePrime.zeroTE,
                       GammaPrime = GammaPrime.zero
                     )
                  )
  
  
  # Return model table and list of models
  return(collect.models())
}

robust.results <- run.robust()

# Compare the two
robust.results$mod.constant$results$real # or $beta
robust.results$modZeroTE$results$real

