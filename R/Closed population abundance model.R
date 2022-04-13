# Closed population model 

# http://www.phidot.org/software/mark/docs/book/pdf/chap14.pdf
# https://www.montana.edu/rotella/documents/502/lab07RMark.html

library(RMark)
library(tidyverse)

# Bring in long data (or simulate in this case)
nind <- 10
nocc <- 10
longdat <- data.frame(
  indiv = rep(1:nind, each = nocc),
  occ = rep(1:nocc,nind),
  eh = rbinom(nocc*nind,1,0.5)
)

# Transform long data to wide 
caps <- longdat %>% 
  pivot_wider( names_from = occ, values_from = eh) %>% 
  # squash columns together
  unite(col = ch, 2:11, sep = "") 

# Process data
caps.pr <- process.data(caps, begin.time = 1, model = "Closed")

# Create design data
caps.ddl <- make.design.data(caps.pr)

# Function to run models 
run.caps <- function() {

  # Define parameters for p and c 
  # Note: "share=TRUE" indicates that 'p' & 'c'
  #  share the same columns in the design matrix
  p.dot <- list(formula =  ~ 1, share = TRUE)
  p.time <- list(formula =  ~ time, share = TRUE)
  
  # Create competing models based on structures for 'p' & 'c'
  caps.model.list <- create.model.list("Closed")
  
  # NOTE: if you do not want to see the output for each model, add  
  # ', output=FALSE' after 'ddl=caps.ddl' below. 
  caps.results <- mark.wrapper(caps.model.list,
                              data = caps.pr, 
                              ddl = caps.ddl)
  
  # Return model table and list of models
  return(caps.results)
}

caps.results <- run.caps()
caps.results

caps.results$p.time$results$real
