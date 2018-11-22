# Student ID = 180028774

# I confirm that the following report and associated code is my own work,
# except where clearly indicated.

library("dslabs")
library("tidyverse")
library('ggthemes')

## Research question:
# Are the average ratings for 21st century animated movies different from Sci-Fi
# movies of the same period?


# Initialisation ----------------------------------------------------------


Movies <- data.frame(movielens) %>%  filter(., year > 2000) 

Animated <- Movies %>% filter(., grepl("Animation", genres)) %>% 
  filter(., year > 2000) %>% select(., rating)

Sci.Fi <- Movies %>% filter(., grepl("Sci-Fi", genres))  %>% 
  filter(., year > 2000) %>% select(., rating)


# Purpose - To give a table of the values from original data
# Input - None (But "Movies", "Animated", and "Sci-Fi" above must have been called)
# Output - A table of mean and sd for the specifies generes

Initialise <- function() { 
# Make the table to be filled with ata
Summary.Table <- matrix(NaN, nrow = 2, ncol = 2)

# Calculate mean and sd for Sci-Fi, then put into the table
Sci.Fi.mean <- mean(Sci.Fi$rating)
Summary.Table[1,1] <- Sci.Fi.mean
Sci.Fi.sd <- sd(Sci.Fi$rating)
Summary.Table[2,1] <- Sci.Fi.sd

# Calculate mean and sd for Animated movies, then put into the table
Animated.mean <- mean(Animated$rating)
Summary.Table[1,2] <- Animated.mean
Animated.sd <- sd(Animated$rating)
Summary.Table[2,2] <- Animated.sd

# Assign names to table
colnames(Summary.Table) <- c('Sci-Fi', 'Animated')
rownames(Summary.Table) <- c('Mean', 'Sd')

# Showcase the distribution of the original data
par(mfrow = c(1,2))
hist(Animated$rating, main = "Ratings of Animated movies of 21th centry", 
     xlab = "Rating", col = "slateblue4")
hist(Sci.Fi$rating, main = "Ratings of Sci-Fi movies of 21th centry", 
     xlab = "Rating", col = "grey")
par(mfrow = c(1,1))

return(Summary.Table)
}

Initialise()



# Monte Carlo -------------------------------------------------------------

# Purpose - To generate Monte Carlo simulations for the given parameters and 
# calculate the Size/Power of said parameters using both parametric and 
# non-parametric tests

# Input - Sample size (n),
# number of simulations (nSim), choosen level of alpha (alpha), and the two 
# means and sds

# Output - a table of Size/Power according to the above specifications - 
# given for both paramteric and non-parametric tests 

General.MC <- function(Distribution = 'rnorm', n, nSim = 1000, alpha, MC.Mean1, 
                       MC.Sd1, MC.Mean2, MC.Sd2){
  # Initial input check of arguments, print error message if erroneours
  
  if ( n < 0  | !is.numeric(n) | length(n) > 1  | nSim < 0  | !is.numeric(nSim) 
       | length(nSim) > 1 | !is.numeric(alpha) | length(alpha) > 1
       | alpha < 0 | !is.numeric(MC.Mean1) | length(MC.Mean1) > 1 | MC.Mean1 < 0  
       | !is.numeric(MC.Sd1) | length(MC.Sd1) > 1 | MC.Sd1 < 0  
       | !is.numeric(MC.Mean2) | length(MC.Mean2) > 1 | MC.Mean2 < 0  
       | !is.numeric(MC.Sd2) | length(MC.Sd2) > 1 | MC.Sd2 < 0 | n == Inf 
       | nSim == Inf | alpha == Inf| MC.Mean1 == Inf
       | MC.Sd1 == Inf| MC.Mean2 == Inf| MC.Sd2 == Inf) 
  {
    stop("Invalid arguments - Note: SampleSize and 
         Effect is binary - only one can be 1" )
  }
  
  # Construct matricies to be filled with p-values, the two columns are to seperate
  # significant (column 1) from non-significant (column 2) p-values
  par.matrix <- matrix(NaN,nSim, ncol = 2)
  non.par.matrix <- matrix(NaN,nSim, ncol = 2)
  
    # From 1 to the number of simulations asked for - simulate data sets, 
    # extract and store the p-values from both tests
    for(i in 1:nSim){
      
      if(Distribution == 'rnorm'){ 
      # Generate a new dataset for each simulation
      A.Sim <- rnorm(n = n, mean = MC.Mean1, sd = MC.Sd1)
      
      S.Sim <- rnorm(n = n, mean = MC.Mean2, sd = MC.Sd2)
      }
      else if(Distribution == 'rchisq') { 
        # Generate a new dataset for each simulation
        A.Sim <- rchisq(n = n, df = MC.Mean1)
      
        S.Sim <- rchisq(n = n, df = MC.Mean2)
      }
      # Perform a parametric test on the simulated dataset
      Parametric <- t.test(A.Sim, S.Sim)
      # Depending on if the test is significant, or not, extract the p-value and 
      # store it appropriatly in  one of the matrices above as specified above
      if (Parametric$p.value <= alpha) {
        par.matrix[i,1] <- Parametric$p.value
      }
      else {
        par.matrix[i,2] <- Parametric$p.value
      }
      
      # Perform a non-parametric test (Mann-Whitney U test) and perform just as 
      # described for the parametric test 
      NonParametric <-  wilcox.test(A.Sim, S.Sim)
      if (NonParametric$p.value <= alpha) {
        non.par.matrix[i,1] <- NonParametric$p.value
      }
      else {
        non.par.matrix[i,2] <- NonParametric$p.value
      }
      
    }
  
  # Once the nSim number of tests have been performed and their p-values stored 
  # according to above - calculate the size/power of these tests. 
  # Extract the number of significant p-values (under the choosen alpha) using 
  # the colSums funciton for the amount of non-NA cases in the respective p-value 
  # storage matrix
  
  nonpar.proportions <- colSums(!is.na(non.par.matrix))
  # Size/Power is the proportion of significant results divided by the number of 
  # simulations/tests
  nonpara.SizePower <- (nonpar.proportions[1] / nSim)
  
  # Same for the parametric test
  par.proportions <- colSums(!is.na(par.matrix))
  para.SizePower <- (par.proportions[1] / nSim)
  
  # Make a data frame of these values and present
  Result.frame <- data.frame(para.SizePower, nonpara.SizePower)
  
  return(Result.frame)
}


# Calling  function -----------------------------------------------------------

# Purpose - To call the Monte Carlo function and being capable of doing such with 
# different specifications - such as variations in sample size (n) or for varying 
# effect size (making the means sequentially differ) to evaluate such effects on 
# size and power

# Input - Output as variation in sample size (SampleSize = 1 (yes) or 0 (no)), 
# output as effect size 
# (Effect = 1 (yes) or 0 (no)), choosen alpha level (alpha), and two means and sds

# Output - See output from the General.MC function

Call.General.MC <- function(Distribution, SampleSize, Effect, alpha, MC.Mean1, 
                            MC.Sd1, MC.Mean2, MC.Sd2) {
  # Initial input check of arguments, print error message if erroneours
  
  if  ( SampleSize == 1 & Effect == 1 | SampleSize > 1 | SampleSize < 0 | 
        !is.numeric(SampleSize) | length(SampleSize) > 1 
       | !is.numeric(alpha) | length(alpha) > 1 | alpha < 0 | 
       ! is.numeric(MC.Mean1) | length(MC.Mean1) > 1 | MC.Mean1 < 0 | 
       !is.numeric(MC.Sd1) | length(MC.Sd1) > 1 | MC.Sd1 < 0 | 
       !is.numeric(MC.Mean2) | length(MC.Mean2) > 1 | MC.Mean2 < 0  
       | !is.numeric(MC.Sd2) | length(MC.Sd2) > 1 | MC.Sd2 < 0 | alpha == Inf
       | MC.Mean1 == Inf | MC.Sd1 == Inf| MC.Mean2 == Inf| MC.Sd2 == Inf) 
  {
    stop("Invalid arguments - Note: SampleSize and 
         Effect are binary - only one can be 1" )
  }
  # A seed is set to ensure reproducibility of current results
  set.seed(45578)
  
  # If output is to be size/power explored as an increase of sample sizes (n) - 
  # then SampleSize = 1 - SampleSize and Effect cannot both be 1, only one of them.
  if(SampleSize == 1) { 
    
    # Make a table to store the results (size/power) 
    Result.table <- data.frame(matrix( NaN, nrow =  3 ,ncol =  2))
    # Make a matrix with diffent sample sizes - these will be called sequentially
    n.matrix <- matrix( c(10, 100, 500, 1000), nrow = 1, ncol = 4)

 for (i in 1:4) {
   
   # Call the General.MC function with different "n" from the n-matrix 
   # Store these results in the result table (filled per row per iteration)
   Result.table[i,] <- General.MC(n = n.matrix[,i], Distribution, nSim = 1000, alpha,
                                  MC.Mean1, MC.Sd1, MC.Mean2, MC.Sd2)
 }
    # Assign names to the result table
  colnames(Result.table) <- c('Parametric', 'Non-Parametric')
  rownames(Result.table) <- c('n = 10', 'n = 100', 'n = 500', 'n = 1000')
  
  }
  
  # If output is to be size/power explored as an increase of effect sizes 
  # (mean difference) then Effect = 1 - Effect and SampleSize cannot both be 1,
  # only one of them.
  
  else if(Effect == 1) {
    # Matrix to store the results
    Result.table <- data.frame( matrix( NaN, nrow = 4, ncol = 2))
    # Matrix with the sequential increases of mean differences
    effect.matrix <- matrix( c(0, 0.2, 0.4, 0.6), nrow = 1, ncol = 4)
    
    for (i in 1:4) {
      # Call the General.MC function with different effect sizez from the effect-matrix 
      # Store these results in the result table (filled per row per iteration)
      # Observe that the difference here is that the first mean is made sequentially
      # smaller - thus increasing the difference if mean and thus the power
      Result.table[i,] <- General.MC(Distribution, n = 100, nSim = 1000, alpha, 
                                     MC.Mean1 - effect.matrix[,i] , MC.Sd1, MC.Mean2, MC.Sd2)
    }
    # Assign names to the result table
    colnames(Result.table) <- c('Parametric', 'Non-Parametric')
    rownames(Result.table) <- c('Effect = 0', 'Effect = 0.2', 'Effect = 0.4', 'Effect = 0.6')
    
  }
return(Result.table)
}

# Plotting function -------------------------------------------------------

# Purpose - To create plots of size and power for both scenarios of increases 
# in sample size, effect size, or for a simulation of size.

# Input - The "result-table" as the output from the Call.General.MC function

# Output - A plot to visually describe the effect or sample size or effect size 
# on power and size
Plotting.MC <- function(Result.table, SampleSize, Effect) {

  # If called for an output of power with sample size:
  if(SampleSize == 1) {
  
ggplot(Result.table) +
  geom_point(aes(x = c(10, 100, 500, 1000), y = Result.table$Parametric)) +
  geom_line(aes(x = c(10, 100, 500, 1000), y = Result.table$Parametric, color = 'Parametric')) +
  geom_point(aes(x = c(10, 100, 500, 1000), y = Result.table$`Non-Parametric`)) +
  geom_line(aes(x = c(10, 100, 500, 1000), y = Result.table$`Non-Parametric`, color = 'Non-Parametric')) + 
      ggtitle('Parametric and Non-Parametric Power as sample size increases') +
      labs(x = 'Sample size', y = 'Power') +
      theme_economist() + scale_colour_economist()
  }
  
  # If called for an output of power with effect size:
  else if(Effect == 1){
    
    ggplot(Result.table) +
      geom_point(aes(x = c(0, 0.2, 0.4, 0.6), y = Result.table$Parametric)) +
      geom_line(aes(x = c(0, 0.2, 0.4, 0.6), y = Result.table$Parametric, color = 'Parametric')) +
      geom_point(aes(x = c(0, 0.2, 0.4, 0.6), y = Result.table$`Non-Parametric`)) +
      geom_line(aes(x = c(0, 0.2, 0.4, 0.6), y = Result.table$`Non-Parametric`, color = 'Non-Parametric')) + 
      ggtitle('Parametric and Non-Parametric Power as effect size increases') +
      labs(x = 'Effect size', y = 'Power') +
      theme_economist() + scale_colour_economist()
  }

}
## Scenarios ---------------------------------------------------------------


# For Size ----------------------------------------------------------------

# Number one - Size with alpha of 0.05
Scenario1 <- Call.General.MC(SampleSize = 1, Effect = 0, alpha = 0.05, MC.Mean1 = 3, MC.Sd1 =1,
                                     MC.Mean2 = 3, MC.Sd2 = 1, Distribution = 'rnorm')
Plotting.MC(Scenario1, SampleSize = 1, Effect =  0)


# Number two - Size with alpha of 0.01
Scenario2 <- Call.General.MC(SampleSize = 1, Effect = 0, alpha = 0.01, MC.Mean1 = 3, MC.Sd1 = 1,
                MC.Mean2 = 3, MC.Sd2 = 1, Distribution = 'rnorm')
Plotting.MC(Scenario2, SampleSize =  1, Effect =  0)

# For Power ---------------------------------------------------------------

# Effect size - the differences in means increase sequentually by 0.2 units from 
# a difference of 0 to a diff of 0.6 - sd is the same
Scenario3 <- Call.General.MC( SampleSize = 0, Effect = 1, alpha = 0.05, MC.Mean1 = 3, MC.Sd1 = 1, 
                MC.Mean2 = 3, MC.Sd2 = 1, Distribution = 'rnorm')
Plotting.MC(Scenario3, SampleSize =  0, Effect =  1)

# Effect size - the differences in means increase sequentually by 0.2 units from 
# a difference of 0 to a diff of 0.6 - sd is different
Scenario4 <- Call.General.MC( SampleSize = 0, Effect = 1, alpha = 0.05, 
                MC.Mean1 = 3, MC.Sd1 = 0.75, MC.Mean2 = 3, MC.Sd2 = 1.5, Distribution = 'rnorm')
Plotting.MC(Scenario4, SampleSize =  0, Effect =  1)

# Effect size - the differences in means increase sequentually by 0.2 units from 
# a difference of 0 to a diff of 0.6 - sd is different - now tested under a chi-square
# distribution. Therefore the non-parametric test outperforms with regards to power.
Scenario5 <- Call.General.MC( SampleSize = 0, Effect = 1, alpha = 0.05, 
                              MC.Mean1 = 3, MC.Sd1 = 1, MC.Mean2 = 3, MC.Sd2 = 1, Distribution = 'rchisq')
Plotting.MC(Scenario5, SampleSize =  0, Effect =  1)


# In this scenario, the means are the same but there is a difference in sd
Scenario6 <- Call.General.MC( SampleSize = 0, Effect = 1, alpha = 0.05, 
                MC.Mean1 = 3, MC.Sd1 = 0.75, MC.Mean2 = 3, MC.Sd2 = 1.5, Distribution = 'rnorm')
Plotting.MC(Scenario6, SampleSize =  0, Effect =  1)


# In this scenario, the means and sd from original data for these genres are used
Scenario8 <- Call.General.MC(SampleSize = 1, Effect = 0, alpha = 0.05, 
                MC.Mean1 = mean(Animated$rating), MC.Sd1 = sd(Animated$rating),
                MC.Mean2 = mean(Sci.Fi$rating), MC.Sd2 = sd(Sci.Fi$rating), Distribution = 'rnorm')
Plotting.MC(Scenario8, SampleSize =  1, Effect =  0)



