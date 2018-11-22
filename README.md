# MT4113-Assignment.3-180028774
Assignment 3

The code for running the current Monte Carlo simulations and scenarios is relatively straight forward. There is one script which contains all code with documentation as much as thought to be needed. Running it from top-to-botton should ensure capture of all code and for the simulations to be called in the end. Running a simulation is expected to take between 4,3-9,5 seconds (see attached profvis files). 

What is important to emphesize is the usage of the arguments to call the functions. I have resorted to using a binary switch-board style for my arguments whereby a '1', e.g SampleSize = 1, will provide the size / power as simulated with a set variations in sample sizes ( 10, 100, 500, 1000). There are two such binary arguments, only one can be switched 'on' at one (Effect = 1, SampleSize = 0 or vice versa). There are other arguments too but these are more straight forward - alpha is for the alpha level, Distribution is most often 'rnorm' in order for random resampling from a normal probability distribution but note that this can also be turned to Distribution = 'rchisq'. 

The scenarios are set up and 'ready-to-go' however if you´d like to change the properties of the call - the means and standard deviations - then MC.Mean1, MC.Sd1 control one samples specifications and MC.Mean2 and MC.Sd2 the other. 

A seed has been set and should ensure reproducability of my work and findings. 

Best of luck and thank you for taking your time to read this description!
