#' Q1: You roll N fair six-sided dice. 
#' The sum of the values is M. 
#' We wish to know about the product of the faces.

#' This script is for data incubator challenge
#' This is a numeric solution. According to law of large numbers, the sample mean should be close 
#' to the expected value when the size of the sample is large enough.
#' Therefore, I simulated 100,000 randome trials of each sequencial N times roll.
#' Then calculate the sample mean and sample standard deviation for the prodcut of faces, which sum is M.
#' Date: 10/29/2016
#' Author: Ying Yang

############################################################################################################
#' define a function of rolling a dice N times with the sum as M
dice_roll <- function(N, M){
        dice <- sample(1:6, size = N, replace = TRUE)
        #print(dice)
        if(sum(dice) == M){
                return(prod(dice))
        }else{
                return(0)
        }
}
###########################################################################################################
#' set the digits requirement
options(digits=10)
#' define a function which calculates the expectation and standard deviation when 
#' using 100,000 times simulations with N as the number of rolls and M as the sum of the faces
set.seed(1234)
exp_std <- function(N, M, S){
        # exception control
        if(M < 0){
                return("This is not a right input. M should be bigger than 0")
        }else if(M > 6*N){
                return(paste("This is not a right input. ", "M should be smaller than ",6*N, sep = ""))
        }else{
                dice_simulation <- replicate(S, dice_roll(N, M))    
                return(data.frame("Expectation" = mean(dice_simulation), "Standard Deviation" = sd(dice_simulation)))
        }
}
###############################################################################################################
#' Calculate the expectation and standard deviation when 
#' N = 8, M = 24, S = 100000
exp_std(8, 24, 100000)
#' Calculate the expectation and standard deviation when 
#' N = 50, M = 150, S = 100000
exp_std(50, 150, 100000)

