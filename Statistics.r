# ilbey evcil - 2079242

x <- rexp(50, rate=1000)

for (i in 1 : 50) {
  if(x[i] <= -1){
    x[i] <- 0
  } else if (x[i] <= 0){
    x[i] <- 6 * x[i] + 3
    
  } else if (x[i] <= (1/32)){
    x[i] <- 3 + (4 * sqrt(2 * x[i]))
    
  } else{
    x[i] <- 1
  }  
}
hist(x, main = "first_function_histogram")
# expected value (mean)
a <- mean(x)
# standard deviation
b <- sd(x)
#confidence level = %95
# for n = 100
error <- qexp(0.975)*b/sqrt(100)
left <- a-error
right <- a+error

# for n = 1000
error <- qexp(0.975)*b/sqrt(1000)
left <- a-error
right <- a+error

# for n = 10000
error <- qexp(0.975)*b/sqrt(10000)
left <- a-error
right <- a+error



y <- 15 * runif(50) + 5
hist(y, main = "second_function_histogram")
# expected value (mean)
c <- mean(y)
# standard deviation
d <- sd(y)
# confidence level = %95
# for n = 100
error <- qnorm(0.975)*d/sqrt(100)
left <- c-error
right <- c+error

# for n = 1000
error <- qnorm(0.975)*d/sqrt(1000)
left <- c-error
right <- c+error

# for n = 10000
error <- qnorm(0.975)*d/sqrt(10000)
left <- c-error
right <- c+error