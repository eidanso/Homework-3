rm(list=ls())

##### Q1 Solving for Univariate nonlinear Equation using Bisection Method
#---------------------------------------------------------------------------------------

bisec_function <- function(f,a,b,tol=1e-15,maxN=10000){
  c <- (a + b)/2
  fa <- f(a)
  fb <- f(b)
  fc <- f(c)
  
  # Exploring to see if solution can be found in [a,b]
  if (fa * fb > 0) {
    stop("No Solution Found!:")
  }
  
  if ((abs(a-b) < tol) | (abs(fc) < tol)){
    return(c)
    break
  }
  
  # Main Loop:
  for (i in 1:maxN){
    if (fa * fc < 0){
      b <- c
      fb <- fc
    }
    else {
      a <- c
      fa <- fc
    }
    if ((abs(a-b) < tol) | (abs(fc) < tol)){
      message(sprintf("Solution Found in %d iterations.",i))
      return(c)
      break
    }
    c <- (a + b)/2
    fc <- f(c)
  }
  message("Warning - reached maximum number of iterations!")
  return(c)
}

#### Nonlinear equation to test the function 
bisec_func <- function(x)
  return(x ** 3 + 3 * x ** 2 - 2 * x + 4)

# Test the function
bisec_soln <- bisec_function(bisec_func, -90, 500)



#-----------------------------------------------------------
# Q2 Using Newton's Method to solve nonlinear equations 
newton <- function(f, derivative, x0 = 0.2, tol = 1e-15, maxN = 10000, searchPoint = 0)
{
  # We could also get the derivative through approximation... 
  # I think we're supposed to use the actual derivative for the HW though...
  
  x <- x0
  fx <- f(x)
  df <- derivative(x)
  i <- 0
  
  while(abs(fx - searchPoint) > tol & i < maxN)
  {
    # Calculate the new guess
    x <- ((searchPoint - fx) / derivative(x)) + x  # Per math 
    
    fx <- f(x)
    df <- derivative(x)
    i <- i + 1
  }
  
  if(i == maxN)
    message("Danger: Reached max iterations")
  
  message(sprintf("The Nonlinear Equation was solved with Newton's Methon in %d iterations.", i))
  return(x)
}



#### Nonlinear equation to test the function with Newton
newton_func <- function(x)
  return(x ** 3 + 3 * x ** 2 - 2 * x + 4)

# Testing the Function
Newton_soln <- newton(newton_func, bisec_func, 0)




#-----------------------------------------------------------
# Question 3: Bond Prices
n <- matrix(c(0.25,0.5,0.5,1,2,5,10,30),ncol=1)
C <- matrix(c(0.000,0.000,0.000,0.000, 0.00625,0.01375,0.02,0.02875),ncol=1)
FV <- matrix(c(100,100,100,100,100,100,100,100),ncol=1)
P <- matrix(c(100.0025,100.00,99.965,99.8004,100.1445,100.5156,100.2344,100.8438),ncol=1)

ytm <- matrix(NA,ncol=2,nrow=8)
df <- as.data.frame(cbind(n,C,FV,P,ytm))
names(df) <- c("Years to Maturity","Coupon (%)","Face Value","Market Price ($)", "Calculated YTM (BISECTION)", "Calculated YTM (NEWTON)")
#View(df)
a <- -0.02
b <- 0.2
x0 <- 0.111


for(i in 1:8){
  n <- df$`Years to Maturity`[i]
  C <- df$`Coupon (%)`[i] * df$`Face Value`[i]
  FV <- df$`Face Value`[i]
  P <- df$`Market Price ($)`[i]
  
  #### Bond Pricing Formula
  PV <- function(C,r,FV,n){
    P <- C/r*(1 - 1/(1+r)^n) + FV/(1+r)^n
    return(P)
  }
  
  Obj_bisec <- function(r){
    x <- PV(C,r,FV,n) - P
    return(x)
  }
  
  df$`Calculated YTM (BISECTION)`[i] <- bisec_function(Obj_bisec,a,b)
  
  
  #### Using Newton Method
  # First derivative:
  obj_new <- function(r){
    x <- -C/r^2 - 
      (-C/r^2*(1/(1+r)^n) + C/r*n*(1+r)^(-(1+n))) - 
      FV*n*(1 + r)^(-(1+n))
    return(x)
  }
  
  df$`Calculated YTM (NEWTON)`[i] <- newton(Obj_bisec,obj_new,x0)
}


print(df)