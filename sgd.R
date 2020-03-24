for (i in 1:69){
print("ur mum")
}

###downloaded off the internet but wanted to do it myself to understand it
gradientDesc <- function(x, y, learn_rate, conv_threshold, n, max_iter) {
  plot(x, y, col = "blue", pch = 20)
  m <- runif(1, 0, 1)
  c <- runif(1, 0, 1)
  yhat <- m * x + c
  MSE <- sum((y - yhat) ^ 2) / n
  converged = F
  iterations = 0
  while(converged == F) {
    ## Implement the gradient descent algorithm
    m_new <- m - learn_rate * ((1 / n) * (sum((yhat - y) * x)))
    c_new <- c - learn_rate * ((1 / n) * (sum(yhat - y)))
    m <- m_new
    c <- c_new
    yhat <- m * x + c
    MSE_new <- sum((y - yhat) ^ 2) / n
    if(MSE - MSE_new <= conv_threshold) {
      abline(c, m) 
      converged = T
      return(paste("Optimal intercept:", c, "Optimal slope:", m))
    }
    iterations = iterations + 1
    if(iterations > max_iter) { 
      abline(c, m) 
      converged = T
      return(paste("Optimal intercept:", c, "Optimal slope:", m))
    }
  }
}

gradientDesc(data.frame(x1,x2,x3), y, 0.0000293, 0.001, 5, 2500)

###my code below

##observations
x1 = c(3,0,1,1,0)
x2 = c(0,1,3,5,2)
x3 = c(3,2,0,2,0)

#docs
d1=c(3,0,3)
d2=c(0,1,2)
d3=c(1,3,0)
d4=c(1,5,2)
d5=c(0,2,0)

#default weighting and bias
w = c(1,-1,1)
b = 0

#y values for each obs
y = c(1,1,0,0,0)



dotprod = function(x,w){
  result = 0
  for (i in 1:length(x)){
    result = result + (x[i]*w[i])
  }
  return(result)
}

ccsigma = function(x){
  return(1/(1+exp(-1*x)))
}

lce = function(x, w, y, b){
  result = -1*((y*log(ccsigma(dotprod(x,w)+b))) + ((1-y)*log(1-ccsigma(dotprod(x,w)+b))))
  return(result)
}

gradient = function(x, w, y, b){
  theta = c()
  for (i in 1:length(w)){
    theta = c(theta, 0)
  }
  for (i in 1:length(theta)){
    theta[i] = ((ccsigma(w[i])+b)-y)*x[i]
  }
  theta = c(theta, ccsigma(b)-y)
  return(theta)
  
}

graddesc = function(docs, w, y, b, eta, iters, end){
  
  
  
  for (i in 1:iters){
    theta = gradient(docs[i], w, y, b)
    if (max(theta)<end){
      break
      } else {
      for(j in 1:length(theta))
        {theta[j] = theta[j] - eta*theta[j]}
      
    }
  }
  return(theta)
}
  


