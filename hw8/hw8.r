xUnique = 1:5
trueCoeff = c(0, 1, 1)

getData = function(coefs = c(0, 1, 1), xs = 1:5, dupl = 10,
                   sd = 5, seed=2222){
  ### This function creates the artificial data
  set.seed(seed)
  x = rep(xs, each = dupl)
  y = coefs[1] + coefs[2]*x + coefs[3] * x^2 + 
      rnorm(length(x), 0, sd)
  return(data.frame(x, y))
}

### 
genBootY = function(x, y, rep = TRUE){
  ### For each unique x value, take a sample of the
  ### corresponding y values, with replacement.
  ### Return a vector of random y values the same length as y
  ### You can assume that the xc(s are sorted
  ### Hint use tapply here!
  return(unlist(tapply(y,x, function(z) sample(z, length(z), replace = TRUE))))
}

genBootR = function(fit, err, rep = TRUE){
  ### Sample the errors 
  ### Add the errors to the fit to create a y vector
  ### Return a vector of y values the same length as fit
  ### HINT: It can be easier to sample the indices than the values
  return(fit + sample(err, length(fit), replace = FALSE))
}

fitModel = function(x, y, degree = 1){
  ### use the lm function to fit a line of a quadratic 
  ### e.g. y ~ x or y ~ x + I(x^2)
  ### y and x are numeric vectors of the same length
  ### Return the coefficients as a vector 
  ### HINT: Take a look at the repBoot function to see how to use lm()
  if(degree == 1){
    coeff = lm(formula = y~x)   
  }
  else{
    coeff = lm(formula = y~x+I(x^2))   
  }
  return(coeff)
}

oneBoot = function(data, fit = NULL, degree = 1){
  ###  data are either your data (from call to getData)
  ###  OR fit and errors from fit of line to data
  ###  OR fit and errors from fit of quadratic to data  

 
  ### Use fitModel to fit a model to this bootstrap Y 
  if (is.null(fit)){
    values = genBootY(data$x,data$y)
  }
  else{
    values = genBootR(fit[,1], fit[,2])
  }
  return(fitModel(data$x, values, degree))
}

repBoot = function(data, B = 1000){
  
  ### Set up the inputs you need for oneBoot, i.e.,
  ### create errors and fits for line and quadratic
  line = lm(y~x, data = data)
  quad = lm(y~x + I(x^2), data = data)
  
  line_error = data$y - line$fitted
  quad_error = data$y - quad$fitted
  
  line_fit = matrix(data = c(line$fitted,line_error), ncol = 2)
  quad_fit = matrix(data = c(quad$fitted,quad_error), ncol = 2)
  
  ### replicate a call to oneBoot B times
  ### format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a data frame with B rows
  ### and one or two columns, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  
  ### Replicate a call to oneBoot B times for 
  ### each of the four conditions
  
  line_with_line_fit = numeric()
  line_without_line_fit = numeric()
  quad_with_quad_fit = numeric()
  quad_without_quad_fit = numeric()
  

  for(i in 1:B){
    line_with_line_fit = c(line_with_line_fit, oneBoot(data, line_fit, 1)$coef)
    line_without_line_fit = c(line_without_line_fit, oneBoot(data, NULL, 1)$coef)
    quad_with_quad_fit = c(quad_with_quad_fit, oneBoot(data, quad_fit, 2)$coef)
    quad_without_quad_fit = c(quad_without_quad_fit, oneBoot(data, NULL, 2)$coef)
  }
  
  lwl.df = matrix(line_with_line_fit, nrow = 2)
  lwol.df = matrix(line_without_line_fit, nrow = 2)
  qwq.df = matrix(quad_with_quad_fit, nrow = 3)
  qwoq.df = matrix(quad_without_quad_fit, nrow = 3)
  
  ### Format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a matrix with B columns
  ### and two or three rows, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  
  coeff = list(lwl.df, lwol.df, qwq.df, qwoq.df)
  return(coeff)
} 

bootPlot = function(x, y, coeff, trueCoeff){
  ### x and y are the original data
  ### coeff is a matrix from repBoot
  ### trueCoeff contains the true coefficients 
  ### that generated the data
  
  ### Make a scatter plot of data
  
  plot(x,y)

  ### Add lines or curves for each row in coeff
  ### Use transparency
  ### You should use mapply to construct all 
  ### 1000 of the bootstrapped lines of best fit 
  ### Have a look at ?mapply for details.
  ### This can be done in ggplot2 or base graphics.
#   browser()
  if(nrow(coeff) == 2){
    sapply(1:ncol(coeff), function(x) abline(coef = coeff[,x], col = rgb(1, 0, 0, alpha = 0.1)))
  }
  else{
    sapply(1:ncol(coeff), function(z) curve((function (x) coeff[1,z]+coeff[2,z]*x+coeff[3,z]*x^2)(x), col = rgb(1, 0, 0, alpha = 0.1), add = TRUE))
  }

  
  ### Use trueCoeff to add true line/curve - 
  ###  Make the true line/curve stand out

  curve((function (x) trueCoeff[1]+trueCoeff[2]*x+trueCoeff[3]*x^2)(x), add = TRUE, col = 'blue')

}

### Run your simulation by calling this function
### This function doesn't need any changing
runSim = function() {
  xUnique = 1:5
  trueCoeff = c(0, 1, 1)
  myData = getData(coefs = trueCoeff, xs = xUnique)
  expt = repBoot(data = myData)
  par(mfrow = c(2, 2))
  for (i in 1:4){
   bootPlot(myData$x, myData$y, 
            coeff = expt[[i]], trueCoeff) 
  }
  return(expt)
}
