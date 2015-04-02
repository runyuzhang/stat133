#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
  return(sample(c(0,1,2), size = r*c, prob = (1-p,p/2,p/2), replace = TRUE), nrow = r, ncol = c, byrow = TRUE)
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

bml.step <- function(m){
  step_car <- function(m,car){
    if((car == 1 & ncol(m) == 1) | (car == 2 & nrow(m) == 1)){
      return(m)
    }
    cars = m*(m==car)
    block = (m!=0)[,c(2:ncol(m), 1)]
    return(cars*block + (cars*!block)[,c(ncol(m), 1:(ncol(m)) - 1)] + m*(m!=car))
  }
  m_red = t(step_car(m,1))[,c(nrow(m):1)]
#   browser()
  m_blue = t(step_car(m_red, 2)[,c(nrow(m):1)])
  return(list(m_blue, !all(m_blue==m)))
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
  step_n_times = function(m, n){
    for (i in 1:n){
      result = bml.step(m)
      m = result[[1]]
      grid.new = result[2]
      if (grid.new == FALSE){
        return(i)
      }
    }
    return(n)
  }
  num_experiments = 20
  n = 500
  list_results = lapply(c(1:num_experiments), function(x) step_n_times(bml.init(r,c,p), n))
  print(length(which(list_results == n)) / num_experiments)
  print(mean(unlist(list_results[which(list_results != n)])))
  return(list(1 - length(which(list_results == n)) / num_experiments, mean(unlist(list_results[which(list_results != n)]))))
}
