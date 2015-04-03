# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).

sim.doctors <- function(initial.doctors, n.doctors, n.days, p){

  # Set up the output variable, define it as a matrix then use initial.doctors
  # to set the first column (day)

  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p

  # return the output
  
  output.matrix = matrix(0, ncol = n.doctors + 1, nrow = n.doctors, byrow = FALSE)
  output.matrix[,1] = 1:n.doctors
  
#   browser()
  for (i in 1:n.days+1){
    if (i > 2){
      output.matrix[,i] = output.matrix[,i-1]
    }
    doctors = sample(1:n.doctors, 2, replace = F)
    agreements = which(initial.doctors[doctors] == 1)
    if (length(agreements) == 1){
      output.matrix[doctors[[agreements]], i] = 1
      output.matrix[doctors[which(initial.doctors[doctors] != 1)], i] = sample(c(0,1), p = c(1-p, p), size = 1)
    }
    else if (length(agreements) == 2){
      output.matrix[doctors[[1]], i] = 1
      output.matrix[doctors[[2]], i] = 1
    }
  }
  return(output.matrix)

}

# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

set.seed(42)
# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)

n.doctors = 100

initial.doctors = sample(0:1, size = n.doctors, replace = T, p = c(0.9, 0.1))
n.days = 100

p.s = seq(0,0.8, length.out = 5)
colours = rainbow(5)
for (j in 1:length(p.s)){
  print(j)
  p = p.s[j]
  num_agreements = 1:n.days
  x = sim.doctors(initial.doctors, n.doctors, n.days, p)
  for (i in 2:n.days + 1){
    num_agreements[i-1] = length(which(x[,i] == 1))
  }
  if(j == 1){
    
    plot(1:n.days, num_agreements, pch = 46, type = "l", col = colours[j], main = "# of doctors adopting the drug vs. days", xlab = "Days", ylab = "# of doctors adopting the drug")
  }
  else{
    lines(1:n.days, num_agreements, pch = 22, col = colours[j])
  }
}

legend(x=70, y=10, legend=p.s, lwd=2,
       col=colours, title = "p value")



