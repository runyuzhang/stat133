#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.


bml.sim(50,50,0.4)

bml.sim(50,50,0.3)

bml.sim(50,50,0.35)

p = c(35:50/100)

p_gridlock = 35:50

num_steps_for_gridlock = 35:50

sim = function(r,c){
  for (i in 1:length(p)){
    print(p[i])
    results = bml.sim(r,c,p[i])
    p_gridlock[i] = results[[1]]
    num_steps_for_gridlock[i] = results[[2]]
  }
  
  jpeg(paste(r,"*", c, "_", 1,".jpg",sep = ""))
  
  plot(p, p_gridlock, main = paste("Probability of hitting a gridlock vs. p for matrix ", r, "*", c, sep=""), xlab = "p", ylab = "P(Gridlock|p)")
  text(p,p_gridlock,p_gridlock, pos  =3)
  dev.off()
  jpeg(paste(r,"*", c, "_", 2,".jpg",sep = ""))
  plot(p, num_steps_for_gridlock, main = paste("Number of steps to reach gridlock vs. p for matrix ", r, "*", c, sep=""), xlab = "p", ylab = "Number of steps to reach gridlock")
  text(p,num_steps_for_gridlock, ceiling(num_steps_for_gridlock), pos  =2)
  dev.off()
}

sim(10,10)
sim(30,30)
sim(50,50)
sim(70,70)
sim(100,100)
sim(20,45)
sim(15,60)
sim(10,90)