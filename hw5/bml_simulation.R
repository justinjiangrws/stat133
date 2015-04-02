#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.

par(las = 2)

bml.run <- function(r, c, p, step = 5000, runsize){
  list <- c()
  counter <- 1
  while (counter < runsize + 1){
    m <- bml.sim(r,c,p, step)
    counter <- counter + 1
    list <- c(list, m)
  }
  return (list)
}


p_val.study <- function (r, c, step = 5000, runsize){
  count <- 0.1
  list <- c()
  while (count < 1){
    run <- bml.run(r,c,count,step,runsize)
    list <- c(list,run)
    count <- count + 0.1
  }
  study <- matrix(list,nrow = runsize, dimnames = list(c(paste0("run",1:runsize)), c(paste0("pval",seq(.1,1,by=0.1)))))
  return (study)
}

pval_mat <- p_val.study(5,5,step = 5000,50)
#bplot_pval <- boxplot(study_mat, main = "Steps Until Gridlock Against P-Value", xlab = "P-value",ylab = "Steps Until Gridlock")

runsize.study <- function (r, c, p, step = 5000, start, end){
  count <- start
  list <- c()
  while (count < end){
    run <- bml.run(r,c,p,step,count)
    list <- c(list,run)
    for (i in count:(end - 1)){
        list <- c(list,NA)
    }
    count <- count + 1
  }
  study <- matrix(list,nrow = end)
  return (study)
}

#runsize_mat <- runsize.study(15,15, 0.5, step = 5000,25,50)
#bplot_runsize <- boxplot(runsize_mat, main = "Steps Until Gridlock Against Runsize",xlab = "Runsize",ylab = "Steps Until Gridlock")

#p_val.study(r,c,step = 5000,50)
#runsize.study(r,c,0.5,step = 5000, 25, 50)

#do this for different values of r,c
