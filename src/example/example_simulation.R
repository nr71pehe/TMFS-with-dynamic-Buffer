#This is an example-file where there will be some examples and explanations on how to start and save simulations for two machine flow shops with dynamic intermediate or spanning buffer.
#Also make sure to set the working directory to "<absolute>/<Path>/<to>/TMFSDB/src".

#To execute any of this make sure all libraries mentioned in Shopowner.R are installed. Then source it twice (just to be sure; can be buggy sometimes). 
source("Shopowner.R"); source("Shopowner.R");

#At first to define the randomness in the calculations you will have to create a list of seeds

seed_list <- list(04103, 04107, 04109)
#Now define a constant (for size orientation) c and if the used buffer should be a spanning Buffer or not. If it is not it will be an intermediate Buffer.
#Also there has to be different directories for each buffertype.
{
  c <- 20
  span <- TRUE
  if(span) span_string <- "spanning_isolated"
  else span_string <- "intermediate_isolated"
  directory <- paste("calculated_instances/", span_string, sep="")
}

#Now you can start looping through the seeds and run start_calculating_isolated_instances to start the simulation.
#This may take some time (especially for spanning buffers) depending on your CPU. Consider splitting the seed_list and run this as multiple Jobs.
#You will be informed, if one instance is done and if the entire seed is calculated. It will also be displayed what the given job times are and how much time the simulation took (in minutes).
for(seed in seed_list){
  d_seed <- paste("d-", seed, sep = "")
  start_calculating_isolated_instances(c = c, seed = seed, directory = directory, span = span)
  old_dir <- paste(directory, "/", seed, sep="")
  new_dir <- paste(directory, "/", d_seed, sep="")
  if(dir.exists(new_dir)){
    print(paste(seed, "is already done!"))
  }
  else{
    shell(paste('rename', " \"", old_dir, "\" ", d_seed, sep=""))
  }
}
#The results are saved in the corresponding directories as .xml-files
