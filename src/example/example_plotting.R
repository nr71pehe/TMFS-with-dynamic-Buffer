#This is an example-file where there will be some examples and explanations on how to plot different graphics for two machine flow shops with dynamic intermediate or spanning buffer.
#As a reminder: make sure to set the working directory to "<Path>/<to>/TMFSDB/src".

#To execute any of this make sure all libraries mentioned in Shopowner.R are installed. Then source it twice (just to be sure; can be buggy sometimes). 
source("Shopowner.R"); source("Shopowner.R");
#First, define some runtimes on machine1 (a_i) and machine2 (b),
{
  a1 <- 13
  a2 <- 26
  a3 <- 29
  a4 <- 10
  a5 <- 27
  a6 <- 12
  b <- 20
}

#create Jobs with those.
{
  j1 <- Job$new(time_machine_one = a1, time_machine_two = b, size = a1, name = r'($J_1$)')
  j2 <- Job$new(time_machine_one = a2, time_machine_two = b, size = a2, name = r'($J_2$)')
  j3 <- Job$new(time_machine_one = a3, time_machine_two = b, size = a3, name = r'($J_3$)')
  j4 <- Job$new(time_machine_one = a4, time_machine_two = b, size = a4, name = r'($J_4$)')
  j5 <- Job$new(time_machine_one = a5, time_machine_two = b, size = a5, name = r'($J_5$)')
  j6 <- Job$new(time_machine_one = a6, time_machine_two = b, size = a6, name = r'($J_6$)')
}
#and define an order for the jobs (permutation)
jobs = list(j1, j2, j3, j4, j5, j6)

#Also, we will need a seed to "define the randomness" 
seed = 04109

#and a minimum and maximum value for the buffer. If both are equal the buffer will be constant and have that given size.
min = 1
max = 42

#Now we can define the Buffer with some variation. This will be a spanning Buffer with a volatility variation of [0.2, 0.4] and a constant change rate of 10.
#The volatility variation means, that it can have any integer size in [(1-0.4)*(max/2), (1-0.2)*(max/2)] and [(1+0.2)*(max/2), (1+0.4)*(max/2)].
buffer = Buffer$new(seed = seed, min_size = min, max_size = max, span = TRUE, intensity_scaling = list(0.2, 0.4), change_rate = list(10))

#Now the given job order can be calculated with the given buffer.
perm <- Permutation$new(buffer = buffer, jobs = jobs)
#fyi: there also is a class named PermutationWithMax where after initialization a custom maximum time (perm$max_time) can be defined which causes the calculation to stop automatically after given time.

#To plot the given permutation execute
#The "...lines" inputs are optional and are used to vary the displayed lines in the plot.
plot_permutation(perm, buffer, steps_vlines = 100, lty_vlines = 2, lty_hlines = 2, steps_hlines = 10)

#To display the pareto-front for the pareto-optimal solution out from all possible permutations for given seed use
instance_id = "jobvar01u03crvar1intvar0u1"
directory = "calculated_instances/spanning_isolated/"
dfc = DataFrameCollection$new(id = instance_id, directory = paste0(directory, "d-", seed))
#where instance_id is a composed string that defines the used variations and directory is the path to the calculated seed 
#(i.e. jobvar01u03 => v_job = [0.1, 0.3], crvar1 => v_cr = 1, intvar0u1 => v_vola = [0, 1])

plot_DataFrameCollection(dfc = dfc)
