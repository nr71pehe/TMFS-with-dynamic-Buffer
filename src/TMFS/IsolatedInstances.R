Instances <- R6Class("IsolatedInstances",
                     public = list(
                       c = NULL,
                       seed = NULL,
                       permutations = NULL,
                       base_job_time_variation = NULL,
                       job_time_variation = NULL,
                       intensity_variation = NULL,
                       change_rate_variation = NULL,
                       num_of_jobs = NULL,
                       min_size = NULL,
                       span = TRUE,
                       instances_data = NULL,
                       
                       initialize = function(c, seed,
                                             num_of_jobs = 6,
                                             base_job_time_variation = list(0.1, 0.3),
                                             base_intensity_variation = list(0.0, 1.0),
                                             base_change_rate_variation = 1,
                                             job_time_variation = list(list(0.25, 0.45), list(0.45, 0.65), list(0.65, 0.85)),
                                             intensity_variation = list(list(0.1, 0.5), list(0.3, 0.7), list(0.5, 0.9)), 
                                             change_rate_variation = list(0.8, 0.5, 0.2),
                                             min_size = 1,
                                             span = TRUE){
                         if(c != 20) {
                           print("c!=20, inputs in instances.initialize anpassen!")
                           return();
                         }
                         
                         self$c <- c
                         self$seed <- seed
                         self$base_job_time_variation <- 
                         self$job_time_variation <- job_time_variation
                         self$intensity_variation <- intensity_variation
                         self$change_rate_variation <- change_rate_variation
                         self$num_of_jobs <- num_of_jobs
                         self$min_size <- min_size
                         self$span <- span
                         self
                         self$instances_data <- data.frame("job_time_variation" = sizes_to_string(job_time_variation),
                                                           "intensity_variation" = sizes_to_string(intensity_variation),
                                                           "change_rate_variation" = sizes_to_string(change_rate_variation))
                         
                         jobs = list()
                         buffers = list()
                         instance_datas = list()
                         permutations = list()
                         
                         perm <- self$gen_permutation_with_variations(job_time_var_range = base_job_time_variation,
                                                                                  intensity_var = base_intensity_variation,
                                                                                  change_rate_var = base_change_rate_variation,
                                                                                  min_size = min_size)
                         
                         jobs <- append(jobs, list(perm$jobs))
                         buffers <- append(buffers, perm$buffer)
                         dfcontainer <- DataFrameContainer$new(perm$instance_data)
                         instance_datas <- append(instance_datas, dfcontainer)
                         
                         for(job_time_variation in job_time_variation) {
                           perm <- self$gen_permutation_with_variations(job_time_var_range = job_time_variation,
                                                                        intensity_var = base_intensity_variation,
                                                                        change_rate_var = base_change_rate_variation,
                                                                        min_size = min_size)
                           
                           jobs <- append(jobs, list(perm$jobs))
                           buffers <- append(buffers, perm$buffer)
                           dfcontainer <- DataFrameContainer$new(perm$instance_data)
                           instance_datas <- append(instance_datas, dfcontainer)
                         }
                         
                         for(intensity_variation in intensity_variation) {
                           perm <- self$gen_permutation_with_variations(job_time_var_range = base_job_time_variation,
                                                                        intensity_var = intensity_variation,
                                                                        change_rate_var = base_change_rate_variation,
                                                                        min_size = min_size)
                           
                           jobs <- append(jobs, list(perm$jobs))
                           buffers <- append(buffers, perm$buffer)
                           dfcontainer <- DataFrameContainer$new(perm$instance_data)
                           instance_datas <- append(instance_datas, dfcontainer)
                         }
                         
                         for(change_rate_variation in change_rate_variation) {
                           perm <- self$gen_permutation_with_variations(job_time_var_range = base_job_time_variation,
                                                                        intensity_var = base_intensity_variation,
                                                                        change_rate_var = change_rate_variation,
                                                                        min_size = min_size)
                           
                           jobs <- append(jobs, list(perm$jobs))
                           buffers <- append(buffers, perm$buffer)
                           dfcontainer <- DataFrameContainer$new(perm$instance_data)
                           instance_datas <- append(instance_datas, dfcontainer)
                         }
                         
                         for(i in 1:length(jobs)){
                           perm = Permutation$new(buffer = buffers[[i]], jobs = jobs[[i]], instance_data = instance_datas[[i]]$get_df())
                           permutations = append(permutations, perm)
                         }
                         self$permutations = permutations
                       },
                       
                       gen_permutation_with_variations = function(job_time_var_range, intensity_var, change_rate_var, min_size = 1){
                         
                         jobs = gen_jobs(c = self$c,
                                         seed = self$seed,
                                         variation = job_time_var_range, 
                                         num_of_jobs = self$num_of_jobs)
                         
                         
                         upper_bound_intensity = intensity_var[[2]]
                         
                         size_that_should_fit = get_biggest_job_size_on_m1(jobs) + get_second_smallest_job_size_on_m1(jobs)
                         
                         if(!self$span) {
                           size_that_should_fit = get_second_smallest_job_size_on_m1(jobs)
                         }
                         
                         buffer_max_size = calculate_buffer_size_so_that_given_size_definitely_fits(size_that_should_fit, upper_bound_intensity)
            
                         buffer = gen_buffer(c = self$c, 
                                             intensity_scaling = intensity_var, 
                                             change_rate_variation = change_rate_var, 
                                             seed = self$seed,
                                             jobs = jobs,
                                             min_size = min_size,
                                             max_size = buffer_max_size,
                                             span = self$span)
                         
                         instance_data = data.frame("job_time_variation" = range_to_string(job_time_var_range),
                                                    "change_rate_variation" = change_rate_var,
                                                    "intensity_variation" = range_to_string(intensity_var))
                         
                         perm = Permutation$new(buffer = buffer, jobs = jobs, instance_data = instance_data)
                      
                         return(perm)
                       },
                       
                       save_data_of_instances = function(directory){
                         write.xml(self$instances_data, file = paste("./", directory, "/instances_data.xml", sep=""))
                       },
                       
                       boxed_variation_string_to_list = function(str){
                         s <- ""
                         eval(parse(s = paste("list", str, sep="")))
                         
                         print(s)
                       }
                     ))