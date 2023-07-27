DataFrameCollection = R6Class("DataFrameCollection",
                              public = list(
                                id = "",
                                job_dataframe = data.frame(),
                                buffer_dataframe = data.frame(),
                                permutations_dataframe = data.frame(),
                                instance_dataframe = data.frame(),
                                directory = "",
                                
                                initialize = function(id = "", directory = "calculated_FS"){
                                  job_doc <- xmlParse(paste(directory, "/", id, "/jobs.xml", sep =""))
                                  buffer_doc <- xmlParse(paste(directory, "/", id, "/buffer.xml", sep = ""))
                                  permutations_doc <- xmlParse(paste(directory, "/", id, "/permutation_results.xml", sep = ""))
                                  instance_doc <- xmlParse(paste(directory, "/", id, "/instance_data.xml", sep = ""))
                                  
                                  self$id <- id
                                  self$job_dataframe <- xmlToDataFrame(job_doc, c("character", "integer", "integer", "integer"))
                                  self$buffer_dataframe <- xmlToDataFrame(buffer_doc, c("integer", "integer", "integer", "character", "integer", "integer", "character", "character", "double", "double", "character"))
                                  self$permutations_dataframe <- xmlToDataFrame(permutations_doc, c("character" , "character", "integer", "integer"))
                                  self$instance_dataframe <- xmlToDataFrame(instance_doc, c("character", "character", "character"))
                                  self$directory = directory
                                  if(!self$validity_check()) print(paste("id:", id, "invalid"))
                                },
                                
                                get_jobs_as_list = function(row_in_perms){
                                  job_order_string <- self$permutations_dataframe[row_in_perms, "jobs"]
                                  jobs <- list()
                                  
                                  names <- as.list(strsplit(job_order_string, split = ", ")[[1]])
                                  
                                  for(i in 1:length(names)){
                                    name <- names[i]
                                    for(k in 1:nrow(self$job_dataframe)){
                                      if(self$job_dataframe[k, "name"] == name){
                                        jobs <- append(jobs, Job$new(self$job_dataframe[k, "time_on_m1"],
                                                                     self$job_dataframe[k, "time_on_m2"],
                                                                     self$job_dataframe[k, "size"],
                                                                     self$job_dataframe[k, "name"]))
                                      }
                                    }
                                  }
                                  if(length(jobs) != nrow(self$job_dataframe)){
                                    print("something went wrong with reading the jobs from jobs.xml")
                                    return(as.list(jobs))
                                  }
                                  else{
                                    return(as.list(jobs))
                                  }
                                },
                                
                                predetermined_sizes_to_list = function(){
                                  pre_sizes_string <- self$buffer_dataframe[[1, "predetermined_sizes"]]
                                  
                                  if(pre_sizes_string == "()"){
                                    return(list())
                                  }
                                  else{
                                    pre_sizes_string <- gsub('[(|)]', '', pre_sizes_string)
                                    
                                    sizes <- as.list(strsplit(pre_sizes_string, split = ", ")[[1]])
                                    
                                    int_sizes <- list()
                                    
                                    for(size in sizes){
                                      int_sizes <- append(int_sizes, as.integer(size))
                                    }
                                    
                                    return(int_sizes)
                                  }
                                },
                                
                                change_rate_to_list_or_seed = function(){
                                  change_rate <- self$buffer_dataframe[[1, "change_rate"]]
                                  if(grepl("(", change_rate, fixed = TRUE) | grepl(")", change_rate, fixed = TRUE)){
                                    
                                    if(change_rate == "()"){
                                      return(list())
                                    }
                                    else{
                                      change_rate <- gsub('[(|)]', '', change_rate)
                                      
                                      change_rate <- as.list(strsplit(change_rate, split = ", ")[[1]])
                                      
                                      int_sizes <- list()
                                      
                                      for(rate in change_rate){
                                        int_sizes <- append(int_sizes, as.integer(rate))
                                      }
                                      
                                      return(int_sizes)
                                    }
                                  }
                                  else{
                                    return(as.integer(change_rate))
                                  }
                                },
                                
                                intensity_to_list = function(){
                                  intensity <- self$buffer_dataframe[1, "intensity"]
                                  if(grepl("(", intensity, fixed = TRUE) | grepl(")", intensity, fixed = TRUE)){
                                    
                                    if(intensity == "()"){
                                      return(list())
                                    }
                                    else{
                                      intensity <- gsub('[(|)]', '', intensity)
                                      
                                      intensity <- as.list(strsplit(intensity, split = ", ")[[1]])
                                      
                                      int_sizes <- list()
                                      
                                      for(rate in intensity){
                                        int_sizes <- append(int_sizes, as.integer(rate))
                                      }
                                      return(int_sizes)
                                    }
                                  }
                                  else{
                                    return(as.integer(intensity))
                                  }
                                },
                                
                                get_buffer_max_size = function(){
                                  return(as.integer(self$buffer_dataframe[1, "max_size"]))
                                },
                                
                                get_buffer_min_size = function(){
                                  return(as.integer(self$buffer_dataframe[1, "min_size"]))
                                },
                                
                                get_buffer_seed = function(){
                                  return(self$buffer_dataframe[1, "seed"])
                                },
                                
                                get_buffer_max_change = function(){
                                  return(as.integer(self$buffer_dataframe[1, "max_change"]))
                                },
                                
                                get_buffer_min_change = function(){
                                  return(as.integer(self$buffer_dataframe[1, "min_change"]))
                                },
                                
                                get_buffer_intensity_scaling = function(){
                                  return(list(as.double(self$buffer_dataframe[1, "intensity_scaling_min"]), as.double(self$buffer_dataframe[1, "intensity_scaling_max"])))
                                },
                                
                                get_buffer_span = function(){
                                  val = self$buffer_dataframe[1, "span"]
                                  if(val == "TRUE") return(TRUE)
                                  else return(FALSE)
                                },
                                
                                get_variation_name = function(base_jobvar, base_crvar, base_intvar) {
                                  jobvar = gsub("\\.", "", self$instance_dataframe[1, "job_time_variation"])
                                  crvar = gsub("\\.", "", self$instance_dataframe[1, "change_rate_variation"])
                                  intvar = gsub("\\.", "", self$instance_dataframe[1, "intensity_variation"])
                                  
                                  if(base_jobvar != jobvar) return(paste("jobvar", jobvar, sep =""))
                                  if(base_crvar != crvar) return(paste("crvar", crvar, sep =""))
                                  if(base_intvar != intvar) return(paste("intvar", intvar, sep=""))
                                },
                                
                                recreate_buffer = function(){
                                  seed <- self$get_buffer_seed()
                                  min_size <- self$get_buffer_min_size()
                                  max_size <- self$get_buffer_max_size()
                                  min_change <- self$get_buffer_min_change()
                                  max_change <- self$get_buffer_max_change()
                                  change_rate <- self$change_rate_to_list_or_seed()
                                  predetermined_sizes <- self$predetermined_sizes_to_list()
                                  intensity <- self$intensity_to_list()
                                  intensity_scaling <- self$get_buffer_intensity_scaling()
                                  span <- self$get_buffer_span()

                                  return(Buffer$new(seed = seed,
                                                    min_size = min_size,
                                                    max_size = max_size,
                                                    min_change = min_change,
                                                    max_change = max_change,
                                                    change_rate = change_rate,
                                                    sizes = predetermined_sizes,
                                                    intensity = intensity,
                                                    intensity_scaling = intensity_scaling,
                                                    span = span))
                                },
                                
                                recreate_permutation_with_buffer = function(row_in_xml = 1){
                                  jobs <- self$get_jobs_as_list(row_in_xml)
                                  
                                  buffer <- self$recreate_buffer()
                                  
                                  perm <- Permutation$new()
                                  perm$jobs <- jobs
                                  perm$buffer <- buffer
                                  
                                  if(buffer$span) perm$calc_time_with_spanning_buffer(buffer)
                                  else perm$calc_time_with_buffer(buffer)
                                  
                                  return(perm)
                                },
                                
                                recreate_permutation_with_buffer_no_calc = function(row_in_xml = 1){
                                  jobs <- self$get_jobs_as_list(row_in_xml)
                                  
                                  buffer <- self$recreate_buffer()
                                  
                                  perm <- Permutation$new()
                                  perm$jobs <- jobs
                                  perm$buffer <- buffer
                                  
                                  return(perm)
                                },
                                
                                sort_permutations_dataframe_by_time_taken = function(){
                                  sorted_df <- self$permutations_dataframe[order(self$permutations_dataframe$time_taken), ]
                                  
                                  return(sorted_df)
                                },
                                
                                sort_permutations_dataframe_by_costs = function(){
                                  df <- self$sort_permutations_dataframe_by_time_taken()
                                  sorted_df <- df[order(df$costs), ]
                                  
                                  return(sorted_df)
                                },
                                
                                get_row_of_permutation_with_nth_lowest_time_taken = function(n){
                                  df <- self$sort_permutations_dataframe_by_time_taken()
                                  print("Permutation Data:")
                                  print(df[n, ])
                                  df[n, "num"]
                                },
                                
                                get_row_of_permutation_with_nth_highest_time_taken = function(n){
                                  df <- self$sort_permutations_dataframe_by_time_taken()
                                  print("Permutation Data:")
                                  print(df[nrow(df)-n+1, ])
                                  df[nrow(df)-n+1, "num"]
                                },
                                
                                get_row_of_permutation_with_nth_lowest_costs = function(n){
                                  df <- self$sort_permutations_dataframe_by_costs()
                                  print("Permutation Data:")
                                  print(df[n, ])
                                  df[n, "num"]
                                },
                                
                                get_row_of_permutation_with_nth_highest_costs = function(n){
                                  df <- self$sort_permutations_dataframe_by_costs()
                                  print("Permutation Data:")
                                  print(df[nrow(df)-n+1, ])
                                  df[nrow(df)-n+1, "num"]
                                },
                                
                                get_pareto_front = function(){
                                  perm <- self$permutations_dataframe
                                  
                                  front <- get_frontier(data = perm, x = time_taken, y = costs, quadrant = c("bottom.left")) 
                                  
                                  return(front)
                                },
                                
                                validity_check = function(){
                                  row_n <- strtoi(tail(self$permutations_dataframe, n=1)[1, "num"])
                                  num_of_jobs <- nrow(self$job_dataframe)
                                  return(row_n == factorial(num_of_jobs))
                                }
                              ))