library(R6)
library(combinat)
library(digest)
library(kulife)
library(XML)
library(latex2exp)
library(ggplot2)
library(dplyr)
library(ggalt)
library(ggforce)
library(concaveman)
library(KraljicMatrix)
library(foreach)
library(REdaS)
library(readr)
source("./Objects/RectWithLabel.R")
source("./Objects/DataFrameCollection.R")
source("./Objects/PlotSchedule.R")
source("./Objects/intervals.R")
source("./Objects/DisplayBuffer.R")
source("./Objects/BufferFillData.R")
source("./Objects/Algorithms.R")
source("./Objects/DataFrameContainer.R")
source("./Objects/ParetoContainer.R")
source("./TMFS/Block.R")
source("./TMFS/Buffer.R")
source("./TMFS/Job.R")
source("./TMFS/Machine.R")
source("./TMFS/Permutation.R")
source("./TMFS/PermutationWithMax.R")
source("./TMFS/Instances.R")
source("./TMFS/IsolatedInstances.R")
source("./TMFS/Experiment.R")

get_all_combinations = function(perm){
  num_jobs <- length(perm$jobs)
  job_combi <- permn((perm$jobs))
  
  permutations <- list()
  
  for(job in job_combi){
    n_perm <- Permutation$new()
    n_perm$jobs <- job 
    permutations <- append(permutations, n_perm)
  }
  return(permutations)
}


calc_perm_list_for_perm = function(perm, buffer){
  perms <- get_all_combinations(perm)
  
  print(Sys.time())
  
  for(n_perm in perms){
    if(buffer$span) n_perm$calc_time_with_spanning_buffer(buffer)
    
    else n_perm$calc_time_with_buffer(buffer)
  }
  return(perms)
}

get_job_order_string = function(jobs){
  str = ""
  for(job in jobs){
    if(str == ""){ 
      str <- paste(str, job$name, sep="")
      next
      }
    else str <- paste(str, job$name, sep=", ")
  }
  
  return(str)
}

job_to_dataframe = function(job){
  data.frame("name" = job$name, 
             "time_on_m1" = job$time_machine_one, 
             "time_on_m2" = job$time_machine_two, 
             "size" = job$size) 
}

add_job_to_dataframe = function(df, job){
  job_df <- job_to_dataframe(job)
  
  return(rbind(df, job_df))
}

job_list_to_dataframe = function(jobs){
  df = data.frame()
  
  for(job in jobs){
    df <- add_job_to_dataframe(df, job)
  }
  
  return(df)
}

job_dataframe_to_string = function(df){
  str <- ""
  for(row in 1:nrow(df)){
    if(str == ""){
      str <- paste(str, df[row, "name"], sep="")
    }
    else{
      str <- paste(str, df[row, "name"], sep=", ")
    }
  }
  return(str)
}

permutation_to_dataframe = function(perm, num = NULL){
  if(is.null(num)){
  data.frame("jobs" = get_job_order_string(perm$jobs),
             "time_taken" = perm$time_taken,
             "costs" = perm$buffer_overlap_costs
             )
  }
  else{
    data.frame("num" = num,
               "jobs" = get_job_order_string(perm$jobs),
               "time_taken" = perm$time_taken,
               "costs" = perm$buffer_overlap_costs
               )
  }
}

add_permutation_to_dataframe = function(df, perm, num){
  perm_df <- permutation_to_dataframe(perm, num)
  
  return(rbind(df, perm_df))
}

permutation_list_to_dataframe = function(perms){
  df <- data.frame()
  
  for(i in 1: length(perms)){
    perm <- perms[[i]]
    df <- add_permutation_to_dataframe(df, perm, i)
  }
  
  return(df)
}

sizes_to_string = function(sizes){
  s <- paste(sizes, collapse = ', ', sep="")
  s <- paste("(", s, ")", sep="")
  return(s)
}

sizes_to_string(list(5))

buffer_to_dataframe = function(buffer){
  cr <- NULL
  if(!is.null(buffer$cr_seed)){
    cr <- buffer$cr_seed
    return(data.frame("seed" = buffer$seed,
                      "min_size" = buffer$min_size,
                      "max_size" = buffer$max_size,
                      "change_rate" = cr,
                      "min_change" = buffer$min_change,
                      "max_change" = buffer$max_change,
                      "predetermined_sizes" = sizes_to_string(buffer$passed_sizes),
                      "intensity" = sizes_to_string(as.list(buffer$intensity)),
                      "intensity_scaling_min" = buffer$intensity_scaling[[1]],
                      "intensity_scaling_max" = buffer$intensity_scaling[[2]],
                      "span" = buffer$span
    ))
  }
  else{
    cr <- buffer$change_rate
  }
  return(data.frame("seed" = buffer$seed,
             "min_size" = buffer$min_size,
             "max_size" = buffer$max_size,
             "change_rate" = sizes_to_string(cr),
             "min_change" = buffer$min_change,
             "max_change" = buffer$max_change,
             "predetermined_sizes" = sizes_to_string(buffer$passed_sizes),
             "intensity" = sizes_to_string(as.list(buffer$intensity)),
             "intensity_scaling_min" = buffer$intensity_scaling[[1]],
             "intensity_scaling_max" = buffer$intensity_scaling[[2]],
             "span" = buffer$span
             ))
}

make_perm_to_string = function(perm, buffer = perm$buffer){
  str = ""
  for(job in perm$jobs){
    str <- paste(str, paste(as.character(job$time_machine_one), as.character(job$time_machine_two), as.character(job$size), sep = "_"))
  }
  
  str <- paste(str, as.character(buffer$seed), as.character(buffer$min_size), as.character(buffer$max_size), as.character(buffer$min_change), as.character(buffer$max_change), as.character(buffer$change_rate), sep = "_")
  
  for(size in buffer$local_sizes){
    str <- paste(str, as.character(size), "_")
  }

  return(str)
}

get_id_for_string = function(str){
  digest(str, algo = "sha256")
}

get_id_for_permutation = function(perm){
  perm_string <- make_perm_to_string(perm)
  
  return(get_id_for_string(perm_string))
}

write_dataframe_to_xml = function(df, name){
  write.xml(df, file = paste("./", name, sep=""))
}

save_new_buffer_xml = function(seed){
  instances <- Instances$new(c = 20, seed=seed)
  
  for(perm in instances$permutations){
    new_buffer <- buffer_to_dataframe(perm$buffer)
    id <- perm$get_instance_id()
    path <- paste("calculated_instances/", seed, sep="")
    d_path <- paste("calculated_instances/d-", seed, sep="")
    file <- paste(path, "/", id, "/buffer.xml", sep="")
    d_file <- paste(d_path, "/", id, "/buffer.xml", sep="")
    
    if(dir.exists(path)){
    write_dataframe_to_xml(df = new_buffer, name = file)
    }
    if(dir.exists(d_path)){
      write_dataframe_to_xml(df = new_buffer, name = d_file)
    }
  }
}

calc_and_save_permutation_results = function(perm, id = NULL, directory = "calculated_FS"){
  
  if(is.null(id)){
    id <- get_id_for_string(make_perm_to_string(perm, perm$buffer))
  }
  else{
    id <- id
  }
  
  path <- paste(directory, "/", id, sep = "")
  d_path <- sub("/", "/d-", path)

  if(dir.exists(path) || dir.exists(d_path)){
    print(paste("Path: ", path, " already exists", sep = ""))
    return()
  }
  
  if(!dir.exists(directory)){
    dir.create(directory)
  }
  perms <- calc_perm_list_for_perm(perm, perm$buffer)
  
  jobs_df <- job_list_to_dataframe(perm$jobs)
  buffer_df <- buffer_to_dataframe(perm$buffer)
  perms_df <- permutation_list_to_dataframe(perms)
  
  dir.create(paste(directory, "/", id, sep = ""))
  
  jobs_path <- paste(directory, "/", id, "/jobs.xml", sep="")
  buffer_path <- paste(directory, "/", id, "/buffer.xml", sep="")
  perms_path <- paste(directory, "/", id, "/permutation_results.xml", sep="")
  
  if(!is.null(perm$instance_data)){
    instance_data_path <- paste(directory, "/", id, "/instance_data.xml", sep="")
    write_dataframe_to_xml(perm$instance_data, instance_data_path)
  }
  
  write_dataframe_to_xml(jobs_df, jobs_path)
  write_dataframe_to_xml(buffer_df, buffer_path)
  write_dataframe_to_xml(perms_df, perms_path)
  
  print(id)
}

find_pareto_front_in_dataframe = function(df){
  D = df[order(df$time_taken, df$costs, decreasing = FALSE),]
  front = D[which(!duplicated(cummin(D$costs))),]
}

get_distance_point_to_point = function(point1, point2){
  x_1 <- point1[[1]]
  y_1 <- point1[[2]]
  x_2 <- point2[[1]]
  y_2 <- point2[[2]]
  
  sqrt((x_1 - x_2)^2 + (y_1 - y_2)^2)
}

find_Data_for_job_order = function(dfc, jobs){
  job_string <- job_dataframe_to_string(jobs)
  return(subset(dfc$permutations_dataframe, (jobs == job_string)))
}

#höhe dreieck
get_distance_and_intersection_to_pareto_front = function(point, pareto_front){
  x <- point[[1]]
  y <- point[[2]]
  smallest <- NULL
  return_data <- list()
  
  if(nrow(pareto_front) == 1){
    x_pareto <- pareto_front[1, "time_taken"]
    y_pareto <- pareto_front[1, "costs"]
    
    return_data <- list(get_distance_point_to_point(point, list(x_pareto, y_pareto)), list(x_pareto, y_pareto))
  }
  else{
    x_0 <- point[[1]]
    y_0 <- point[[2]]
    for(i in 2:nrow(pareto_front)){
      x_1 <- pareto_front[i, "time_taken"]
      y_1 <- pareto_front[i, "costs"]
      x_2 <- pareto_front[i-1, "time_taken"]
      y_2 <- pareto_front[i-1, "costs"]
      
      if(x_1 == x_2){
        x_F <- x_1
        y_F <- y_0
      }
      else{
        m <- (y_2 - y_1)/ (x_2 - x_1)
        if(m == 0){
          y_F <- y_1
          x_F <- x_0
        }
        else{
          m_0 <- -1/m
          n <- y_1 - m*x_1 
          n_0 <- y_0 - m_0*x_0
          
          x_F <- (n_0 - n)/(m - m_0)
          y_F <- m*x_F + n
        }
      }
      
      #switch points P_1 and P_2 if P_1 would be on the right side of P_2
      if(x_1 > x_2){
        x_temp <- x_1
        y_temp <- y_1
        x_1 <- x_2
        y_1 <- y_2
        x_2 <- x_temp
        y_2 <- y_temp
      }
      
      if(x_F > x_2){
        x_F <- x_2
        y_F <- y_2
        h_c <- get_distance_point_to_point(list(x_2, y_2), list(x_0, y_0))
      }
      else if(x_F < x_1){
        x_F <- x_1
        y_F <- y_1
        h_c <- get_distance_point_to_point(list(x_1, y_1), list(x_0, y_0))
      }
      else{
        h_c <- get_distance_point_to_point(list(x_F, y_F), list(x_0, y_0))
      }
      
      if(!is.null(smallest)){
        if(h_c < smallest){
          return_data <- list(h_c,list(x_F, y_F))
        }
      }
      else{
        return_data <- list(h_c,list(x_F, y_F))
      }
    }
  }
return(return_data)
}

get_distance_to_pareto_front = function(point, pareto_front){
  get_distance_and_intersection_to_pareto_front(point, pareto_front)[[1]]
}


get_arrow_dataframe = function(pareto_front, algo){
  solutions <- algo$get_algorithm_information()
  x_pareto_intersection <- c()
  y_pareto_intersection <- c()
  distance <- c()
  for(row in 1:nrow(solutions)){
    point <- list(solutions[row, "time_taken"], solutions[row, "cost"])
    d <- get_distance_and_intersection_to_pareto_front(point, pareto_front)[[1]]
    pareto_point <- get_distance_and_intersection_to_pareto_front(point, pareto_front)[[2]]
    
    x_pareto <- pareto_point[[1]]; y_pareto <- pareto_point[[2]]
    
    x_pareto_intersection <- c(x_pareto_intersection, x_pareto)
    y_pareto_intersection <- c(y_pareto_intersection, y_pareto)
    distance <- c(distance, d)
  }
  
  cbind(solutions, x_pareto_intersection, y_pareto_intersection, distance)
}
#arrow dataframe generieren!! https://stackoverflow.com/questions/38008863/how-to-draw-a-nice-arrow-in-ggplot2

range_to_string = function(range){
  return(paste(range[[1]], "u", range[[2]], sep=""))
}

get_average_size_from_jobs = function(jobs){
  size = 0
  
  for(job in jobs){
    size <- size + job$size
  }
  
  return(size/length(jobs))
}

get_average_time_m1_from_jobs = function(jobs){
  time = 0
  
  for(job in jobs){
    time <- time + job$time_machine_one
  }
  
  return(time/length(jobs))
}

get_longest_job_time_on_m1 = function(jobs){
  max_time <- 0
  
  for(job in jobs){
    if(job$time_machine_one > max_time) max_time <- job$time_machine_one
  }
  
  return(max_time)
}

get_biggest_job_size_on_m1 = function(jobs){
  max_size <- 0
  
  for(job in jobs){
    if(job$size > max_size) {max_size <- job$size}
  }
  
  return(max_size)
}

get_second_smallest_job_size_on_m1 = function(jobs)  {
  sizes = c()
  
  for(job in jobs) {
    sizes = c(sizes, job$size)
  }
  print(sizes)
  sizes = sort(sizes, decreasing = FALSE)
  
  return(sizes[[2]])
  
}

gen_jobs = function(c, seed, variation_range, num_of_jobs){
  set.seed(seed)
  jobs <- list()
  sumed_up_time <- 0
  for(i in 1:num_of_jobs){
    runtime1 <- floor(runif(1, min = (1-variation_range[[2]]) * c, max = (1-variation_range[[1]]) * c + 1))
    runtime2 <- floor(runif(1, min = (1+variation_range[[1]]) * c, max = (1+variation_range[[2]]) * c + 1))
    
    runtime <- sample(c(runtime1, runtime2), 1)
    jobs <- append(jobs, Job$new(
      time_machine_one = runtime, 
      time_machine_two = 0, 
      size = runtime, 
      name = paste(r'($J_{)', i, r'(}$)', sep = r'()')))
    
    sumed_up_time <- sumed_up_time + runtime
  }
  average_runtime_one <- sumed_up_time/length(jobs)
  
  for(job in jobs){
    job$time_machine_two = round(average_runtime_one)
  }
  
  return(jobs)
}

gen_buffer = function(c, intensity_scaling, change_rate_variation, seed, jobs, min_size = 1, max_size, span = TRUE){
  
  average_size_jobs <- get_average_size_from_jobs(jobs)
  average_time_jobs <- get_average_time_m1_from_jobs(jobs)
  
  return(Buffer$new(
    seed = seed,
    min_size = min_size,
    max_size = max_size,
    change_rate = list(round(c * change_rate_variation)),
    intensity_scaling = intensity_scaling,
    span = span
  ))
  
}

evaluate_to_color = function(cr_pos, int_pos){
  if(cr_pos == 1){
    if(int_pos == 1){
      return("red2")
    }
    if(int_pos == 2){
      return("red3")
    }
    if(int_pos == 3){
      return("red4")
    }
  }
  if(cr_pos == 2){
    if(int_pos == 1){
      return("blue2")
    }
    if(int_pos == 2){
      return("blue3")
    }
    if(int_pos == 3){
      return("blue4")
    }
  }
  if(cr_pos == 3){
    if(int_pos == 1){
      return("green2")
    }
    if(int_pos == 2){
      return("green3")
    }
    if(int_pos == 3){
      return("green4")
    }
  }
  
  return("white")
}

strip_to_id_string = function(d){
  d_ <- gsub("[[:punct:]]", "", as.character(d))
  
  return(d_)
}

get_consecutive_numbers_in_string = function(str){
  nums <- list()
  curr_num <- ""
  for(i in 1:nchar(str)){
    curr_char <- substr(str, i, i)
    if(is.na(as.numeric(curr_char))){
      if(curr_num != ""){
        while(TRUE){
          if(nchar(curr_num) >= 3) break
          curr_num <- paste(curr_num, "0", sep = "")
        }
        nums <- append(nums, curr_num)
        curr_num <- ""
      }
    }
    else{
      curr_num <- paste(curr_num, curr_char, sep = "")
    }
    if(i == nchar(str)){
      if(curr_num != ""){
        while(TRUE){
          if(nchar(curr_num) >= 3) break
          curr_num <- paste(curr_num, "0", sep = "")
        }
        nums <- append(nums, curr_num)
        curr_num <- ""
      }
      nums <- append(nums, curr_num)
    }
  }
  dummy_nums <- list()
  
  for(num in nums){
    dummy_str <- num
    if(substr(num, 1, 1) == 0){
      dummy_str <- substr(num, 2, nchar(num))
    }
    dummy_nums <- append(dummy_nums, dummy_str)
  }
  return(dummy_nums)
}

get_data_string_from_path = function(path, job = TRUE, cr = TRUE, int = TRUE){
  ret <- ""
  substr_path <- substr(path, gregexpr("jobvar", path), nchar(path))
  nums <- get_consecutive_numbers_in_string(substr_path)
  
  if(job){
    ret <- paste(ret, "job",nums[[1]], "%-", nums[[2]], "% ", sep="")
  }
  if(cr){
    ret <- paste(ret, "cr", nums[[3]], "% ", sep="")
  }
  if(int){
    ret <- paste(ret, "vol", nums[[4]], "%", sep="")
  }
  
  if(ret != ""){
    return(ret)
  }
  return("invalid path")
}

start_calculating_instances = function(c = 20, seed = 42, directory = "calculated_instances", span = TRUE){
  instances = Instances$new(c = c, seed = seed, span = span)
  direct = paste(directory ,"/", seed, sep="")
  d_dir <- sub("/", "/d-", direct)
  
  if(!dir.exists(direct) & !dir.exists(d_dir)) dir.create(direct)
  
  for(perm in instances$permutations){
    id <- perm$get_instance_id()
    start.time <- Sys.time()
    calc_and_save_permutation_results(perm = perm, id = id, directory = direct)
    end.time <- Sys.time()
    
    print(paste("time taken:", as.numeric(end.time - start.time, units = "mins"), sep=""))
  }
}

start_calculating_isolated_instances = function(c = 20, seed = 42, directory = "calculated_instances", span = TRUE){
  instances = Instances$new(c = c, seed = seed, span = span)
  direct = paste(directory ,"/", seed, sep="")
  d_dir <- paste(directory, "/d-", seed, sep= "")
  
  if(dir.exists(d_dir)) {
    return();
  }
    
  if(!dir.exists(direct)) dir.create(direct)
  
  for(perm in instances$permutations){
    id <- perm$get_instance_id()
    start.time <- Sys.time()
    calc_and_save_permutation_results(perm = perm, id = id, directory = direct)
    end.time <- Sys.time()
    
    print(paste("time taken:", as.numeric(end.time - start.time, units = "mins"), sep=""))
  }
}

get_divergence_from_point = function(range, scale, min = 1){
  mid <- range[[1]]
  max <- range[[2]]
  
  sorted_scale <- sort(unlist(scale))
  
  if(sorted_scale[1] == sorted_scale[2]) scaling <- sorted_scale[1]
  else scaling <- runif(1, sorted_scale[1], sorted_scale[2])
  
  sizeUp <- floor(runif(1, min = (1-sorted_scale[[2]]) * mid, max = (1-sorted_scale[[1]]) * mid + 1))
  sizeLow <- floor(runif(1, min = (1+sorted_scale[[1]]) * mid, max = (1+sorted_scale[[2]]) * mid + 1))
  
  divergence <- sample(c(sizeUp, sizeLow), 1)
  
  return(divergence)
}

calculate_buffer_size_so_that_given_size_definitely_fits = function(size, max_volatility) {
  return(ceiling((2*size)/(1+max_volatility)))
}

replace_specific_values_in_column = function(df, colname, prev, new) {
  df[[colname]] <- ifelse(df[[colname]] == prev, new, df[[colname]])
  return(df)
}
