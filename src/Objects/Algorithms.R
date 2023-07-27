 #contains NEH, ordered_by_time_increasing, ordered_by_time_decreasing, ordered_by_size_increasing, ordered_by_size_decreasing

Algorithms = R6Class("Algorithms",
                     public = list(
                       jobs = NULL,
                       dfc = NULL,
                       algorithm_info = NULL,
                       solution_neh = NULL,
                       solution_ordered_by_time_increasing = NULL,
                       solution_ordered_by_time_decreasing = NULL,
                       solution_ordered_by_size_increasing = NULL,
                       solution_ordered_by_size_decreasing = NULL,

                       algorithm_color_scheme = c("OT-I"="blue", 
                                                   "OT-D"="green", 
                                                   "OS-I"="burlywood", 
                                                   "OS-D"="violetred3"),
                       
                       initialize = function(dfc){
                         self$jobs <- dfc$job_dataframe
                         self$dfc <- dfc
                         self$calculate_all_algorithms()
                       },
                       
                       ordered_by_time_increasing = function(){
                         jobs <- self$jobs
                         ordered_jobs <- jobs[order(jobs$time_on_m1, jobs$time_on_m2),]
                         
                         self$solution_ordered_by_time_increasing <- find_Data_for_job_order(self$dfc, ordered_jobs)
                         
                         return(self$solution_ordered_by_time_increasing)
                       },
                       
                       ordered_by_time_decreasing = function(){
                         jobs <- self$jobs
                         
                         ordered_jobs <- jobs[order(-jobs$time_on_m1, -jobs$time_on_m2),]
                         
                         self$solution_ordered_by_time_decreasing <- find_Data_for_job_order(self$dfc, ordered_jobs)
                         
                         return(self$solution_ordered_by_time_decreasing)
                       },
                       
                       ordered_by_size_increasing = function(){
                         jobs <- self$jobs
                         
                         ordered_jobs <- jobs[order(jobs$size),]
                         
                         self$solution_ordered_by_size_increasing <- find_Data_for_job_order(self$dfc, ordered_jobs)
                         
                         return(self$solution_ordered_by_size_increasing)
                       },
                       
                       ordered_by_size_decreasing = function(){
                         jobs <- self$jobs
                         
                         ordered_jobs <- jobs[order(-jobs$size),]
                         
                         self$solution_ordered_by_size_decreasing <- find_Data_for_job_order(self$dfc, ordered_jobs)
                         
                         return(self$solution_ordered_by_size_decreasing)
                       },
                       
                       calculate_all_algorithms = function(){
                         self$ordered_by_time_increasing()
                         self$ordered_by_time_decreasing()
                         self$ordered_by_size_increasing()
                         self$ordered_by_size_decreasing()
                         
                         ti <- self$solution_ordered_by_time_increasing
                         td <- self$solution_ordered_by_time_decreasing
                         si <- self$solution_ordered_by_size_increasing
                         sd <- self$solution_ordered_by_size_decreasing

                         self$algorithm_info <- data.frame(algorithm = c( "OT-I", 
                                                                     "OT-D", 
                                                                     "OS-I", 
                                                                     "OS-D"),
                                                           
                                                           time_taken = c(ti[1, "time_taken"],
                                                                          td[1, "time_taken"],
                                                                          si[1, "time_taken"],
                                                                          sd[1, "time_taken"]),
                                                           
                                                           cost = c(ti[1, "costs"],
                                                                    td[1, "costs"],
                                                                    si[1, "costs"],
                                                                    sd[1, "costs"]),
                                                           
                                                           color = c("blue",
                                                                     "green",
                                                                     "burlywood",
                                                                     "darkslategrey")
                         )
                       },
                       
                       get_algorithm_information = function(){
                         self$algorithm_info
                       }
                     ))

