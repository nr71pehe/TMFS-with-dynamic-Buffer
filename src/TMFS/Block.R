#to be expected labels: "blank", "x", "J_n" (where n is 1..num_of_jobs)
Block <- R6Class("Block",
                 public = list(
                     label = NULL,
                     start_time = NULL,
                     end_time = NULL,
                     size = NULL,
                     assigned_job = NULL,
                     initialize = function(label = "", start_time, end_time, size = 0){
                       self$label <- label
                       self$start_time <- start_time
                       self$end_time <- end_time
                       self$size <- size
                     },
                     
                     assign_job = function(job){
                       self$assigned_job <- job
                       self$size <- job$size
                     },
                     
                     get_time_for_m1 = function(){
                       self$assigned_job$time_machine_one
                     },
                     
                     get_time_for_m2 = function(){
                       self$assigned_job$time_machine_two
                     },
                     
                     adjust_start_time = function(t){
                       self$start_time <- t
                     },
                     
                     adjust_end_time = function(t){
                       self$end_time <- t
                     },
                     
                     adjust_start_end_time = function(start, end){
                       self$adjust_start_time(start)
                       self$adjust_end_time(end)
                     },
                     
                     set_label = function(label){
                       self$label <- label
                     },
                     
                     is_in_interval = function(x, y) {
                       if(self$start_time <= x & self$end_time <= x) return(FALSE)
                       if(self$start_time >= y & self$end_time >= y) return(FALSE)
                       return(TRUE)
                     }
                 ))

get_blockage_block <- function(start, end){
  Block$new(label = "x", start_time = start, end_time = end, 1)
}

get_blank_block <- function(start, end){
  Block$new(label = "blank", start_time = start, end_time = end, 0)
}
