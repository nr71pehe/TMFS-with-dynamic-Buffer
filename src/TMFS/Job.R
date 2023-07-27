Job <- R6Class("Job",
               public= list(
                   time_machine_one = NULL,
                   time_machine_two = NULL,
                   size = NULL,
                   name = NULL,
                   initialize = function(time_machine_one, time_machine_two, size, name){
                       self$time_machine_one <- time_machine_one
                       self$time_machine_two <- time_machine_two
                       self$size <- size
                       self$name <- name
                   },
                   set_time_machine_one = function(time){
                       self$time_machine_one <- time
                   },
                   set_time_machine_two = function(time){
                       self$time_machine_two <- time
                   },
                   to_Block = function(label, start_time, end_time){
                     Block$new(label = label,
                               start_time = start_time,
                               end_time = end_time,
                               size = self$size)
                   }
               ))
