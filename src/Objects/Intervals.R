Interval <- R6Class("Interval",
                    public = list(
                        size = NULL,
                        step = 1,
                        start_time = NULL,
                        initialize = function(size = 1, start_time = 1){
                            self$size = size
                        },
                        is_used_up = function(){
                            return(step > size)
                        },
                        reset_interval = function(){
                            self$step = 1
                        }
                    )
)

Interval_list <- R6Class("Interval_list",
                         public = list(
                             intervals = list(),
                             current_interval = 1,
                             add_interval = function(size){
                                 
                                 if(size > 0){
                                     self$intervals <- c(self$intervals, Interval$new(size))
                                 }
                             },
                             list_to_intervals = function(cr_list){
                                 for(i in 1 : length(cr_list)){
                                     self$add_interval(cr_list[[i]])
                                 }
                             },
                             return_current_interval = function(){
                                 return(self$intervals[[self$current_interval]])
                             },
                             return_current_interval_size = function(){
                                 return(self$intervals[[self$current_interval]]$size)
                             },
                             # takes one step through the interval list
                             # returns the interval size after the step
                             make_step = function(){
                                 curr_step <- self$intervals[[self$current_interval]]$step
                                 curr_size <- self$intervals[[self$current_interval]]$size
                                 
                                 #next_interval
                                 if((curr_step) > curr_size){
                                     self$intervals[[self$current_interval]]$reset_interval()
                                     self$current_interval <- self$current_interval + 1
                                     
                                     # loop list
                                     if(self$current_interval > length(self$intervals)){
                                         self$current_interval <- 1
                                     }
                                 }
                                 else{
                                     self$intervals[[self$current_interval]]$step <- curr_step + 1
                                 }
                                 
                                 self$intervals[[self$current_interval]]$size
                             },
                             switch_interval = function(){
                                 curr_interval <- self$return_current_interval()
                                 if(curr_interval$step > (curr_interval$size)){
                                     return(TRUE)
                                 }
                                 else{
                                     return(FALSE)
                                 }
                             }
                         )
)

sizes_to_intervals <- function(sizes){
    interval_list <- Interval_list$new()
    
    for(i in 1 : length(sizes)){
        interval_list$add_interval(sizes[[i]])
    }
    return(interval_list)
}
