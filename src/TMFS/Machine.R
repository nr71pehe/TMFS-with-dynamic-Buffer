Machine <- R6Class("Machine",
                   public = list(
                     blocks = list(),
                     blockage_blocks = NULL,
                     occupying_block = NULL,
                     is_occupied = FALSE,
                     actual_end_times = list(),
                     
                     set_occupying_block = function(block){
                       self$occupying_block <- block
                     },
                     
                     add_block = function(block){
                       self$blocks <- append(self$blocks, block)
                     },
                     
                     add_set_block = function(block){
                       self$blocks <- append(self$blocks, block)
                       self$set_occupying_block(block)
                     },
                     
                     is_done = function(t){
                       self$occupying_block$end_time <= t
                     },
                     
                     set_is_occupied = function(x){
                       self$is_occupied <- x
                     },
                     
                     can_accept_block = function(){
                       (!(self$is_occupied) & is.null(self$occupying_block))
                     },
                     
                     clear = function(){
                       self$occupying_block <- NULL
                     },
                     
                     get_block_at_t = function(t){
                       for(block in self$blocks){
                         if(block$start_time <= t & block$end_time > t){
                           return(block)
                           }
                       }
                       return(NULL)
                     },
                     
                     add_actual_end_time = function(t){
                       self$actual_end_times <- append(self$actual_end_times, t)
                     },
                     
                     get_actual_end_time = function(i){
                      return(self$actual_end_times[[i]]) 
                     },
                     
                     print_blocks = function(length){
                       machine_string <- ""
                       for(t in 1 : length ){
                         block <- self$get_block_at_t(t)
                         if(!is.null(block)){
                           machine_string <- paste(machine_string, block$label,",", sep = "")
                         }
                         else{
                           machine_string <- paste(machine_string, "_x_,", sep = "")
                         }
                       }
                       print(machine_string)
                     },
                     
                     add_blockage = function(start, end){
                       self$blockage_blocks <- append(self$blockage_blocks, Block$new(start_time = start, end_time = end))
                     }
                   ))
