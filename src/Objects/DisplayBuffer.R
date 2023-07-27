# THIS IS FOR DISPLAYING ONLY
# Buffer class
# It holds its min and max size as well as the buffersizes for every timestep
DisplayBuffer <- R6Class("Buffer",
                         public = list(
                           seed = NULL,
                           min_size = NULL,
                           max_size = NULL,
                           change_rate = NULL,
                           len = NULL,
                           local_sizes = list(),
                           working_time = 0,
                           
                           initialize = function(buffer){
                             self$seed <- buffer$seed
                             self$min_size <- buffer$min_size
                             self$max_size <- buffer$max_size
                             self$change_rate <- buffer$change_rate
                             self$len <- buffer$len
                             self$local_sizes <- buffer$local_sizes
                             self$working_time <- buffer$working_time
                           }
                         )
)