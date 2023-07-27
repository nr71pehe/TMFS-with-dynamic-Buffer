Buffer <- R6Class("Buffer",
                  public = list(
                    seed = NULL,
                    min_size = NULL,
                    max_size = NULL,
                    change_rate = NULL,
                    min_change = NULL,
                    max_change = NULL,
                    cr_seed = NULL,
                    len = NULL,
                    local_sizes = list(),
                    blocks = list(),
                    stored_blocks = list(),
                    precalculated_local_sizes = list(),
                    working_time = 0,
                    passed_sizes = list(),
                    intensity = NULL,
                    intensity_scaling = NULL,
                    span = NULL,

                    initialize = function(seed = 101, min_size = 1,  max_size = 5, change_rate = list(1), min_change = 2, max_change = 5, len = 1000, intensity = list(), sizes = list(), intensity_scaling = 1.0, span = TRUE){
                      self$seed <- seed
                      self$min_size <- min_size
                      self$max_size <- max_size
                      self$min_change <- min_change
                      self$max_change <- max_change
                      cr_seed <- change_rate
                      self$precalculated_local_sizes <- sizes
                      self$passed_sizes <- sizes
                      self$intensity <- intensity
                      self$intensity_scaling <- intensity_scaling
                      self$span <- span

                      if(!is.list(change_rate)){
                        self$cr_seed <- change_rate
                      }

                      if(self$consistency_check(sizes, max_size) == FALSE){
                        print("given sizes get too big")
                        return()
                      }
                      
                      for(inten in intensity){
                        if(inten > (max_size - min_size)){
                          print("buffer-intensity is too big to stay in [min_size, max_size]")
                          return()
                        }
                      }

                      if(is.list(change_rate)){
                        self$change_rate <- change_rate
                      }
                      else{
                        self$change_rate <- randomify_change_rate(cr_seed, self$min_change, self$max_change)
                      }

                      self$continue_local_sizes(start = length(sizes) + 1, end = len)
                      self$len <- len
                    },

                    calc_buff_size_at_t = function(t){
                      if(t < length(self$precalculated_local_sizes)){
                        return(unlist(self$precalculated_local_sizes[t]))
                      }
                      else{
                        gen_local_sizes(self$seed, self$min_size, self$max_size, self$change_rate, t, self$intensity, intensity_scaling = self$intensity_scaling)[[t]]
                      }
                    },

                    #Block logic

                    get_occupied_space = function(){
                      occupied_space <- 0
                      for(block in self$stored_blocks){
                        occupied_space <- occupied_space + block$size
                      }
                      return(occupied_space)
                    },

                    #REVERSED STACK
                    put_stored_block = function(block){
                      self$stored_blocks <- append(list(block), self$stored_blocks)
                    },

                    pop_stored_block = function(){
                      popped_block <- self$stored_blocks[[length(self$stored_blocks)]]
                      self$stored_blocks <- head(self$stored_blocks, -1)
                      if(self$len < popped_block$end_time){
                        self$len <- popped_block$end_time
                      }

                      return(popped_block)
                    },
                    
                    return_top_stored_block = function(){
                      popped_block <- self$stored_blocks[[length(self$stored_blocks)]]
                      
                      return(popped_block)
                    },

                    add_block = function(block){
                      self$put_stored_block(block)
                      self$local_sizes <- gen_local_sizes(self$seed, self$min_size, self$max_size, self$change_rate, self$len, self$intensity)
                    },

                    has_no_stored_blocks = function(){
                      length(self$stored_blocks) == 0
                    },

                    can_accept_block = function(block, t){
                      occupied_space <- self$get_occupied_space()
                      return(block$size <= (self$calc_buff_size_at_t(t) - occupied_space))
                    },

                    set_local_sizes_to_t = function(t){
                      self$local_sizes <- gen_local_sizes(self$seed, self$min_size, self$max_size, self$change_rate, t)
                    },

                    print_local_sizes = function(){
                      sizes_string <- ""

                      for(size in self$local_sizes){
                        sizes_string <- paste(sizes_string, "_", as.character(size), "_,", sep="")
                      }
                      print(sizes_string)
                    },

                    get_overlap_cost_at_time = function(time){
                      occupied_space <- self$get_occupied_space()
                      current_buffer_size <- self$calc_buff_size_at_t(time)

                      if(occupied_space > current_buffer_size){
                        return(occupied_space - current_buffer_size)
                      }
                      else{
                        return(0)
                      }
                    },

                    continue_local_sizes = function(start = 1, end){
                      sizes <- list()
                      seed <- self$seed
                      min_size <- self$min_size
                      max_size <- self$max_size
                      change_rate <- self$change_rate
                      precalc_sizes <- self$precalculated_local_sizes
                      intensity <- self$intensity
                      intensity_scaling <- self$intensity_scaling
                      
                      if(end <= length(precalc_sizes)){

                        if(start == 1){
                          sizes <- precalc_sizes
                          }

                        else{
                          for(i in start : end){
                            print(i)
                            sizes <- append(sizes, precalc_sizes[[i]])
                          }
                        }
                        self$local_sizes <- sizes
                      }
                      else{
                        sizes <- precalc_sizes
                        loc_sizes <- gen_local_sizes(seed, min_size, max_size, change_rate, end, intensity, intensity_scaling = intensity_scaling)
                        
                        for(i in (length(precalc_sizes) + 1) : end){
                          sizes <- append(sizes, loc_sizes[[i]])
                        }
                        self$local_sizes <- sizes
                      }
                    },

                    consistency_check = function(sizes, max){
                      for(size in sizes){
                        if(size > max) return(FALSE)
                      }
                      return(TRUE)
                    }

                  ))

randomify_change_rate = function(seed, min_change, max_change, len = 100){
  set.seed(seed)
  change_rate <- list()
  while (TRUE) {
    if(sum(unlist(change_rate)) >= len){
      return(change_rate)
    }else{
      change_rate <- c(change_rate, floor(runif(1, min = min_change, max = max_change + 1)))
    }
  }
}

#min_size, max_size geben intervall, um wie viel sich etwas ändern kann. runif darf nicht =n sein, wenn -min_size < n < min_size aber muss
#-max_size <= n <= max_size
gen_local_sizes = function(seed, min_size, max_size, change_rate, max_len, intensity = list(1), intensity_scaling = list(0.25, 0.5)){

  sizes <- list()
  time_intervals <- Interval_list$new()
  new_cr <- change_rate
  
  time_intervals$list_to_intervals(new_cr)
  intensity_scaling_is_list = is.list(intensity_scaling)
  middle_of_buffer <- max_size/2
  set.seed(seed)
  
  if(intensity_scaling_is_list){
    current_time <- get_divergence_from_point(range = list(middle_of_buffer, max_size), scale = intensity_scaling)
  }
  else{
    current_time <- floor(runif(1, min = min_size, max = max_size + 1))
  }
  
  if(length(time_intervals$intervals) == 0 | min_size == max_size){
    for(t in 1 : max_len){
      sizes[t] <- max_size
    }
  }
  else{
    for(t in 1 : max_len){
      if(time_intervals$switch_interval()){
        if(length(sizes) != 0){
          prev_time <- sizes[[t-1]]
        }
        else{
          prev_time <- 0
        }
        while(TRUE){
          if(intensity_scaling_is_list){
            current_time <- get_divergence_from_point(range = list(middle_of_buffer, max_size), scale = intensity_scaling)
            time_intervals$make_step()
            break
          }
          else{
            change <- 0
            change <- round(floor(runif(1, min = -max_size, max = max_size + 1)) * intensity_scaling)
            
            size_to_be <- change + prev_time
            if(size_to_be == prev_time) next
            
            if(min_size != 0) if(size_to_be < min_size) next
            
            if(size_to_be > max_size) next
            
            if(!length(intensity) == 0) if(!(abs(change) %in% intensity))next
            current_time <- size_to_be
            time_intervals$make_step()
            break
          }
        }
      }
      time_intervals$make_step()
      sizes[t] <- current_time
    }
  }
  sizes
}