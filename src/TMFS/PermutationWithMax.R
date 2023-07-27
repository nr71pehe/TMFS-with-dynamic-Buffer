PermutationWithMax <- R6Class("PermutationWithMax",
                       public = list(
                         jobs = c(),
                         m1 = Machine$new(),
                         m2 = Machine$new(),
                         m1_blocks = NULL,
                         buffer = NULL,
                         time_taken = 0,
                         instance_data = NULL,
                         max_time = 100,
                         
                         buffer_blocks = list(),
                         buffer_overlap_costs = 0,
                         
                         initialize = function(buffer = NULL, jobs = list(), instance_data = NULL){
                           self$buffer <- buffer
                           self$jobs <- jobs
                           self$instance_data <- instance_data
                         },
                         
                         add_job = function(job){
                           self$jobs <- append(self$jobs, job)
                         },
                         
                         swap_jobs = function(i, j){
                           self$jobs <- replace(self$jobs,
                                                c(i, j),
                                                self$jobs[c(j, i)])
                         },
                         
                         make_time_machine_one_constant = function(time){
                           for(job in self$jobs){
                             job$time_machine_one <- time
                           }
                         },
                         
                         make_time_machine_two_constant = function(time){
                           for(job in self$jobs){
                             job$time_machine_two <- time
                           }
                         },
                         
                         set_jobs = function(job_order){
                           self$jobs <- job_order
                         },
                         
                         set_buffer = function(buffer){
                           self$buffer <- buffer
                         },
                         
                         calc_time_with_buffer = function(buffer = self$buffer){
                           biggest_size <- 0
                           overlap_costs <- 0
                           
                           #check if buffer can be big enough
                           for(job in self$jobs){
                             if (biggest_size < job$size){
                               biggest_size <- job$size
                             }
                           }
                          
                           i <- 1
                           m1 <- Machine$new()
                           m1_blockage_time <- 0
                           m2 <- Machine$new()
                           global_time <- 1
                           while(TRUE){
                             any_has_traded <- FALSE
                             
                             #trade jobs to m1
                             if(i <= length(self$jobs)){
                               to_be_added_job <- self$jobs[[i]]
                               label <- paste(to_be_added_job$name)
                               to_be_added_block <- to_be_added_job$to_Block(label, global_time, global_time + to_be_added_job$time_machine_one)
                               to_be_added_block$assign_job(to_be_added_job)
                               
                               if(m1$can_accept_block()){
                                 m1$add_set_block(to_be_added_block)
                                 m1$set_is_occupied(TRUE)
                                 any_has_traded <- TRUE
                                 i <- i + 1
                               }
                             }
                             
                             #trade m1 straight to m2
                             if(m1$is_occupied){
                               if(m1$is_done(global_time) && !m2$is_occupied){
                                 #prepare block from m1
                                 m1_current_block <- m1$occupying_block
                                 m1$add_actual_end_time(global_time)
                                 m1$add_blockage(start = m1_current_block$end_time, end = global_time+1)
                                 m1$clear()
                                 m1$is_occupied <- FALSE
                                 any_has_traded <- TRUE
                                 
                                 #attach to m2
                                 m1_current_block_job <- m1_current_block$assigned_job
                                 to_be_added_block <- m1_current_block_job$to_Block(m1_current_block$label, global_time, global_time + m1_current_block_job$time_machine_two)
                                 
                                 
                                 m2$add_set_block(to_be_added_block)
                                 m2$set_is_occupied(TRUE)
                                 any_has_traded <- TRUE
                               }
                             }
                             #trade m1 to buffer
                             if(m1$is_occupied){
                               if(m1$is_done(global_time)){
                                 m1_current_block <- m1$occupying_block
                                 if(buffer$can_accept_block(m1_current_block, global_time)){
                                   m1$add_actual_end_time(global_time)
                                   m1$add_blockage(start = m1_current_block$end_time, end = global_time+1)
                                   buffer$add_block(m1_current_block)
                                   m1$clear()
                                   m1$is_occupied <- FALSE
                                   any_has_traded <- TRUE
                                 }
                               }
                             }
                             m1_blocks = m1$blocks
                             
                             #trade buffer to m2
                             if(!buffer$has_no_stored_blocks()){
                               if(!m2$is_occupied){
                                 buffer_block <- buffer$pop_stored_block()
                                 buffer_block_job <- buffer_block$assigned_job
                                 to_be_added_block <- buffer_block_job$to_Block(buffer_block$label, global_time, global_time + buffer_block_job$time_machine_two)
                                 
                                 
                                 m2$add_set_block(to_be_added_block)
                                 m2$set_is_occupied(TRUE)
                                 any_has_traded <- TRUE
                               }
                             }
                             
                             #trade m2 to trash
                             if(m2$is_occupied){
                               if(m2$is_done(global_time)){
                                 m2$clear()
                                 m2$set_is_occupied(FALSE)
                                 buffer$continue_local_sizes(start = 1, end = global_time-1)
                                 any_has_traded <- TRUE
                               }
                             }
                             
                             #increase global_time when no component has done anything this cycle
                             if(!any_has_traded){
                               overlap_costs <- overlap_costs + buffer$get_overlap_cost_at_time(global_time)
                               global_time <- global_time + 1
                             }
                             #break loop when everything is empty
                             if((i > length(self$jobs)) & !m1$is_occupied & buffer$has_no_stored_blocks() & !m2$is_occupied){
                               break
                             }
                             if(global_time-1 == self$max_time) {
                               buffer$continue_local_sizes(start = 1, end = global_time-1)
                               break
                             }
                           }
                           self$buffer_blocks <- get_buffer_block_data(m1, m2)
                           buffer$blocks <- self$buffer_blocks
                           self$m1 <- m1
                           self$m2 <- m2
                           self$buffer <- buffer
                           self$time_taken <- global_time-1
                           self$buffer$working_time <- self$time_taken
                           self$buffer_overlap_costs <- overlap_costs
                         },
                         
                         calc_time_with_spanning_buffer = function(buffer = self$buffer){
                           counter <- 0
                           biggest_size <- 0
                           overlap_costs <- 0
                           
                           #check if buffer can be big enough
                           for(job in self$jobs){
                             if (biggest_size < job$size){
                               biggest_size <- job$size
                             }
                           }
                           
                           i <- 1
                           m1 <- Machine$new()
                           m1_blockage_time <- 0
                           m2 <- Machine$new()
                           global_time <- 1
                           while(TRUE){
                             any_has_traded <- FALSE
                             
                             #job to buffer and m1
                             if(i <= length(self$jobs)){
                               to_be_added_job <- self$jobs[[i]]
                               label <- paste(to_be_added_job$name)
                               to_be_added_block <- to_be_added_job$to_Block(label, global_time, global_time + to_be_added_job$time_machine_one)
                               to_be_added_block$assign_job(to_be_added_job)
                               
                               if(m1$can_accept_block() & buffer$can_accept_block(to_be_added_block, global_time)){
                                 buffer$add_block(to_be_added_block)
                                 m1$add_set_block(to_be_added_block)
                                 m1$set_is_occupied(TRUE)
                                 any_has_traded <- TRUE
                                 i <- i + 1
                               }
                               else if(!m1$is_occupied & !buffer$can_accept_block(to_be_added_block, global_time)){
                                 m1$add_blockage(start = global_time, end = global_time + 1)
                               }
                             }
                             
                             #m1 is done
                             if(m1$is_occupied && m1$is_done(global_time)) {
                               m1_current_block <- m1$occupying_block
                               m1$add_actual_end_time(global_time)
                               m1$add_blockage(start = m1_current_block$end_time, end = global_time)
                               m1$clear()
                               m1$is_occupied <- FALSE
                               any_has_traded <- TRUE
                               counter <- counter + 1
                             }
                             
                             #buffer to m2
                             if(!m2$is_occupied & !buffer$has_no_stored_blocks() & counter > 0) {
                               buffer_block <- buffer$return_top_stored_block()
                               buffer_block_job <- buffer_block$assigned_job
                               to_be_added_block <- buffer_block_job$to_Block(buffer_block$label, global_time, global_time + buffer_block$get_time_for_m2())
                               m2$add_set_block(to_be_added_block)
                               m2$set_is_occupied(TRUE)
                               any_has_traded <- TRUE
                               counter <- counter - 1
                             }
                             m1_blocks = m1$blocks
                             
                             #trade m2 to trash
                             if(m2$is_occupied){
                               if(m2$is_done(global_time)){
                                 buffer_block <- buffer$pop_stored_block()
                                 m2$clear()
                                 m2$set_is_occupied(FALSE)
                                 buffer$continue_local_sizes(start = 1, end = global_time-1)
                                 any_has_traded <- TRUE
                               }
                             }
                             
                             #increase global_time when no component has done anything this cycle
                             if(!any_has_traded){
                               overlap_costs <- overlap_costs + buffer$get_overlap_cost_at_time(global_time)
                               global_time <- global_time + 1
                             }
                             #break loop when everything is empty
                             if((i > length(self$jobs)) & !m1$is_occupied & buffer$has_no_stored_blocks() & !m2$is_occupied){
                               break
                             }
                             if(global_time-1 == self$max_time) {
                               buffer$continue_local_sizes(start = 1, end = global_time-1)
                               break
                             }
                           }
                           self$buffer_blocks <- get_buffer_block_data_span(m1, m2)
                           buffer$blocks <- self$buffer_blocks
                           self$m1 <- m1
                           self$m2 <- m2
                           self$buffer <- buffer
                           self$time_taken <- global_time-1
                           self$buffer$working_time <- self$time_taken
                           self$buffer_overlap_costs <- overlap_costs
                         },
                         
                         get_instance_id = function(){
                           id <- paste("job_var", self$instance_data[1, "job_time_variation"], "cr_var", self$instance_data[1, "change_rate_variation"], "int_var", self$instance_data[1, "intensity_variation"], sep="")
                           id <- gsub("[[:punct:]]", "", id)
                           
                           return(id)
                         },
                         
                         gen_rectangles_from_buffer_blocks = function() {
                           rectangles <- list()
                           prev_end_time <- 1
                           for(block in self$buffer_blocks){
                             blocks_in_interval = list()
                             height <- 0
                             
                             for(block_to_check in self$buffer_blocks){
                               start <- prev_end_time
                               end <- block$end_time
                               if(block_to_check$is_in_interval(start, end)){
                                 x_rect_start <- start
                                 x_rect_end <- end
                                 
                                 if(block_to_check$start_time >= start) x_rect_start <- block_to_check$start_time
                                 if(block_to_check$end_time <= end) x_rect_end <- block_to_check$end_time
                                 
                                 new_rect <- RectWithLabel$new(xl = x_rect_start-1, xr = x_rect_end-1, yb = height, yt = block_to_check$size + height, label = block_to_check$label, buffer = self$buffer)
                                 
                                 height <- block_to_check$size + height
                                 
                                 rectangles <- append(rectangles, new_rect)
                               }
                             }
                             prev_end_time <- block$end_time
                           }
                           return(rectangles)
                         }
                       ))




