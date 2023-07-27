col_blockage <- "gray85"
col_buffer_background <- "gray97"
col_filldata <- "darkseagreen2"
col_overlap <- "indianred1"
h <- -100

plot_background <- function(buffer, plot = TRUE){
  sizeMachines <- 20
  max_time_taken <- buffer$working_time
  
  buffer_size_line_labels = c()
  steps_for_size_line = 5
  
  for(x in steps_for_size_line : 0) {
    buffer_size_line_labels = c(buffer_size_line_labels, round(buffer$max_size * (x/steps_for_size_line)))
  }
   
  if(plot) {
    
    plot(c(0, max(max_time_taken)+1), c(0,h), type = "n", xlab = "", ylab = "",
         yaxt = "n", xaxt = "n", bty="n")
    axis(3, lwd=2, line=NA, at = pretty(0:(max(max_time_taken)), n = 5))
    axis(side = 2, lwd = 2, at = seq(-50, h, (h/2)/steps_for_size_line), labels = buffer_size_line_labels)
    axis(2, at = c(5, -0.5*sizeMachines, -1.5*sizeMachines, -68),
         labels = c("Time     ", "M1    ", "M2    ", ""),
         las=1,tick=F, xpd=TRUE,  line=-1)
    
    rect(xleft = 0, xright = max_time_taken, ybottom = h, ytop = h/2, lty = 0, col = col_buffer_background)
  }
  else{
    plot(c(0, max(max_time_taken)), c(-50,h), type = "n", xlab = "", ylab = "",
         yaxt = "n", xaxt = "n", bty="n")
    axis(side = 2, lwd = 2, at = seq(-50, h, (h/2)/steps_for_size_line), labels = buffer_size_line_labels)
    rect(xleft = 0, xright = max_time_taken, ybottom = h, ytop = h/2, lty = 0, col = col_buffer_background)
  }
}

draw_time_lines = function(buffer, lty = 0, steps = 1){
  ls_end <- buffer$working_time
  
  for(t in 0 : ls_end){
    if(t%%steps == 0)
      segments(x0 = t,
               y0 = 0,
               x1 = t,
               y1= -100,
               lty = lty)
  }
}

draw_horizontal_lines = function(buffer, lty = 0, steps = 4){
  scale <- (50/buffer$max_size)
  max_time_taken = buffer$working_time
  
  for(i in 1 : buffer$max_size){
    if(i%%steps == 0)
    segments(x0 = 0,
             y0 = scale * i + h,
             x1 = max_time_taken,
             y1= scale * i + h,
             lty = lty)
  }
}


#draws the buffer
draw_buffer <- function(buffer, lwd = 1){
    
    #declaring variables
    #to flip buffer upside down make h = -50 and make y_scale positive
    y_scale = -(50/buffer$max_size)
    local_sizes <- buffer$local_sizes
    current_x <- 0
    current_y <- y_scale * (-local_sizes[[1]])
    
    ##bottom
    #first line
    segments(x0 = current_x, 
             y0 = current_y + h, 
             x1 = current_x + 1, 
             y1 = current_y + h,
             lwd = lwd)
    
    current_x <- current_x + 1
    
    for(count in 2:(buffer$working_time)){
        
        if(current_y != (local_sizes[[count]] *y_scale)){
            
            segments(x0 = current_x,
                     y0 = current_y+h, 
                     x1 = current_x, 
                     y1 = y_scale *(-local_sizes[[count]])+h,
                     lwd = lwd)
            
            current_y <- y_scale * -local_sizes[[count]]
        }
        
        segments(x0 = current_x, 
                 y0 = current_y+h, 
                 x1 = current_x + 1, 
                 y1 = current_y+h,
                 lwd = lwd)
        
        current_x <- current_x + 1
    }
    ##rest of buffer
    
    ls_end <- buffer$working_time
    
    #left
    segments(x0 = 0,
             y0 = y_scale *(-local_sizes[[1]]) + h,
             x1 = 0,
             y1 = h,
             lwd = lwd)

    #right
    segments(x0 = ls_end,
             y0 = y_scale *(-local_sizes[[ls_end]]) + h ,
             x1 = ls_end,
             y1 = h,
             lwd = lwd)

    #top
    segments(x0 = 0,
             y0 = h ,
             x1 = ls_end,
             y1 = h,
             lwd = lwd)

}

#getbuffercoordinates

get_buffer_block_data = function(m1, m2){
  blocks_m1 = m1$blocks
  blocks_m2 = m2$blocks
  
  if(length(blocks_m1) != length(blocks_m2)){
    print("unequal amount of jobs on machines")
    return()
  }
  
  buffer_block_data = list()
  
  for(i in 1:length(blocks_m1)){
    block_m1 <- blocks_m1[[i]]
    block_m2 <- blocks_m2[[i]]
    
    
    start_time <- m1$get_actual_end_time(i)
    end_time <- block_m2$start_time
    size <- block_m1$size
    label <- block_m1$label
    
    buffer_block_data <- append(buffer_block_data, list(Block$new(label, start_time, end_time, size)))
  }
  
  return(buffer_block_data)
}

get_buffer_block_data_span = function(m1, m2){
  blocks_m1 = m1$blocks
  blocks_m2 = m2$blocks
  
  buffer_block_data = list()
  
  for(i in 1:length(blocks_m1)){
    block_m1 <- blocks_m1[[i]]
    block_m2 <- blocks_m2[[i]]
    
    
    start_time <- block_m1$start_time
    end_time <- block_m2$end_time
    size <- block_m1$size
    label <- block_m1$label
    
    buffer_block_data <- append(buffer_block_data, list(Block$new(label, start_time, end_time, size)))
  }
  
  return(buffer_block_data)
}

plot_block = function(buffer, start_time, end_time, size, height = 0){

  y_scale <- (50/buffer$max_size)
  
  x_left <- start_time
  x_right <- end_time
  y_top <- y_scale * (size + height) + h
  y_bottom <- y_scale * height + h  
  
  
  if(x_left != x_right){
  rect(xleft = x_left, xright = x_right, ytop = y_top, ybottom = y_bottom, lty = 0, col = col_filldata)
  }
}

plot_buffer_fill_data = function(buffer, buffer_fill_data){
  buckets <- buffer_fill_data$buckets
  for(i in 1 : length(buckets)){
    height <- 0
    bucket_data <- buckets[[i]]$data
    for(data in bucket_data){
      plot_block(buffer, i-1, i, data$get_size(), height)
      height <- height + data$get_size()
    }
  }
}


blocks_overlap = function(block1, block2){
  return(block1$end_time > block2$start_time)
}

get_interval_list_for_blocks = function(blocks, max_time){
  interval <- Interval$new()
  interval_list <- list()
  previous_change_time <- blocks[[1]]$start_time

  for(t in 1 : (max_time)){
    for(block in blocks){

      if((t == block$start_time | t == block$end_time) & t != previous_change_time){

        interval_list <- append(interval_list, Interval$new(size = t, start_time = previous_change_time))
        previous_change_time <- t
        next
      }
    }
  }
  return(interval_list)
}

block_overlaps_intervall = function(block, intervall){
  int_start <- intervall$start_time
  int_end <- intervall$size
  b_start <- block$start_time
  b_end <- block$end_time

  print(paste(int_start, int_end, b_start, b_end), sep = ", ")
  return(
    (int_end >= b_start & int_end <= b_end & int_start <= b_start & int_start <= b_end) |
    (int_end >= b_start & int_end >= b_end & int_start >= b_start & int_start <= b_end) |
    (int_end >= b_start & int_end <= b_end & int_start >= b_start & int_start <= b_end) |
    (int_end >= b_start & int_end >= b_end & int_start <= b_start & int_start <= b_end)
  )
}

get_block_at_t = function(blocks, t){
  for(block in blocks){
    if(block$start_time <= t & block$end_time > t){
      return(block)
    }
  }
  return(NULL)
}

print_blocks = function(blocks, length){
  machine_string <- ""
  for(t in 1 : length ){
    block <- get_block_at_t(blocks, t)
    if(!is.null(block)){
      machine_string <- paste(machine_string, block$label,",", sep = "")
    }
    else{
      machine_string <- paste(machine_string, "_x_,", sep = "")
    }
  }
  print(machine_string)
}

draw_rect_with_label = function(xl, xr, yt, yb, label, h, scale){
  
  
  x_middle <- (xr + xl)/2
  y_middle <- (((scale * yt + h) + (scale * yb + h)))/2
  
  rect(xleft = xl, xright = xr, ytop = scale * yt + h, ybottom = scale * yb + h)
  text(x = x_middle, y = y_middle, label)
}

plot_frames = function(frames, buffer){
  for(frame in frames){
    prev_cut <- 0
    if((length(frame$labels) != 0)){
    for(i in 1 : length(frame$labels)){
      draw_rect_with_label(xl = frame$xleft,
                           xr = frame$xright,
                           yb = prev_cut,
                           yt = frame$cuts[[i]],
                           label = TeX(frame$labels[[i]]),
                           h = -100,
                           scale = (50/buffer$max_size))

      prev_cut <- prev_cut + frame$cuts[[i]]
      }
    }
  }
}

merge_blockage_blocks = function(blockage_blocks){
  size <- 1
  blockage_overview = c()
  if(!is.null(blockage_blocks)){
    
    for(bblock in blockage_blocks){
      if(bblock$end_time > size) size <- bblock$end_time-1
    }
    
    for(i in 1:size){
      was_true <- FALSE
      for(bblock in blockage_blocks){
        if(i >= bblock$start_time & i <= bblock$end_time) {
          blockage_overview[i] <- TRUE
          was_true <- TRUE
          break
          }
      }
      if(!was_true) blockage_overview[i] <- FALSE
    }
    new_blockage_blocks <- list()
    start <- 1
    for(i in 1:size){
      if(blockage_overview[i] == FALSE){
        block <- get_blockage_block(start, i)
        new_blockage_blocks <- append(new_blockage_blocks, block)
        start <- i
      }
      if(i == size & blockage_overview[i] == TRUE){
        block <- get_blockage_block(start, i)
        new_blockage_blocks <- append(new_blockage_blocks, block)
      }
    }
    return(new_blockage_blocks)
  }
  else{
    return(NULL)
  }
}

#m1 height = -10
#m2 height = -30
plot_machine = function(height, size = 10, machine_sizes, machine){

  bblocks <- merge_blockage_blocks(machine$blockage_blocks)
  for(bblock in bblocks){
    start <- bblock$start_time-1
    end <- bblock$end_time-1
    
    rect(xleft = start,
         xright = end,
         ybottom = height - size/2,
         ytop = height + size/2,
         col = col_blockage)
  }
  
  for(block in machine$blocks){
    start <- block$start_time-1
    end <- block$end_time-1
    label <- block$label
    
    
    rect(xleft = start,
         xright = end,
         ybottom = height - size/2,
         ytop = height + size/2,
         col = "white")
    
    x_middle <- (start + end)/2
    y_middle <- ((height - size/2) + (height + size/2))/2
    
    text(x = x_middle, y = y_middle, TeX(label))
  }
}

plot_overlap_areas = function(buffer_fill_data, buffer){
  buckets <- buffer_fill_data$buckets
  scale <- (50/buffer$max_size)
  
  for(i in 1:length(buckets)){
    bucket <- buckets[[i]]
    sizes_sum <- bucket$get_sizes_sum()
    buffer_local_size <- buffer$local_sizes[[i]]
    
    if(sizes_sum > buffer_local_size){
      height_local_size <- scale * (buffer_local_size) + h
      height_sizes_sum <- scale * (sizes_sum) + h
      rect(xright = i-1,
           xleft = i,
           ybottom = height_local_size,
           ytop = height_sizes_sum,
           lty = 0,
           col = col_overlap)
    }
  }
}

cross_field = function(x0, y0, x1, y1, lwd = 1, col = col_overlap){
  segments(x0 = x0, y0 = y0, x1 = x1, y1 = y1, col = "red", lwd = lwd)
  segments(x0 = x1, y0 = y0, x1 = x0, y1 = y1, col = col, lwd = lwd)
}

plot_permutation = function(perm, buffer, lty_vlines = 0, steps_vlines = 1, steps_hlines = 2, lty_hlines = 0, plot_machines = TRUE){
  if(buffer$span) perm$calc_time_with_spanning_buffer(buffer)
  
  else perm$calc_time_with_buffer(buffer)
  
  dbuffer  <- DisplayBuffer$new(buffer)
  
  plot_background(dbuffer, plot_machines)
  
  bfd <- BufferFillData$new(buffer$blocks, perm$time_taken)
  
  draw_time_lines(dbuffer, lty = lty_vlines, steps = steps_vlines)
  draw_horizontal_lines(dbuffer, lty = lty_hlines, steps = steps_hlines)
  
  frames <- get_frames_for_buffer_data(bfd)
  
  plot_buffer_fill_data(buffer, bfd)
  plot_overlap_areas(bfd, buffer)
  
  m_rects <- perm$gen_rectangles_from_buffer_blocks()
  
  for(rect in m_rects){
    rect$plot_this()
  }
  
  plot_machine(height = -10, machine = perm$m1)
  plot_machine(height = -30, machine = perm$m2)
  draw_buffer(dbuffer, lwd = 2)
}

save_plot_FS = function(id, num, directory = "calculated_FS"){
  path <- paste(directory, "/", as.character(id), "/plots", sep="")
  
  if(!dir.exists(path)){
    dir.create(path)
  }
  file_path <- paste(path, "/num_", as.character(num),".png", sep="")
  png(file_path, width = 800, height = 600)
  plot_permutation_from_xml(id, num, print = FALSE)
  dev.off()
}

save_dataframecollection = function(id, dfc, directory = "calculated_FS"){
  path <- paste(directory, "/", as.character(id), "/plots", sep="")
  
  if(!dir.exists(path)){
    dir.create(path)
  }
  
  
  file_path <- paste(path, "/", as.character(id),".png", sep="")
  ggsave(file_path)
}

save_dataframecollection_to_all_plots = function(directory = "calculated_FS", show_algo = FALSE){
  directories <- list.dirs(path = directory, full.names = FALSE, recursive = FALSE)
  path <- paste(directory, "/all_plots", sep="")
  
  for(id in directories){
    path <- paste(directory, "/all_plots", sep="")
    if(id != "all_plots"){
      dfc <- DataFrameCollection$new(id, directory = directory)
      if(!show_algo){
      plot_DataFrameCollection(dfc, show_pareto_front = TRUE)
      }
      else{
      algo <- Algorithms$new(dfc)
      algo$calculate_all_algorithms()
      plot_DataFrameCollection(dfc, algo = algo, empty = FALSE)  
      }
      path <- paste(path, "/", as.character(id),".png", sep="")
      ggsave(path)
    }
  }
}

plot_permutation_from_xml = function(id, directory = "calculated_FS", row_in_xml = 1, print = TRUE){
  dfc <- DataFrameCollection$new(id, directory = directory)
  perm <- dfc$recreate_permutation_with_buffer(row_in_xml)
  plot_permutation(perm, perm$buffer, steps_vlines = 20, lty_vlines = 2)
  print(perm$buffer$change_rate)
  if(print){
  print("Job Data:")
  print(job_list_to_dataframe(perm$jobs))
  print(paste0("Makespan: ", perm$time_taken))
  print(paste0("Kosten: ", perm$buffer_overlap_costs))
  }
}


plot_DataFrameCollection = function(dfc, algo=NULL, empty = FALSE, background = FALSE, show_pareto_front = TRUE, show_algo = TRUE, show_arrows = FALSE){
  permutations_ = dfc$permutations_dataframe
  permutations = permutations_
  pareto_front = dfc$get_pareto_front()
  
  names(pareto_front)[names(pareto_front) == "time_taken"] <- "Makespan"
  names(pareto_front)[names(pareto_front) == "costs"] <- "Kosten"
  
  names(permutations)[names(permutations) == "time_taken"] <- "Makespan"
  names(permutations)[names(permutations) == "costs"] <- "Kosten"
  
  bckgr <- 0
  if(background) bckgr <- 1
  
  if(!empty){
  ggp <- ggplot(data = permutations,
         mapping = aes(x = Makespan, y = Kosten))+
         geom_mark_hull(expand = 0,
                        radius = 0,
                        linetype = 0,
                        label.width = 3,
                        size = 1,
                        fill = "gray85",
                        alpha = bckgr,
                        show.legend = FALSE)+
         geom_point()+
         theme_classic()
  }
  else{
    ggp <- ggplot(data = permutations,
           mapping = aes(x = Makespan, y = Kosten))+
      geom_mark_hull(expand = 0,
                     radius = 0,
                     linetype = 0,
                     label.width = 3,
                     size = 1,
                     fill = "gray93",
                     alpha = bckgr,
                     show.legend = FALSE)+
      geom_point(data = permutations, alpha = 0)+
      theme_classic()
    
  }
  if(!is.null(algo)){
    solutions <- algo$get_algorithm_information()
    arrow_df = get_arrow_dataframe(pareto_front, algo)
    if(show_algo){
      ggp <- ggp+
        geom_point(data=solutions, aes(x=Makespan, y=Kosten, colour = algorithm), size = 4)+
        scale_color_manual(values = algo$algorithm_color_scheme)
    }
    if(show_arrows){
      
      ggp <- ggp + 
        geom_segment(data = arrow_df, mapping = aes(x = Makespan, 
                                                    y = Kosten, 
                                                    xend = x_pareto_intersection, 
                                                    yend = y_pareto_intersection,
                                                    colour = algorithm),
                     arrow = arrow(), show.legend = FALSE)
        
    }
    print(arrow_df)
  }
  if(show_pareto_front){
    if(nrow(pareto_front) != 1){
      ggp <- ggp+
        geom_point(data = pareto_front, color="red", size = 3)+
        geom_line(data = pareto_front, color = "red")
    }
    else{
      ggp <- ggp+
        geom_point(data = pareto_front, color = "red", size = 3)+
        theme_classic()
    }
  }
  ggp <- ggp + 
    xlab("Makespan") + ylab("Kosten")
  ggp
}


plot_pareto_fronts_of_specific_variation_from_dataframes = function(pareto_fronts, seed, index, buffertype) {
  
  variation_type = ""
  
  if(index == 1) {
    variation_type = "jobvar"
  }
  else if(index == 2) {
    variation_type = "crvar"
  }
  else {
    variation_type = "volavar"
  }
  
    legend_labels <- c()
    all_data <- data.frame()
    i = 1
    
    ggp <- ggplot() 
    
    for(front in pareto_fronts){
      pareto_front <- front
      a <- rep(paste(variation_type, "_", i, sep = ""))
      i = i + 1
      pareto_front <- cbind(pareto_front, a)
      
      all_data <- rbind(all_data, pareto_front)
    }
    
    names(all_data)[names(all_data) == "a"] <- "instance_properties"
    cols <- c("path1" = "blue", "path2" = "green", "path3" = "gold")
    ggp <- ggplot(data = all_data, mapping = aes(x = time_taken, y = costs, color = instance_properties)) + 
      geom_line() + 
      geom_point() +
      labs(title =paste("Pareto Fronten für ", seed, " mit ", buffertype, " Buffer und ", variation_type, sep=""), x = "time taken")
    
    ggp
}

plot_original_pareto_front = function(pareto_front, seed, buffertype) {
  pareto_front_rename = pareto_front
  names(pareto_front_rename)[names(pareto_front_rename) == "time_taken"] <- "Makespan"
  names(pareto_front_rename)[names(pareto_front_rename) == "costs"] <- "Kosten"
  ggp <- ggplot(data = pareto_front_rename, mapping = aes(x = Makespan, y = Kosten)) + 
    geom_line() + 
    geom_point() +
    labs(title =paste("Originale Pareto Front für ", seed, " mit ", buffertype, " Buffer", sep=""), x = "Makespan")
  
  ggp
}
  
