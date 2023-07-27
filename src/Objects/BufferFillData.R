BufferData = R6Class("BufferData",
                     public = list(
                       label = NULL,
                       size = NULL,
                       
                       initialize = function(label, size){
                         self$label <- label
                         self$size <- size
                       },
                       
                       get_label  = function(){
                         return(self$label)
                       },
                       
                       get_size = function(){
                         return(self$size)
                       }
                       
                     ))
BufferDataBucket = R6Class("BufferDataBucket",
                     public = list(
                       data = list(),
                       
                       add_data = function(label, size){
                         buffer_data <- BufferData$new(label, size)
                         self$data <- append(self$data, buffer_data)
                       },
                       
                       get_data_at = function(i){
                         return(self$data[[i]])
                       },
                       
                       get_label = function(i){
                         return(self$data[[i]]$get_label())
                       },
                       
                       get_sizes = function(i){
                         return(self$data[[i]]$get_size())
                       },
                       
                       get_sizes_sum = function(){
                         sum <- 0
                         for(data in self$data){
                           sum <- sum + data$get_size()
                         }
                         return(sum)
                       },
                       
                       get_sizes_list = function(){
                         size_list <- list()
                         
                         for(data in self$data){
                           size_list <- append(size_list, data$get_size())
                         }
                         return(size_list)
                       },
                       
                       get_labels = function(){
                         ret <- list()
                         for(data in self$data){
                           ret <- append(ret, data$get_label())
                         }
                         
                         return(ret)
                       },
                       
                       is_empty = function(){
                         length(self$data == 0)
                       }
                       
                     ))

BufferFillData = R6Class("BufferFillData",
                         public = list(
                         buckets = list(),
                         
                         initialize = function(blocks, max_time){
                           for(k in 1 : max_time){
                             self$buckets <- append(self$buckets, BufferDataBucket$new())
                           }
                           for(i in 1 : max_time){
                             for(block in blocks){
                               if(block$start_time <= i & block$end_time > i){
                                 self$add_data_to(i, block$label, block$size)
                               }
                             }
                           }
                         },
                         
                         add_data_to = function(i, label, size){
                           
                           bucket_at_i <- self$buckets[[i]]
                           bucket_at_i$add_data(label, size)
                           self$buckets[[i]] <- bucket_at_i
                         }
                         ))

FrameData = R6Class("FrameData",
                    public = list(
                      xleft = 0,
                      xright = 0,
                      ytop = 0,
                      ybottom = 0,
                      labels = list(),
                      cuts = list(),
                      
                      initialize = function(xl, xr, yt, yb, labels){
                        self$xleft <- xl
                        self$xright <- xr
                        self$ytop <- yt
                        self$ybottom <- yb
                        self$labels <- labels
                      },
                      
                      add_cuts = function(sizes){
                        height <- 0
                        
                        for(size in sizes){
                          self$cuts <- append(self$cuts, (height + size))
                          height <- height + size
                        }
                      }
                    ))

buckets_contain_same_data = function(bucket1, bucket2){
  if((!is.null(bucket1) & !is.null(bucket2)) & !(length(bucket1$data) == 0 | length(bucket2$data) == 0)){
    if(length(bucket1$data) == length(bucket2$data)){
      for(i in 1 : length(bucket1$data)){
        bucket1_data <- bucket1$get_data_at(i)
        bucket2_data <- bucket2$get_data_at(i)
        if(bucket1_data$get_label() != bucket2_data$get_label() | bucket1_data$get_size() != bucket2_data$get_size()){
          return(FALSE)
        }
      }
      return(TRUE)
    }
  }
 return(FALSE)
}

get_frames_for_buffer_data = function(bfd){
  start <- 0
  frames <- list()
  
  buckets <- bfd$buckets
  for(i in 1 : (length(buckets) - 1)){
    b1 <- buckets[[i]]
    b2 <- buckets[[i+1]]
    if(!buckets_contain_same_data(b1, b2)){
      frame <- FrameData$new(xl = start,
                             xr = i,
                             yt = b1$get_sizes_sum(),
                             yb = 0,
                             labels = b1$get_labels())
      
      frame$add_cuts(b1$get_sizes_list())
      frames <- append(frames, frame)
      
      start <- i
    }
  }
  
  #last bucket
  b1 <- buckets[[length(buckets) - 1]]
  b2 <- buckets[[length(buckets)]]
  
  if(buckets_contain_same_data(b1, b2)){
    frame <- FrameData$new(xl = start,
                           xr = length(buckets),
                           yt = b1$get_sizes_sum(),
                           yb = 0,
                           labels = b1$get_labels())
    frame$add_cuts(b1$get_sizes_list())
    frames <- append(frames, frame)
  }
  
  else{
    #previous buckets
    frame <- FrameData$new(xl = start,
                           xr = length(buckets) - 1,
                           yt = b1$get_sizes_sum(),
                           yb = 0,
                           labels = b1$get_labels())
    
    frame$add_cuts(b1$get_sizes_list())
    frames <- append(frames, frame)
    
    #last bucket
    frame <- FrameData$new(xl = length(buckets) - 1,
                           xr = length(buckets),
                           yt = b2$get_sizes_sum(),
                           yb = 0,
                           labels = b2$get_labels())
    
    frame$add_cuts(b2$get_sizes_list())
    frames <- append(frames, frame)
    
  }
  
  return(frames)
}

