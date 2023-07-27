gen_rects_with_labels = function(frames, buffer){
  rects <- list()
  for(frame in frames){
    prev_cut <- 0
    if((length(frame$labels) != 0)){
      for(i in 1 : length(frame$labels)){
        rect <- RectWithLabel$new(xl = frame$xleft,
                          xr = frame$xright,
                          yb = prev_cut,
                          yt = frame$cuts[[i]],
                          label = frame$labels[[i]],
                          h = -100,
                          buffer = buffer)
        
        rects <- append(rects, rect)
        prev_cut <- prev_cut + frame$cuts[[i]]
      }
    }
  }
  return(rects)
}

RectWithLabel = R6Class("RectWithLabel",
                        public = list(
                          xl = NULL,
                          xr = NULL,
                          yb = NULL,
                          yt = NULL,
                          label = NULL,
                          h = NULL,
                          scale = NULL,
                          
                          initialize = function(xl, xr, yb, yt, label, h = -100, buffer){
                            self$xl <- xl
                            self$xr <- xr
                            self$yb <- yb
                            self$yt <- yt
                            self$label <- label
                            self$scale <- (50/buffer$max_size)
                            self$h <- h
                          },
                          
                          append_frame = function(frame){
                            self$xl <- self$xl
                            self$xr <- frame$xr
                          },
                          
                          can_append = function(frame){
                            return(((self$xr) == frame$xl) & (self$yb == frame$yb) & (self$yt == frame$yt) & (as.character(self$label) == as.character(frame$label)))
                          },
                          
                          plot_this = function(){
                            xr <- self$xr
                            xl <- self$xl
                            yb <- self$yb
                            yt <- self$yt
                            label <- self$label
                            scale <- self$scale
                            h <- self$h
                            
                            x_middle <- (xr + xl)/2
                            y_middle <- (((scale * yt + h) + (scale * yb + h)))/2
                            
                            rect(xleft = xl, xright = xr, ytop = scale * yt + h, ybottom = scale * yb + h)
                            text(x = x_middle, y = y_middle, TeX(label))
                          },
                          
                          print_label = function(){
                            print(self$label)
                          }
                        ))

find_corresponding_rect = function(rect, rects){
  for(element in rects){
    if(rect$can_append(element)){
      return(element)
    }
  }
  return(NULL)
}

merge_rects = function(rects){
  done = FALSE
  new_rects <- rects
  while(!done){
    temp_rects = list()
    already_merged <- list()
    skip <- FALSE
    
    if(length(rects) == 0){break}
    for(rect in new_rects){
      skip <- FALSE
      
      for(a_merged in already_merged){
        if(identical(a_merged, rect)){
          skip <- TRUE
          }
      }
      if(skip) next
      
      t_rect <- rect
      corr_rect <- find_corresponding_rect(rect, new_rects)
      if(!is.null(corr_rect)){
        already_merged <- append(already_merged, corr_rect)
        t_rect$append_frame(corr_rect)
        temp_rects <- append(temp_rects, t_rect)
        done <- FALSE
      }
      else{
        done <- TRUE
        temp_rects <- append(temp_rects, t_rect)
      }
    }
    new_rects <- temp_rects
  }
  return(new_rects)
}


