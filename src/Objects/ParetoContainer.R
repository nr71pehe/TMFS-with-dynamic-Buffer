ParetoContainer = R6Class("ParetoContainer",
                              public = list(
                                front = NULL,
                                path = NULL,
                                jobvar = "",
                                crvar = "",
                                intvar = "",
                                color = "",
                                
                                initialize = function(front, path){
                                  self$front <- front
                                  self$path <- path
              
                                  
                                  instance_doc <- xmlParse(paste(path, "/instance_data.xml", sep=""))
                                  instance_df <- xmlToDataFrame(instance_doc, c("character", "character", "character"))
                                  
                                  self$jobvar <- strip_to_id_string(instance_df[[1, "job_time_variation"]])
                                  self$crvar <- strip_to_id_string(instance_df[[1, "change_rate_variation"]])
                                  self$intvar <- strip_to_id_string(instance_df[[1, "intensity_variation"]])
                                },
                                
                                get_front = function(){
                                  self$front
                                },
                                
                                get_path = function(){
                                 self$path 
                                },
                                
                                get_color = function(){
                                  self$color
                                },
                                
                                set_color = function(color){
                                  self$color <- color
                                },
                                determine_color = function(instance_table){
                                  for(i in 1:length(instance_table)){
                                    crvar <- instance_table[[i]][[1]]
                                    intvar_table <- instance_table[[i]][[2]]
                                    
                                    for(j in 1 : length(intvar_table)){
                                      intvar <- intvar_table[[j]]
                                      
                                      if((strip_to_id_string(crvar) == self$crvar | self$crvar == "") &
                                         (strip_to_id_string(intvar) == self$intvar | self$intvar == "")){
                                        
                                          color <- evaluate_to_color(i, j)
                                          self$set_color(color)
                                          return(color)
                                      }
                                    }
                                  }
                                }
                              ))