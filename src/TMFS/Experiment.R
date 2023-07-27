Experiment <- R6Class("Experiment",
                      public = list(
                        jobvar = "",
                        crvar = "",
                        intvar = "",
                        id = 0,
                        directory = "",
                        corresponding_directories = list(),
                        instances = NULL,
                        instance_table = NULL,
                        base_instance_directory_name = "jobvar01u03crvar1intvar0u1",
                        
                        initialize = function(jobvar = "", crvar = "", intvar = "", id = 0, directory = "./", instances = NULL){
                          self$jobvar = jobvar
                          self$crvar = crvar
                          self$intvar = intvar
                          self$directory = directory
                          self$id = id
                          self$instances = instances
                          
                          if(!is.null(instances)){
                            crvars <- as.list(sort(unlist(instances$change_rate_variation)))
                            intvars <- as.list(sort(unlist(instances$intensity_variation)))
                            
                            intvar1 <- intvars[[1]]
                            intvar2 <- intvars[[2]]
                            intvar3 <- intvars[[3]]
                            
                            crvar1 <- crvars[[1]]
                            crvar2 <- crvars[[2]]
                            crvar3 <- crvars[[3]]
                            
                            value_table <- list(list(crvar1, list(intvar1, intvar2, intvar3)), 
                                                list(crvar2, list(intvar1, intvar2, intvar3)), 
                                                list(crvar3, list(intvar1, intvar2, intvar3)))
                            
                            self$instance_table = value_table
                          }
                          self$corresponding_directories = self$get_corresponding_directories()
                        },
                        
                        get_corresponding_directories = function(){
                          path <- paste(self$directory, "/d-", self$id, sep = "")
                          print(path)
                          all_directories <- list.dirs(path = path, recursive = FALSE)
                          print(all_directories)
                          corresponding_directories <- list()
                          
                          for(dir_name in all_directories){
                            if(grepl(paste("jobvar", self$jobvar, sep = ""), dir_name, fixed = TRUE) &
                               grepl(paste("crvar", self$crvar, sep = ""), dir_name, fixed = TRUE) &
                               grepl(paste("intvar", self$intvar, sep = ""), dir_name, fixed = TRUE))
                              
                              corresponding_directories <- append(corresponding_directories, dir_name)
                          }
                          self$corresponding_directories <- corresponding_directories
                          
                          corresponding_directories
                        },
                        
                        plot_this = function(start_at_zero = FALSE){
                          legend_labels <- c()
                          pareto_fronts <- list()
                          all_data <- data.frame()
                          
                          print(self$corresponding_directories)
                          
                          for(dir in self$corresponding_directories){
                            dfc <- DataFrameCollection$new(directory = dir)
                            print(dir)
                            pareto_fronts <- append(pareto_fronts, ParetoContainer$new(front = dfc$get_pareto_front(), path = dir))
                          }
                          
                          ggp <- ggplot() 
                          
                          for(front in pareto_fronts){
                            pareto_front <- front$get_front()
                            
                            a <- rep(get_data_string_from_path(front$path))
                            pareto_front <- cbind(pareto_front, a)

                            all_data <- rbind(all_data, pareto_front)
                          }
                          
                          names(all_data)[names(all_data) == "a"] <- "instance_properties"
                          cols <- c("path1" = "blue", "path2" = "green", "path3" = "red")
                          ggp <- ggplot(data = all_data, mapping = aes(x = time_taken, y = costs, color = instance_properties)) + 
                            geom_line() + 
                            geom_point() +
                            labs(title =paste(self$directory, "/",self$id ,"/", sep=""), x = "time taken")
                          
                          if(start_at_zero) ggp <- ggp + expand_limits(x = 0, y = 0)
                          
                          ggp
                        },
                        
                        plot_this_compare_with_base = function(start_at_zero = FALSE) {
                          legend_labels <- c()
                          pareto_fronts <- list()
                          all_data <- data.frame()
                          
                          print(self$corresponding_directories)
                          base_path <- paste(self$directory, "/d-", self$id,"/", self$base_instance_directory_name, sep = "")
                          
                          base_dfc <- DataFrameCollection$new(directory = base_path)
                          base_front <- ParetoContainer$new(front = base_dfc$get_pareto_front(), path = base_path)
                          
                          for(dir in self$corresponding_directories){
                            dfc <- DataFrameCollection$new(directory = dir)
                            print(dir)
                            pareto_fronts <- append(pareto_fronts, ParetoContainer$new(front = dfc$get_pareto_front(), path = dir))
                          }
                          
                          ggp <- ggplot() 
                          
                          real_base_front <- base_front$get_front()
                          a <- rep(get_data_string_from_path(base_front$path))
                          real_base_front <- cbind(real_base_front, a)
                          all_data <- rbind(all_data, real_base_front)
                          
                          for(front in pareto_fronts){
                            pareto_front <- front$get_front()
                            
                            a <- rep(get_data_string_from_path(front$path))
                            pareto_front <- cbind(pareto_front, a)
                            
                            all_data <- rbind(all_data, pareto_front)
                          }
                          
                          names(all_data)[names(all_data) == "a"] <- "instance_properties"
                          cols <- c("base" = "red", "path1" = "blue", "path2" = "green", "path3" = "gold")
                          ggp <- ggplot(data = all_data, mapping = aes(x = time_taken, y = costs, color = instance_properties)) + 
                            geom_line() + 
                            geom_point() +
                            labs(title =paste(self$directory, "/",self$id ,"/", sep=""), x = "time taken")
                          
                          if(start_at_zero) ggp <- ggp + expand_limits(x = 0, y = 0)
                          
                          ggp
                        }
                      ))




