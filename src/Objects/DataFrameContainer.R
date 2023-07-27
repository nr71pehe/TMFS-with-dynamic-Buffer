DataFrameContainer <- R6Class("DataFrameContainer",
                              public = list(
                                df = NULL,
                                
                                initialize = function(df){
                                  self$df <- df
                                },
                  
                                get_df = function(){
                                  return(self$df)
                                }
                              ))