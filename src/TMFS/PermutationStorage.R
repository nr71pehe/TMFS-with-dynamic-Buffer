PermutationStorage = R6Class("PermutationStorage",
                             public = list(
                               permutations = list(),
                               
                               add_permutation = function(perm){
                                 self$permutations = append(self$permutations, perm)
                               },
                               
                               get_permutations = function(){
                                 return(self$permutations)
                               }
                             ))
                             