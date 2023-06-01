matrix_operations=function(matrix_list,operation){
  x0 = matrix_list[[1]]
  for(i in seq(2,length(matrix_list))){
    y = matrix_list[[i]]
    x0 = eval(call(operation,x0 , y))
  }
  return(x0)
}

global_intersect=function(vec_list){
  innit_set = unique(vec_list[[1]])
  for(i in seq(2,length(vec_list))){
    innit_set = intersect(innit_set,vec_list[[i]])
  }
  return(innit_set)
}