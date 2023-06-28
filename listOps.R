# peform any operation on list of matrixes (analogous to do.call + inner/outer products + hadamard operations
matrix_operations=function(matrix_list,operation){
  x0 = matrix_list[[1]]
  for(i in seq(2,length(matrix_list))){
    y = matrix_list[[i]]
    x0 = eval(call(operation,x0 , y))
  }
  return(x0)
}
# finds global intersection over list of elements
global_intersect=function(vec_list){
  innit_set = unique(vec_list[[1]])
  for(i in seq(2,length(vec_list))){
    innit_set = intersect(innit_set,vec_list[[i]])
  }
  return(innit_set)
}
# performs matrix augmentations to incorporate elements found in one matrix in list missing in others, applies to all elements in list
augument_matrix=function(matrix_list){
  name_space = matrix_operations(lapply(matrix_list,function(x) colnames(x)),'union')
  augument_matrix_list = lapply(matrix_list,function(x){
    template_scaffold = matrix(0,length(name_space),length(name_space))
    colnames(template_scaffold)=name_space
    row.names(template_scaffold)=name_space
    for(i in seq(dim(x)[2])){
      template_scaffold[match(row.names(x),row.names(template_scaffold)),match(colnames(x)[i],colnames(template_scaffold))]=x[,i]
    }
    
    return(template_scaffold)
  })
  
  
}
