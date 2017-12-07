
opal_creator <- function(variables_to_filter, opals_to_filter, filter_df){
  
  opal_var = apply(filter_df[variables_to_filter,], 2, prod)
  my_names = names(opal_var[!is.na(opal_var)])
  my_names = my_names[my_names %in% names(opals_to_filter)]
  output = opals_to_filter[my_names]
  return(output)
}

variable_creator <- function(single_opal, filter_df, leave_out){
  
  var_list = filter_df[,single_opal]
  names(var_list) = row.names(filter_df)
  my_names = names(var_list[!is.na(var_list)])
  output = my_names[!my_names %in% leave_out]
  return(output)
}

missing_variable_creator <- function(single_opal, filter_df){
  
  var_list = filter_df[,single_opal]
  names(var_list) = row.names(filter_df)
  my_names = names(var_list[is.na(var_list)])
  output = my_names
  return(output)
}