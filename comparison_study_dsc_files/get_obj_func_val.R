extract_objective_function_val <- function(input){
  # check if it is the simplified gbcd fit object
  if ('fl' %in% names(input)){
    input <- input$fl
  }
  objective_func_names <- c('elbo', 'obj', 'obj_func', 'obj_func_val')
  matched_name <- intersect(names(input), objective_func_names)
  # Safety check: What if none of the names match?
  if (length(matched_name) == 0) {
    warning("No matching objective function name found in the list.")
    return(NULL)
  }
  
  # Safety check: What if multiple names match? (Takes the first one) -- This shouldn't happen, but it is a good check
  if (length(matched_name) > 1) {
    matched_name <- matched_name[1]
  }
  
  # Extract and return the value
  return(input[[matched_name]])
}
result <- extract_objective_function_val(input)