# a function designed to make user-input values that MIGHT be the same thing but with slightly different formatting identical for comparison
platonic_obliterator <- function(string_input){
  # make the string all lowercase
  platonic_ideal <- tolower(string_input)
  # remove all spaces from the string, replacing them with nothing
  platonic_ideal <- gsub(" ", "", platonic_ideal, fixed = TRUE)
  return(platonic_ideal)
}

# take a column in a data frame, make a platonic copy of that column and use it to remove categories from the original column, return the data frame but with that column's categories simplified
platonic_column <- function(data_frame, column_name_string, verbose=FALSE){
  # make a new list to hold new platonic categories we've already found
  platonic_cats <- c()
  # make a list to hold the first formatted instance of a category, pray to god that it's not the ugliest formatting possible
  groomed_cats <- c()
  # !!! Dictionaries would make this process much more elegant and efficient (they do not exist in R, sadge), where the key could be the platonic form and the stored value could be the formatted category name !!!
  # make a vector to eventually replace the category column we're working with, holding formatted platonic values
  new_cats <- c()
  # for each row of our data frame
  for (x in data_frame[[column_name_string]]){
    # if the platonic ideal of the value in the column is one we've seen before, skip this. If not, add the formatted category to the groomed list, and the platonic to the platonic list
    if (!(platonic_obliterator(x) %in% platonic_cats)){
      platonic_cats <- c(platonic_cats, platonic_obliterator(x))
      groomed_cats <- c(groomed_cats, x)
    }
    # loop over the list of formatted options
    for (y in groomed_cats){
      # if the formatted option matches the platonic form of the current category name, add that formatted option to the vector (should keep the order) then break the loop
      if (platonic_obliterator(x) == platonic_obliterator(y)){
        new_cats <- c(new_cats, y)
        break
      }
    }
  }
  # debug option to show the number of obliterated categories
  if (verbose == TRUE){
    # concatenate a bunch of strings to output them
    print(paste(
      "Obliterated ",
      (length(unique(data_frame[[column_name_string]])) - length(groomed_cats)),
      " of ",
      length(unique(data_frame[[column_name_string]])),
      " original categories, resulting in ",
      length(groomed_cats)
    ))
  }
  # replace the original category column with the platonic-friendly formatted values
  data_frame[[column_name_string]] <- new_cats
  # return the data frame, now with formatted but platonic-grouped names
  return(data_frame)
}