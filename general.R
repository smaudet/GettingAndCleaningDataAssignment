dirStack <- c('')

pushd <- function(newd) {
  dirStack <<- append(dirStack,getwd())
  setwd(newd)
  print(getwd())
}

popd <- function() {
  setwd(last(dirStack))
  print(getwd())
  dirStack <<- dirStack[-c(length(dirStack))]
}

ls_dir <- function() {
  print(list.files())
}