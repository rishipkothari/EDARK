# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# exportPattern("^[[:alpha:]]+")

hello <- function() {
  print("Hello, World!")
}

edark <- function(dataset_to_use = iris){

  edark_dataset_input <<- dataset_to_use

  shinyApp(ui_edark, server_edark)

}
