# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

df_rename <- function(data_set, names, title) {

        data_set_new <- names %>%
                cbind(data_set)
        colnames(data_set_new) <- append(title, colnames(data_set))

        return(data_set_new)

}


