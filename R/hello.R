# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
#
#' Illustration of crayon colors
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @param data_set The data frame to be edited
#' @param names the rownames to be displayed
#' @param title the names of the row name.
#'
#' @return None
#'
#' @examples
#' df_rename(
#'      data.frame(letters[1:5], 1:5),
#'      LETTERS[1:5],
#'      "CAPS"
#' )
#'
#' @export
df_rename <- function(data_set, names, title) {

        data_set_new <- names %>%
                cbind(data_set)
        colnames(data_set_new) <- append(title, colnames(data_set))

        return(data_set_new)

}

library(dplyr)
library(broom)
library(knitr)

#' Pretty tables for LM
#'
#' Creates a knitr approved table with regression coefficients and confidence intervals
#'
#' @param fit The lm object
#' @param level confidence level
#' @param roundto how much do you want the results rounded to
#'
#' @return None
#'
#' @examples
#' fit <- lm(nrorm(100) ~ rnorm(100,2,3))
#' nice_table(fit, 0.90, roundto = 3)
#'
#' @export
nice_table <- function(fit, level = 0.95, roundto = 2){
        low_ <- paste(signif(level / 2 * 100,2))
        high_ <- paste(signif((1-level) * 100,2))

        fit_frame <- fit %>%
                tidy() %>%
                cbind(confint(fit, level = level)) %>%
                dplyr::select(estimate, `2.5 %`, `97.5 %`) %>%
                # as.data.frame() %>%
                sapply(round, roundto) %>%
                as.data.frame() %>%
                cbind(tidy(fit)$term)

        colnames(fit_frame)[4] <- 'Term'
        colnames(fit_frame)[2] <- '95% CI lower'
        colnames(fit_frame)[3] <- '95% CI higher'
        fit_frame %>%
                dplyr::select(term, estimate, `95% CI lower`, `95% CI higher`) %>%
                kable()
}

