#' Format a HTML unordered list
#'
#' \code{listify} reformats list objects as unordered HTML lists.
#'
#' @param nested_list A list object containing a combination of strings or lists.
#' Each string element of the list is reformatted as a bullet in an unordered list.
#' Each list object element is treated as a nested unordered list, which will possess
#' a different bullet type.
#'
#' @details
#' This function is for internal package use. It creates a string in HTML format
#' that renders an unordered list when provided with a \code{list} object.
#'
#' @return An html-formatted list
#'
#' @author Ryden Butler
#'
#' @rdname listify

listify <- function(nested_list){
  paste(sapply(nested_list, function(x){
    ifelse(length(x) == 1,
           paste0('<li>', x, '</li>'),
           paste(sapply(x, function(y){
             ifelse(!is.list(y),
                    paste0('<li>', y),
                    paste0('<ul>', listify(y), '</ul></li>'))
           }), collapse = '')
    )
  }), collapse = '')
}
