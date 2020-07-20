#' Say hello
#'
#' Receive a greeting from your package
#'
#' @param name string. Your name
#' @return Returns a greeting, personalised using \code{name} if provided.
#' @examples
#' say_hello()
#' say_hello('Clare')
#' @export
say_hello <- function(name="stranger"){
  print(paste("Hello, ", name, "!", sep=""))
}
