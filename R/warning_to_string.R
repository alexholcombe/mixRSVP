#' Calculate area under a particular domain of a Gaussian distribution
#'
#'
#' @return a string containing all warning info
warning_to_string<- function(thisWarning) {
  #When it's an actual warning rather than null [[1]], it will be of type list and
  #  have both a msg and a call field, so toString won't do the trick
  if (typeof(thisWarning) == "list") {
    msg<- firstWarning$message
    call<- capture.output(  #the function call that caused the warning
      print( thisWarning[[1]]$call ) #Because print has a method that formats it nicely
    )
    msgAndCall<- paste0("message= ",msg,", call= ",call)
    ans<- msgAndCall
  }
  else {
    ans <- firstWarning
  }
  return (ans)
}
