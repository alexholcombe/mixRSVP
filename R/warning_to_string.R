#' Calculate area under a particular domain of a Gaussian distribution
#'
#'
#' @return a string containing all warning info
warning_to_string<- function(thisWarning) {
  #When it's an actual warning rather than null [[1]], it will be of type list and
  #  have both a msg and a call field, so toString won't do the trick
  if (typeof(thisWarning) == "list") {
    msg<- thisWarning$message
    call<- capture.output(  #the function call that caused the warning
      print( thisWarning[[1]]$call ) #Because print has a method that formats it nicely
    )
    if (length(call) > 1) {  #Sometimes call has more than one string, often ",..." when too many params to be printed, I guess
      call <- paste0(call,collapse='') #collapse into one string
    }
    msgAndCall<- paste0("message= ",msg,", call= ",call)
    ans<- msgAndCall
  }
  else {
    ans <- thisWarning
  }
  #print('Converted warning to '); print(ans)

  return (ans)
}
