




#' onError
#'
#' @param err err
#' @param functionName functionName
#' @param step step
#' @export
onError<-function(err, functionName, step)

{
  message_traceback <- err$message
  errormessage = paste(paste("at ", functionName, " :: ",
                             step, sep = ""), message_traceback, sep = "\n")
  stop(errormessage, call. = FALSE)
}


#' getHistory
#' @export
getHistory<-function()
{

  functionName<-match.call()[[1]]
  step<-"Start"

  tryCatch({
    if(file.exists("REPAS.rds")){
      repasData<-readRDS("REPAS.rds");
    }else {
      repasData<-data.frame();
    }
    return(repasData)
  }, error = function(err) onError(err,functionName,step ))
}



#' getCiqualDataBase
#'
#' @return
#' @export
#'
#' @examples
getCiqualDataBase <- function(){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
    aliments <- read.csv(file = "ciqual_noaccent.csv",sep=";")
    return(aliments)
  }, error = function(err) onError(err,functionName,step ))
}




#' addAliment
#'
#' @param history history object
#' @param aliment aliment as data.frame
#'
#' @return
#' @export
#'
#' @examples
addRepasAliment <- function(history,aliment){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
    history <- rbind(history,aliment);
    saveRDS(history,"REPAS.rds");
    return(aliments)
  }, error = function(err) onError(err,functionName,step ))
}


#' createAliment
#'
#' @param name name
#' @param quantity quantity
#' @param repasType repasType
#'
#' @export
createRepasAliment <- function(name, quantity,repasType){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
    repasAliment<-data.frame(DATE=as.character(Sys.Date()),ALIMENT=name,QUANTITY=quantity,REPAS=repasType)
    return(repasAliment)
  }, error = function(err) onError(err,functionName,step ))
}

