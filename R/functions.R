





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
      init = createRepasAliment("test","test","test");
      repasData<-init[0,]
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
    return(history)
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


#' createSelectAlimentList
#' @importFrom rlist  list.append
#' @export
createSelectAlimentList <- function(){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
    ciqual = getCiqualDataBase()
    list = list();
    for (row in 1:dim(ciqual)[1]){
      list = list.append(list,ciqual[row,'alim_code'])
    }
    names(list)= ciqual$alim_nom_fr
  return(list)
  }, error = function(err) onError(err,functionName,step ))
}
