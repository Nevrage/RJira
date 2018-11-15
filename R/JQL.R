library(utils)
library(tidyverse)
library(dplyr)
library(httr)
library(plyr)
library(lubridate)
library(utils)



JQL <- function(query){
  bit <-  '&startAt='
  query <- URLencode(query)
  URL <- paste0(api,query)
  REQ <- function(u) {
      req<-GET(u,
        authenticate(user = user,password = password , type = "basic"))
        stop_for_status(req)
        x <- content(req)
        return(x)
      }
      total <- REQ(URL)
      total <- total$total
    if (total > 0){
      cat(paste0("=>   fetching a total of ",total," issues"),quote="\n")
      i <- (0:trunc(total/50))*50
      mapping <- function(N){
          x <- data.frame(obtained="API",
                          stringsAsFactors = FALSE)
    if (exists("severity")) {
        if(is.null(y$issues[[N]]$fields[severity][[1]]$value)){x$severity <-  NA } else {x$severity  <- y$issues[[N]]$fields[severity][[1]]$value}
      }
    
    if (is.null(y$issues[[N]]$fields$priority$name)) {x$priority <- NA}else{x$priority <- y$issues[[N]]$fields$priority$name}
    if (is.null(y$issues[[N]]$fields$summary)) {x$summary <- NA}else{x$summary <- y$issues[[N]]$fields$summary}
    if (is.null(y$issues[[N]]$key)) {x$key <- NA}else{x$key <- y$issues[[N]]$key}
    if (is.null(y$issues[[N]]$fields$reporter$displayName)) {x$reporter <- NA}else{x$reporter <- y$issues[[N]]$fields$reporter$displayName}
    if (is.null(y$issues[[N]]$fields$reporter$name)) {x$name <- NA}else{x$name <- y$issues[[N]]$fields$reporter$name}
    if (is.null(y$issues[[N]]$fields$issuetype$name)) {x$type <- NA}else{x$type <- y$issues[[N]]$fields$issuetype$name}
    if (is.null(y$issues[[N]]$fields$created)) {x$created <- NA}else{x$created <- y$issues[[N]]$fields$created}
    if (is.null(y$issues[[N]]$fields$status$name)) {x$status <- NA}else{x$status <- y$issues[[N]]$fields$status$name}
    if (is.null(y$issues[[N]]$fields$"customfield_10019"$value)) {x$Result <- NA}else{x$Result <- y$issues[[N]]$fields$"customfield_10019"$value}
    if (is.null(y$issues[[N]]$fields$"customfield_10034"$value)) {x$Install <- NA}else{x$Install <- y$issues[[N]]$fields$"customfield_10034"$value}
    if (is.null(y$issues[[N]]$fields$"customfield_f0033")) {x$Install.comment <- NA}else{x$Install.comment <- y$issues[[N]]$fields$"customfield_10033"}
    if (is.null(y$issues[[N]]$fields$"customfield_12202"$value)) {x$grading <- NA}else{x$grading <- y$issues[[N]]$fields$"customfield_12202"$value}
    if (is.null(y$issues[[N]]$fields$"customfield_10026"$value)) {x$wallet <- NA}else{x$Wallet <-  y$issues[[N]]$fields$"customfield_10026"$value}
    if (is.null(y$issues[[N]]$fields$"customfield_10025")) {x$wallet.comment <- NA}else{x$wallet.comment <-  y$issues[[N]]$fields$"customfield_10025"}
    if (is.null(y$issues[[N]]$fields$"customfield_10037"$value)) {x$launch <- NA}else{x$launch <-  y$issues[[N]]$fields$"customfield_10037"$value}
    if (is.null(y$issues[[N]]$fields$"customfield_10036")) {x$launch.comment <- NA}else{x$launch.comment <-  y$issues[[N]]$fields$"customfield_10036"}
    if (is.null(y$issues[[N]]$fields$"customfield_10024"$value)) {x$AV <- NA}else{x$AV <- y$issues[[N]]$fields$"customfield_10024"$value}
	  if (is.null(y$issues[[N]]$fields$"customfield_10023")) {x$AV.comment <- NA}else{x$AV.comment <- y$issues[[N]]$fields$"customfield_10023"}
	  if (is.null(y$issues[[N]]$fields$"customfield_10505"$value)) {x$catLan <- NA}else{x$catLan <- y$issues[[N]]$fields$"customfield_10505"$value}
    if (is.null(y$issues[[N]]$fields$"customfield_10506")) {x$catLan.comment <- NA}else{x$catLan.comment <- y$issues[[N]]$fields$"customfield_10506"}
    if (is.null(y$issues[[N]]$fields$"customfield_10039"$value)) {x$MatchSP <- NA}else{x$MatchSP <- y$issues[[N]]$fields$"customfield_10039"$value}
    if (is.null(y$issues[[N]]$fields$"customfield_10038")) {x$MatchSP.comment <- NA}else{x$MatchSP.comment <- y$issues[[N]]$fields$"customfield_10038"}
    if (is.null(y$issues[[N]]$fields$"customfield_10507"$value)) {x$trdParty <- NA}else{x$trdParty <-y$issues[[N]]$fields$"customfield_10507"$value}
    if (is.null(y$issues[[N]]$fields$"customfield_10508")) {x$trdParty.comment <- NA}else{x$trdParty.comment <- y$issues[[N]]$fields$"customfield_10508"}
    if (is.null(y$issues[[N]]$fields$"customfield_10509"$value)) {x$review <- NA}else{x$review <- y$issues[[N]]$fields$"customfield_10509"$value}
    if (is.null(y$issues[[N]]$fields$"customfield_10510")) {x$review.comment <- NA}else{x$review.comment <- y$issues[[N]]$fields$"customfield_10510"}
    if (is.null(y$issues[[N]]$fields$"customfield_10700"$value)) {x$Advert <- NA}else{x$Advert <- y$issues[[N]]$fields$"customfield_10700"$value }
   #  if (is.null(y$issues[[N]]$fields$"customfield_10701")) {x$Advert.comment <- NA}else{x$Advert.comment <- y$issues[[N]]$fields$"customfield_10701"}
    if (is.null(y$issues[[N]]$fields$"customfield_10018"$value)) {x$SController <- NA}else{x$SController <- y$issues[[N]]$fields$"customfield_10018"$value}
    if (is.null(y$issues[[N]]$fields$"customfield_10017")) {x$SController.comment <- NA}else{x$SController.comment <- y$issues[[N]]$fields$"customfield_10017"}
    if (is.null(y$issues[[N]]$fields$"customfield_10022"$value)) {x$Uninstall <- NA}else{x$Uninstall <- y$issues[[N]]$fields$"customfield_10022"$value}
    if (is.null(y$issues[[N]]$fields$"customfield_10044")) {x$Uninstall.comment <- NA}else{x$Uninstall.comment <- y$issues[[N]]$fields$"customfield_10044"}
    if (is.null(y$issues[[N]]$fields$"customfield_10020")) {x$Notes <- NA}else{x$Notes <- y$issues[[N]]$fields$"customfield_10020"}
    if (is.null(y$issues[[N]]$fields$"customfield_10501")) {x$devNotes <- NA}else{x$devNotes <- y$issues[[N]]$fields$"customfield_10501"}
    if (is.null(y$issues[[N]]$fields$"customfield_10014")) {x$appid <- NA}else{x$appid <- y$issues[[N]]$fields$"customfield_10014"}
    #if (is.null(y$issues[[N]]$fields$"customfield_10016")) {x$score <- NA}else{x$score <- y$issues[[N]]$fields$"customfield_10016"}
    if (is.null(y$issues[[N]]$fields$"customfield_10403"$value)) {x$Screenshot <- NA}else{x$Screenshot <- y$issues[[N]]$fields$"customfield_10403"$value}
    if (is.null(y$issues[[N]]$fields$"customfield_10404")) {x$Screenshot.comment <- NA}else{x$Screenshot.comment <- y$issues[[N]]$fields$"customfield_10404"}
    if (is.null(y$issues[[N]]$fields$"customfield_10409"$value)) {x$Loc <- NA}else{x$Loc <- y$issues[[N]]$fields$"customfield_10409"$value}
    if (is.null(y$issues[[N]]$fields$"customfield_10410")) {x$Loc.comment <- NA}else{x$Loc.comment <- y$issues[[N]]$fields$"customfield_10410"}
    if (is.null(y$issues[[N]]$fields$"customfield_12007")) {x$proton <- NA}else{x$proton <- y$issues[[N]]$fields$"customfield_12007"}
    if (is.null(y$issues[[N]]$fields$"customfield_10411"$value)) {x$System <- NA}else{x$System <- y$issues[[N]]$fields$"customfield_10411"$value}
    if (is.null(y$issues[[N]]$fields$"customfield_10412")) {x$System.comment <- NA}else{x$System.comment <- y$issues[[N]]$fields$"customfield_10412"}
    if (is.null(y$issues[[N]]$fields$"customfield_10413"$value)) {x$Sound <- NA}else{x$Sound <- y$issues[[N]]$fields$"customfield_10413"$value}
    if (is.null(y$issues[[N]]$fields$"customfield_10414")) {x$Sound.comment <- NA}else{x$Sound.comment <- y$issues[[N]]$fields$"customfield_10414"}
    if (is.null(y$issues[[N]]$fields$"customfield_10417"$value)) {x$NSF <- NA}else{x$NSF <- y$issues[[N]]$fields$"customfield_10417"$value}
    if (is.null(y$issues[[N]]$fields$"customfield_10416")) {x$NSF.comment <- NA}else{x$NSF.comment <- y$issues[[N]]$fields$"customfield_10416"}
    if (is.null(y$issues[[N]]$fields$"customfield_10405"$value)) {x$Capsule <- NA}else{x$Capsule <- y$issues[[N]]$fields$"customfield_10405"$value}
    if (is.null(y$issues[[N]]$fields$"customfield_10406")) {x$Capsule.comment <- NA}else{x$Capsule.comment <- y$issues[[N]]$fields$"customfield_10406"}
    if (is.null(y$issues[[N]]$fields$"customfield_10407"$value)) {x$Written <- NA}else{x$Written <- y$issues[[N]]$fields$"customfield_10407"$value}
    if (is.null(y$issues[[N]]$fields$"customfield_10408")) {x$Written.comment <- NA}else{x$Written.comment <- y$issues[[N]]$fields$"customfield_10408"}
    if (is.null(y$issues[[N]]$fields$"customfield_10504")) {x$requested <- NA}else{x$requested <- y$issues[[N]]$fields$"customfield_10504"}
    if(!is.null(y$issues[[N]]$fields$"customfield_10015")){
      A <- length(y$issues[[N]]$fields$"customfield_10015")
      a <- unlist(lapply(1:A,function(a)(y$issues[[N]]$fields$"customfield_10015"[[a]]$value)))
      if ("SteamOS" %in% a) {x$SteamOS <- TRUE }else{x$SteamOS <- FALSE}
      if ("Windows" %in% a) {x$Windows <- TRUE }else{x$Windows <- FALSE}
      if ("Mac" %in% a) {x$Mac <- TRUE }else{x$Mac <- FALSE}
    }
    b <-length(y$issues[[N]]$fields$component)
    if (b>0){
      x$component <- paste(unlist(lapply(1:b,function(n)(y$issues[[N]]$fields$component[[n]]$name))),collapse=",")
    }

    bb <-length(y$issues[[N]]$fields$labels)
    if (bb>0){
           x$labels  <-  y$issues[[N]]$fields$labels %>% paste(collapse=",") 
    }
    

		return(x)}

    mappedping <- function(I){
      y <<-  REQ(paste0(URL,bit,I))
      y <-  REQ(paste0(URL,bit,I))
      l <- length(y$issues)
      l <- 1:l
      z <- lapply(l,mapping)
      z <- plyr::ldply(z, data.frame)
      cat(paste0("Parsed issues up to  ",as.character(I+50)," out of ",total),quote="\n")
      return(z)
    }
      # main loop
      x <- lapply(i,mappedping)
      x <- plyr::ldply(x, data.frame)

      x <- x[,colSums(is.na(x))<nrow(x)]

      x$created <- gsub("T","-",x$created)
      x$created <- gsub(".000-0500","",x$created)
      x$created <- gsub(".000-0400","",x$created)
      x$created.date <- as.Date(as.character(x$created,format="%Y-%m-%d-%h-%m-%s"))

      if (!is.null(x$requested)){
      x$requested <- gsub("T","-",x$requested)
      x$requested <- gsub(".000-0500","",x$requested)
      x$requested <- gsub(".000-0400","",x$requested)
      x$requested.date <- as.Date(as.character(x$requested,format="%Y-%m-%d-%h-%m-%s"))
      }
      x <- mutate(x,week=formatC(week(created.date),width = 2,format = "d",flag="0"),year=year(x$created.date),dateWeek = paste0(year,"-",week))

      # FINALLY
      return(x)} else {
              return(0)
    }
}

view <- function(data, autofilter=TRUE) {
    # data: data frame
    # autofilter: whether to apply a filter to make sorting and filtering easier
    open_command <- switch(Sys.info()[['sysname']],
                           Windows= 'open',
                           Linux  = 'xdg-open',
                           Darwin = 'open')
    require(XLConnect)
    temp_file <- paste0(tempfile(), '.xlsx')
    wb <- loadWorkbook(temp_file, create = TRUE)
    createSheet(wb, name = "temp")
    writeWorksheet(wb, data, sheet = "temp", startRow = 1, startCol = 1)
    if (autofilter) setAutoFilter(wb, 'temp', aref('A1', dim(data)))
    saveWorkbook(wb, )
    system(paste(open_command, temp_file))
}
