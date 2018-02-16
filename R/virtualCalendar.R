.getVirtualCalendar <- function(dates, interSeasonBegin, interSeasonEnd, firstDay){
  
  #Push interSeasonBegin and interSeasonEnd on virtual dates
  allY <- unique(year(dates))
  
  
  interSeasonBegin <- sort(as.Date(do.call(c, sapply(interSeasonBegin, function(X){
    paste0(allY, substr(X, 5,10))
  }, simplify = FALSE))))
  
  interSeasonBegin <- interSeasonBegin[interSeasonBegin%in%dates]
  
  interSeasonEnd <- sort(as.Date(do.call(c, sapply(interSeasonEnd, function(X){
    paste0(allY, substr(X, 5,10))
  }, simplify = FALSE))))
  interSeasonEnd <- interSeasonEnd[interSeasonEnd%in%dates]
  
  
  monthSWinter <- c(1:4, 10:12)
  monthSSummer <- 5:9
  
  # found weekend, virtual calendar, usage of generic function impossible.
  interSeasonDay <- c(interSeasonBegin, interSeasonEnd)
  firstWeek <- firstDay:7
  torm <- length(firstWeek)
  dayType <- c(firstWeek, suppressWarnings(data.table(dates[-(1:torm)], 1:7)$V2))
  weD <- dates[dayType %in% c(6, 7)]
  
  # Select day in interSeason
  interSeason <- data.frame(begin = as.Date(interSeasonBegin), end = as.Date(interSeasonEnd))
  interSeasonDay <- sapply(1:nrow(interSeason), function(X){
    Y <- interSeason[X,]
    seq(as.Date(Y[,1]), as.Date(Y[,2]), by = "day")
  }, simplify = FALSE)
  interSeasonDay <- do.call("c", interSeasonDay)
  saisonDay <- which(!dates%in%interSeasonDay)
  
  # found breaks for interSeasonDay, create vector for each season
  breakS <- which(diff(saisonDay)!=1)+1
  allSaison <- list()
  saisonAffect <- 0
  for(i in 0:(length(breakS))){
    if(i == length(breakS)){
      CurrentSaison <-(breakS[i]):length(saisonDay)
      saisonAffect <- saisonAffect + 1
      allSaison[[saisonAffect]] <- saisonDay[CurrentSaison]
    }else{
      
      if(i == 0){
        CurrentSaison <- 1:(breakS[1]-1)
      }else{
        CurrentSaison <-breakS[i]:(breakS[i+1]-1)
      }
      saisonAffect <- saisonAffect + 1
      allSaison[[saisonAffect]] <- saisonDay[CurrentSaison]
    }
  }
  
  Saison <- lapply(allSaison, function(X){
    dates[X]
  })
  
  WS <- unlist(lapply(Saison, function(X){
    nbDayInWinter <- sum(month(X)%in%monthSWinter)
    nbDayInSummer <- sum(month(X)%in%monthSSummer)
    if(nbDayInWinter>nbDayInSummer){
      "W"
    }else{
      "S"
    }
  }))
  
  winter <- do.call("c",(Saison[which(WS == "W")]))
  
  summer <-  do.call("c", (Saison[which(WS == "S")]))
  
  winter
  summer
  interSeasonDay
  
  winterWeekend <- winter[winter%in%weD]
  winterWeek <- winter[!winter%in%weD]
  
  summerWeekend <- summer[summer%in%weD]
  summerWeek <- summer[!summer%in%weD]
  
  interSWeekend <- interSeasonDay[interSeasonDay%in%weD]
  interSWeek <- interSeasonDay[!interSeasonDay%in%weD]
  
  list(summerWd = summerWeek,
       summerWe = summerWeekend,
       winterWd = winterWeek,
       winterWe = winterWeekend,
       interSeasonWd = interSWeek,
       interSeasonWe = interSWeekend)
}
