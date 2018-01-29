context(".getVirtualCalendar")

test_that("test .getVirtualCalendar", {
  date <- seq(as.Date("2018-01-10"), as.Date("2019-01-22"), by = "day")
  interSeasonBegin <- as.Date(c("2018-03-01", "2018-09-01"))
  interSeasonEnd <- as.Date(c("2018-05-01", "2018-11-01"))
  
  #test weekend
  sapply(1:7, function(x){
    calendar <- .getVirtualCalendar(date, interSeasonBegin, interSeasonEnd, x)
    toSearch <- ifelse(x%in%c(6, 7), "winterWe", "winterWd")
    expect_true(date[1] %in% calendar[[toSearch]])
    expect_true(date[8] %in% calendar[[toSearch]])
    expect_true(date[15] %in% calendar[[toSearch]])
    if(x%in%c(6)){
      expect_true(date[2] %in% calendar[[toSearch]])
      expect_true(date[9] %in% calendar[[toSearch]])
      expect_true(date[16] %in% calendar[[toSearch]])
    }
    
    ##Test summer
    sumD <- seq(as.Date("2018-05-02"), as.Date("2018-08-31"), by = "day")
    expect_true(all(calendar$summerWe%in%sumD))
    expect_true(all(calendar$summerWd%in%sumD))
    summer <- c(calendar$summerWe, calendar$summerWd)
    summer <- summer[order(summer)]
    expect_true(identical(summer, sumD))
    
    
    ##Test winter
    winter <- c(seq(as.Date("2018-01-10"), as.Date("2018-02-28"), by = "day"),
                seq(as.Date("2018-11-02"), as.Date("2019-01-22"), by = "day"))
    expect_true(all(calendar$winterWe%in%winter))
    expect_true(all(calendar$winterWd%in%winter))
    winterC <- c(calendar$winterWe, calendar$winterWd)
    winterC <- winterC[order(winterC)]
    expect_true(identical(winterC, winter))
    
    
    ##Test winter
    interS <- c(seq(as.Date("2018-03-01"), as.Date("2018-05-01"), by = "day"),
                seq(as.Date("2018-09-01"), as.Date("2018-11-01"), by = "day"))
    expect_true(all(calendar$interSeasonWd%in%interS))
    expect_true(all(calendar$interSeasonWe%in%interS))
    
    
    interX <- c(calendar$interSeasonWe, calendar$interSeasonWd)
    interX <- interX[order(interX)]
    expect_true(identical(interS, interX))
    
    
  })%>%invisible()
  
 
  
  
  
})
  