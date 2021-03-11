
set.timR.1 <- function(time, unit){

  countdown  <- as.difftime(time, units=unit)
  ends.at    <- Sys.time() + countdown

  repeat{

    time.left    <- round(as.numeric(ends.at - Sys.time(), units = "secs"), 0)

    time.left.h  <- time.left %/% (60*60)
    time.left.h  <- ifelse(nchar(time.left.h)==1, paste0(0, time.left.h), time.left.h)
    time.left.m  <- time.left %/% 60
    time.left.m  <- ifelse(nchar(time.left.m)==1, paste0(0, time.left.m), time.left.m)
    time.left.s  <- round(time.left %% 60, 0)
    time.left.s  <- ifelse(nchar(time.left.s)==1, paste0(0, time.left.s), time.left.s)
    time.left.string <- paste(time.left.h, time.left.m, time.left.s, sep=":")

    cat(paste0("\nSession continues at ", format(ends.at, "%H:%M:%S"),
               ". Time left: ", time.left.string))

    if(round(Sys.time() - ends.at) == 0){
      cat("\n\n      >>> SESSION CONTINUES NOW <<<\n\n\n")
      break
    }

    sleep.time <- ifelse(time.left > 300, 30,
                         ifelse(time.left > 10, 5,
                                1))
    Sys.sleep(sleep.time)
  }
}

#install.packages("beepr")
library(beepr)


set.timR.2 <- function(time, unit){

  countdown  <- as.difftime(time, units=unit)
  ends.at    <- Sys.time() + countdown

  repeat{

    time.left    <- round(as.numeric(ends.at - Sys.time(), units = "secs"), 0)

    time.left.h  <- time.left %/% (60*60)
    time.left.h  <- ifelse(nchar(time.left.h)==1, paste0(0, time.left.h), time.left.h)
    time.left.m  <- time.left %/% 60
    time.left.m  <- ifelse(nchar(time.left.m)==1, paste0(0, time.left.m), time.left.m)
    time.left.s  <- round(time.left %% 60, 0)
    time.left.s  <- ifelse(nchar(time.left.s)==1, paste0(0, time.left.s), time.left.s)
    time.left.string <- paste(time.left.h, time.left.m, time.left.s, sep=":")

    cat(paste0("\nSession continues at ", format(ends.at, "%H:%M:%S"),
               ". Time left: ", time.left.string))

    if(round(Sys.time() - ends.at) == 0){
      beepr::beep("ping")
      cat("\n\n      >>> SESSION CONTINUES NOW <<<\n\n\n")
      break
    }

    sleep.time <- ifelse(time.left > 300, 30,
                         ifelse(time.left > 10, 5,
                                1))
    Sys.sleep(sleep.time)
  }
}
