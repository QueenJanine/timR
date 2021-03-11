### timR ###
### function 3: function 1 with audio signal and a super cool 90s progress bar

#install.packages("beepr")
#install.packages("progress")
# library(beepr)
# library(progress)

#' Title
#'
#' @param time numeric to indicate the time
#' @param unit character string to indicate the unit: c("secs", "mins")
#'
#' @export
#'
#' @examples \dontrun{set.timR3(10, "secs")}
set.timR.3 <- function(time, unit){

  countdown    <- as.difftime(time, units=unit)
  ends.at      <- Sys.time() + countdown
  total.secs   <- round(as.numeric(ends.at - Sys.time(), units = "secs"), 0)

  pb <- progress::progress_bar$new(
    format = "Timer ends at :ends.at. Time left: [:bar] :time (:percent)",
    clear = FALSE,
    total = total.secs,
    width = 80)

  for (i in 1:(total.secs)) {
    Sys.sleep(1)

    time.left   <- round(as.numeric(ends.at - Sys.time(), units = "secs"), 0)

    time.left.h <- time.left %/% (60*60)
    time.left.h <- ifelse(nchar(time.left.h)==1, paste0(0, time.left.h), time.left.h)
    time.left.m <- time.left %/% 60
    time.left.m <- ifelse(nchar(time.left.m)==1, paste0(0, time.left.m), time.left.m)
    time.left.s <- round(time.left %% 60, 0)
    time.left.s <- ifelse(nchar(time.left.s)==1, paste0(0, time.left.s), time.left.s)

    time.left.string <- paste(time.left.h, time.left.m, time.left.s, sep=":")
    time.left.string <- ifelse(substr(time.left.string,1,1)=="-", "00:00:00", time.left.string)

    pb$tick(tokens = list(ends.at = format(ends.at, "%H:%M:%S"),
                          time = time.left.string)
    )
  }
  beepr::beep("ping")
}

## test:
#
# time = 15
# unit ="secs"
# set.timeR.3(time, unit)
