### timR ###
### function 4: timer with audio signal and a super cool donut plot in ggplot

#install.packages("beepr")
#install.packages("ggplot2")
# library(beepr)
# library(ggplot2)


#' Title
#'
#' @param time numeric to indicate the time
#' @param unit character string to indicate the unit: c("secs", "mins")
#'
#' @return
#' @export
#'
#' @examples
set.timR.4 <- function(time, unit){

  countdown  <- as.difftime(time, units=unit)
  ends.at    <- Sys.time() + countdown

  plot.df <- data.frame(const=1, cat = stats::relevel(as.factor(c("passed", "left")), "passed"), time = NA)

  repeat{
    Sys.sleep(1)

    time.left    <- round(as.numeric(ends.at - Sys.time(), units = "secs"),0)
    time.passed  <- as.numeric(countdown, units = "secs") - time.left

    time.left.h  <- time.left %/% (60*60)
    time.left.h  <- ifelse(nchar(time.left.h)==1, paste0(0, time.left.h), time.left.h)
    time.left.m  <- time.left %/% 60
    time.left.m  <- ifelse(nchar(time.left.m)==1, paste0(0, time.left.m), time.left.m)
    time.left.s  <- round(time.left %% 60, 0)
    time.left.s  <- ifelse(nchar(time.left.s)==1, paste0(0, time.left.s), time.left.s)
    time.left.string <- paste(time.left.h, time.left.m, time.left.s, sep=":")

    plot.df$time[1] <- time.passed
    plot.df$time[2] <- time.left

    print(
      ggplot2::ggplot(plot.df, ggplot2::aes(fill=cat, y=time, #x=const
                                            ), xmin=0, xmax=4#, ymax=ymax, ymin=ymin
                      ) +
        ggplot2::geom_bar(position="stack", stat="identity") +
        ggplot2::coord_polar(theta="y") +
        ggplot2::xlim(c(0.1, 1.5)) +
        ggplot2::labs(x="", y="") +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::annotate("text", x = 0.1, y = 0, label = time.left.string)
    )

    if(round(Sys.time() - ends.at) >= 0){
      break
    }
  }
  beepr::beep("ping")
  cat("\n\n    >>> SESSION CONTINUES NOW <<<\n\n\n")

}

## test:
#
# time  = 2
# unit ="mins"
# set.timeR.4(time, unit)
