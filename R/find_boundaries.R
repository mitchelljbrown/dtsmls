#' find_peaks - finds the monitroing interval boundaries automatically
#'
#' @param alltests data read from dts (must be the output of "get_instantaneous" which uses "fit_convolve")
#'
#' @import pracma
#' @import ggrepel
#'
#' @return
#' @export
#'
#' @examples


find_peaks <- function(alltests) {

  # isolate single test
  best_interval <- alltests[e_time == unique(alltests$e_time)[3]]

  # create differences column
  best_interval[,difference := append(apply(best_interval[,'slope'], 2, diff)^3, 0)]

  # subset so only ports included (extreme slopes in vadose zone throw off peak finding algorithm)
  best_interval <- best_interval[(distance > (ports$top[1]+1)) & (distance < (ports$bottom[nrow(ports)] + 10))]

  # check with plot
  plot(difference~distance, best_interval, type='l')

  # set a max: take the average of the top 10 biggest differences
  thresh <- mean(best_interval[order(difference, decreasing=TRUE)]$difference[1:5])

  # Find Peaks====================================================================

  # REMINDER OF findpeaks OUTPUT
  # col 1: height of peak
  # col 2: index
  # col 3: peak begin
  # col 4: peak end




  # find peaks and dips
  peaks <- data.table(findpeaks(best_interval$difference, threshold = max/3))
  dips <- data.table(findpeaks((best_interval$difference*-1), threshold = max/3))

  # critical! - add one idex location so the "peak" shows up after the large change in slope, not before it - this marks the end of a monitoring interval
  peaks$V2 <- peaks$V2 + 1
  peaks_dips <- rbind(peaks, dips)[order(V2)]
  peaks_dips <- peaks_dips[abs(V1)>max/3]



  colnames(peaks_dips)[2] <- "index"

  # create index column for join
  best_interval[,index := 1:(nrow(best_interval))]
  df <- merge(x=best_interval, y=peaks_dips, by="index", all.x=TRUE)
  df[,peaks := ifelse(is.na(V1) == TRUE, "FALSE", "TRUE")]
  df1 <- df[peaks==TRUE]


  # plot==========================================================================

  # plot of peaks on differences^3
  peaks <- ggplot(best_interval, aes(distance, difference)) +
    geom_line(aes(color=material, group=1)) +
    geom_point(data=df1, aes(distance, difference, label=difference)) +
    geom_line(data=best_interval, aes(color=material, group=1)) +
    coord_flip(ylim=c(-.1,.1)) +
    scale_x_reverse() +
    theme(legend.position = c(0.9,0.3))
  peaks

  # peak locations on slope plot
  slopes <- ggplot(best_interval, aes(distance, slope)) +
    geom_point(aes(col=material, group=1)) +
    geom_line(aes(col=material, group=1)) +
    geom_point(data=df1, aes(distance, slope)) +
    coord_flip(ylim=c(0,5)) +
    scale_x_reverse() +
    theme(legend.position = c(0.9,0.3))
  slopes


  data <- df[, c('distance', 'slope', 'e_time', 'l_time', 'material', 'machine', 'date', 'well', 'difference', 'V1', 'peaks')]

  return(data)

}
