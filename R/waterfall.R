waterfall <- function(temp, x, y, percentage = T) {
  require("scales")
  temp <- as.data.frame(temp)
  temp <- temp[(order(temp[, y], decreasing = T)), ]
  if (percentage) {
    temp[, y] <- temp[, y]/sum(temp[, y])
  }
  temp[, x] <- factor(temp[, x], levels = (temp[!duplicated(temp[, x]), x]), ordered = T)
  temp$end <- cumsum(temp[, y])
  temp$start <- c(0, temp$end[-length(temp$end)])
  temp$id <- nrow(temp):1
  p <- ggplot(temp, aes(y = id), x = c(0,1))
  p <- p + geom_rect(aes(y = get(y), ymin = id - 0.45, ymax = id + 0.45, xmin = end,
                         xmax = start), 
                     fill = iep.charts::gg_color_hue(2)[2]) +
    scale_y_discrete(limits = rev(levels(temp$project)), expand = c(0,0))
  if (percentage) {
    p <- p + scale_x_continuous(labels = percent)
  }
  return(p)
}