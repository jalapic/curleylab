#' Curley Lab ggplot2 generic theme
#' @return Generic theme for ggplots
#' @examples
#' ggplot(df, aes(x, y)) + geom_point() + curleytheme
#' @section Further details:
#' A generic plotting theme for ggplot graphs
#' @export

curleytheme <- function(){

  library(ggplot2)
  
  theme(
  plot.title = element_text(hjust=0,vjust=1, size=rel(2.3)),
  panel.background = element_blank(),
  panel.grid.major.y = element_line(color="gray65"),
  panel.grid.major.x = element_line(color="gray65"),
  panel.grid.minor = element_blank(),
  plot.background  = element_blank(),
  text = element_text(color="gray20", size=10),
  axis.text = element_text(size=rel(1.0)),
  axis.text.x = element_text(color="gray20",size=rel(1.5)),
  axis.text.y = element_text(color="gray20", size=rel(1.5)),
  axis.title.x = element_text(size=rel(1.5), vjust=0),
  axis.title.y = element_text(size=rel(1.5), vjust=1),
  axis.ticks.y = element_blank(),
  axis.ticks.x = element_blank(),
  legend.position = "none"
)
}