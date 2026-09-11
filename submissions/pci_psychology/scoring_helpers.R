# Shared score-construction helpers for the PCI Psychology analyses.

scale_mean <- function(data, items,
                       min_answered = ceiling(length(items) / 2)) {
  values <- as.data.frame(lapply(data[items], as.numeric))
  answered <- rowSums(!is.na(values))
  result <- rowMeans(values, na.rm = TRUE)
  result[answered < min_answered] <- NA_real_
  result
}
