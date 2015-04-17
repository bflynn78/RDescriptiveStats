
#' Discover and Analyze summary statistics from a list of numbers
#' 
#' @param   x (list of numbers)
#' @return  list \cr 
#'          list$Size \cr
#'          list$Sum \cr
#'          list$Minimum \cr
#'          list$Maximum \cr
#'          list$ArithmeticMean \cr
#'          list$HarmonicMean \cr
#'          list$GeometricMean \cr
#'          list$Median \cr
#'          list$Mode \cr
#'          list$Range \cr
#'          list$FirstQuartile \cr
#'          list$ThirdQuartile \cr
#'          list$IQR \cr
#'          list$SampleVariance \cr
#'          list$PopulationVariance \cr
#'          list$SampleStandardDeviation \cr
#'          list$PopulationStandardDeviation \cr
#'          list$CoefficientOfVariation \cr
#'          list$Skewness \cr
#'          list$Kurtosis \cr
#' @export
descriptiveStats <- function (x) {
  
  stats <- list()
  
  # 1 ////////////////////////////#
  size <- "Size"
  value <- length(x)
  stats[[ size ]] <- value
  #///////////////////////////////#
  
  # 2 ////////////////////////////#
  sum <- "Sum"
  value <- sum(x)
  stats[[ sum ]] <- value
  #////////////////////////////#
  
  # 3 ////////////////////////////#
  minimum <- "Minimum"
  value <- min(x)
  stats[[ minimum ]] <- value
  #///////////////////////////////#
  
  # 4 ////////////////////////////#
  maximum <- "Maximum"
  value <- max(x)
  stats[[ maximum ]] <- value
  #///////////////////////////////#
  
  # 5 ////////////////////////////#
  arithmeticMean <- "ArithmeticMean"
  value <- mean(x)
  stats[[ arithmeticMean ]] <- value
  #///////////////////////////////#
  
  # 6 ////////////////////////////#
  harmonicMean <- "HarmonicMean"
  value <- 1/mean(1/x) 
  stats[[ harmonicMean ]] <- value
  #///////////////////////////////#
  
  # 7 ////////////////////////////#
  geometicMean <- "GeometricMean"
  n = length(x)
  value <- prod(x)^(1/n) 
  stats[[ geometicMean ]] <- value
  #///////////////////////////////#
  
  # 8 ////////////////////////////#
  median <- "Median"
  value <- median(x)
  stats[[ median ]] <- value
  #///////////////////////////////#
  
  # 9 ////////////////////////////#
  mode <- "Mode"
  temp <- table(as.vector(x))
  value <- as.numeric(names(temp)[temp == max(temp)])
  stats[[ mode ]] <- value
  #///////////////////////////////#
  
  # 10 ////////////////////////////# 
  range <- "Range"
  value <- max(x) - min(x) 
  stats[[ range ]] <- value
  #////////////////////////////////#
  
  # 11////////////////////////////#
  firstQuartile <- "FirstQuartile"
  value <- quantile(x) 
  value
  stats[[ firstQuartile ]] <- value[2]
  #////////////////////////////////#
  
  # 12 ////////////////////////////#
  thirdQuartile <- "ThirdQuartile"
  value <- quantile(x) 
  stats[[ thirdQuartile ]] <- value[4]
  #////////////////////////////////#
  
  # 13 ////////////////////////////#
  iqr <- "IQR"
  value <- IQR(x) 
  stats[[ iqr ]] <- value
  #////////////////////////////////#
      
  # 14 ////////////////////////////#
  sampleVariance <- "SampleVariance"
  value <- var(x) 
  stats[[ sampleVariance ]] <- value
  #////////////////////////////////#
    
  # 15 ////////////////////////////#
  populationVariance <- "PopulationVariance"
  n = length(x)
  value <- ((n-1)/n) * var(x) 
  stats[[ populationVariance ]] <- value
  #////////////////////////////////#
  
  # 16 ////////////////////////////#
  sampleStandardDeviation <- "SampleStandardDeviation"
  value <- sd(x) 
  stats[[ sampleStandardDeviation ]] <- value
  #////////////////////////////////#
      
  # 17 ////////////////////////////#
  populationStandardDeviation <- "PopulationStandardDeviation"
  value <- sqrt( mean( ( x - mean( x ) )^2 ) )
  stats[[ populationStandardDeviation ]] <- value
  #////////////////////////////////#
  
  # 18 ////////////////////////////#
  coefficientOfVariation <- "CoefficientOfVariation"
  value <- (sd(x)/mean(x))
  stats[[ coefficientOfVariation ]] <- value 
  #////////////////////////////////#
  
  # 19 ////////////////////////////#
  library(e1071)
  sskewness <- "Skewness"
  value <- skewness(x)
  stats[[ sskewness ]] <- value 
  #////////////////////////////////#
  
  # 20 ////////////////////////////#
  kkurtosis <- "Kurtosis"
  value <- kurtosis(x)
  stats[[ kkurtosis ]] <- value 
  #////////////////////////////////#  
  
  stats
  
}