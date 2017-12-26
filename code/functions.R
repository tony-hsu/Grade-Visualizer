#' returns the input vector without missing values
#'
#' @param x A numeric vector
#' @return input vector without missing values
#' @title remove_missing
remove_missing = function(x) {
  result = c()
  for (i in x) {
    if (!is.na(i)) {
      result = c(result, i)
    }
  }
  return(result)
}

#' find the minimum value.
#'
#' @param x A numeric vector
#' @param na.rm boolean
#' @return the minimum
#' @title get_minimum
get_minimum = function(x, na.rm) {
  if (!((is.vector(x)) && (is.numeric(x)))) {
    stop("non-numeric argument")
  }
  if (na.rm) {
    x = remove_missing(x)
  }
  if (NA %in% x) {
    return(NA)
  }
  x = sort(x)
  return(x[1])
}

#' find the maximum value
#'
#' @param x A numeric vector
#' @param na.rm boolean
#' @return the maximum value
#' @title get_maximum
get_maximum = function(x, na.rm) {
  if (!((is.vector(x)) && (is.numeric(x)))) {
    stop("non-numeric argument")
  }
  if (na.rm) {
    x = remove_missing(x)
  }
  if ((!na.rm) && (NA %in% x)) {
    return(NA)
  }
  x = sort(x, decreasing = TRUE)
  return(x[1])
}

#' compute the overall range of the input vector
#'
#' @param x A numeric vector
#' @param na.rm boolean
#' @return the overall range
#' @title get_range
get_range = function(x, na.rm) {
  if (!((is.vector(x)) && (is.numeric(x)))) {
    stop("non-numeric argument")
  }
  if (na.rm) {
    x = remove_missing(x)
  }
  if ((!na.rm) && (NA %in% x)) {
    return(NA)
  }
  return(get_maximum(x, FALSE) - get_minimum(x, FALSE))
}

#' to compute the 10th percentile of the input vector.
#'
#' @param x A numeric vector
#' @param na.rm boolean
#' @return the 10th percentile of the input vector
#' @title get_percentile10
get_percentile10 = function(x, na.rm) {
  if (!((is.vector(x)) && (is.numeric(x)))) {
    stop("non-numeric argument")
  }
  if (na.rm) {
    y = remove_missing(x)
  }
  if ((!na.rm) && (NA %in% x)) {
    return(NA)
  }
  percentile = quantile(y, c(0.1))
  return(percentile[[1]])
  
}

#' to compute the 90th percentile of the input vector.
#'
#' @param x A numeric vector
#' @return the 90th percentile of the input vector
#' @param na.rm boolean
#' @title get_percentile90
get_percentile90 = function(x, na.rm) {
  if (!((is.vector(x)) && (is.numeric(x)))) {
    stop("non-numeric argument")
  }
  if (na.rm) {
    x = remove_missing(x)
  }
  if ((!na.rm) && (NA %in% x)) {
    return(NA)
  }
  percentile = quantile(x, c(0.9))
  return(percentile[[1]])
  
}

#' to compute the median of the input vector
#'
#' @param x A numeric vector
#' @param na.rm boolean
#' @return median
#' @title get_median
get_median = function(x, na.rm) {
  if (!((is.vector(x)) && (is.numeric(x)))) {
    stop("non-numeric argument")
  }
  if (na.rm) {
    x = remove_missing(x)
  }
  if ((!na.rm) && (NA %in% x)) {
    return(NA)
  }
  x = sort(x)
  if ((length(x) %% 2) != 0) {
    middle = (1 + length(x)) / 2
    return(x[middle])
  } else {
    return((x[length(x) / 2] + x[(length(x) + 2) / 2]) / 2)
    
  }
}

#' get the average
#'
#' @param x A numeric vector
#' @param na.rm boolean
#' @return the average
#' @title get_average
get_average = function(x, na.rm) {
  if (!((is.vector(x)) && (is.numeric(x)))) {
    stop("non-numeric argument")
  }
  if (na.rm) {
    x = remove_missing(x)
  }
  if ((!na.rm) && (NA %in% x)) {
    return(NA)
  }
  sum = 0
  for (i in x) {
    sum = sum + i
  }
  return(sum / length(x))
}

#' get the standard deviations
#'
#' @param x A numeric vector
#' @param na.rm boolean
#' @return the standard deviations
#' @title get_stdev
get_stdev = function(x, na.rm) {
  if (!((is.vector(x)) && (is.numeric(x)))) {
    stop("non-numeric argument")
  }
  if (na.rm) {
    x = remove_missing(x)
  }
  if ((!na.rm) && (NA %in% x)) {
    return(NA)
  }
  x_bar = get_average(x, FALSE)
  sum = 0
  for (i in x) {
    sum = sum + (i - x_bar) ** 2
  }
  variance = sum / (length(x) - 1)
  return(sqrt(variance))
}

#' returns the first quartile
#'
#' @param x A numeric vector
#' @param na.rm boolean
#' @return the first quartile
#' @title get_quartile1
get_quartile1 = function(x, na.rm) {
  if (!((is.vector(x)) && (is.numeric(x)))) {
    stop("non-numeric argument")
  }
  if (na.rm) {
    x = remove_missing(x)
  }
  if ((!na.rm) && (NA %in% x)) {
    return(NA)
  }
  quartiles = quantile(x)
  return(quartiles[[2]])
}

#' returns the third quartile
#'
#' @param x A numeric vector
#' @param na.rm boolean
#' @return the third quartile
#' @title get_quartile3
get_quartile3 = function(x, na.rm) {
  if (!((is.vector(x)) && (is.numeric(x)))) {
    stop("non-numeric argument")
  }
  if (na.rm) {
    x = remove_missing(x)
  }
  if ((!na.rm) && (NA %in% x)) {
    return(NA)
  }
  quartiles = quantile(x)
  return(quartiles[[4]])
}

#' returns the number of missing elements in the vector
#'
#' @param x A numeric vector
#' @return number of missing elements
#' @title count_missing
count_missing = function(x) {
  return(length(x) - length(remove_missing(x)))
}

#' takes a numeric vector, and returns a list of summary statistics
#'
#' @param x A numeric vector
#' @return a list of summary statistics
#' @title summary_stats
summary_stats = function(a) {
  result = list(
    minimum = get_minimum(a, TRUE),
    percent10 = get_percentile10(a, TRUE),
    quartile1 = get_quartile1(a, TRUE),
    median = get_median(a, TRUE),
    mean = get_average(a, TRUE),
    quartile3 = get_quartile3(a, TRUE),
    percent90 = get_percentile90(a, TRUE),
    maximum = get_maximum(a, TRUE),
    range = get_range(a, TRUE),
    stdev = get_stdev(a, TRUE),
    missing = count_missing(a)
  )
}

#' takes a list of summary statistics, and prints the values in a nice format,
#'
#' @param x A numeric vector
#' @return nothing.
#' @title print_stats
print_stats = function(a) {
  names = names(a)
  for (i in 1:length(a)) {
    name = names[i]
    index =
      len = 9 - nchar(name)
    space = ""
    if (len > 1) {
      for (j in 1:len) {
        space = paste(space, "")
      }
    }
    toPrint = paste0(name, space , ": ", format(round((a)[[i]], 4), nsmall = 4))
    cat(noquote(toPrint), "\n")
  }
}

#' takes three arguments: a numeric vector x, a minimum xmin, and a
# maximum xmax, to compute a rescaled vector with a potential scale from 0 to 100
#'
#' @param x A numeric vector
#' @param xmin minimum xmin
#' @param xmax maximum xmax
#' @return input vector rescaled
#' @title rescale100
rescale100 = function(x, xmin, xmax) {
  for (i in 1:length(x)) {
    x[i] = 100 * (x[i] - xmin) / (xmax - xmin)
  }
  return(x)
}

#' takes a numeric vector of length n, and returns a vector of length n − 1
# by dropping the lowest value.
#'
#' @param x A numeric vector
#' @return input vector with lowest value being dropped
#' @title drop_lowest
drop_lowest = function(x) {
  lowest = 1
  result = c()
  for (i in 2:length(x)) {
    if (x[lowest] > x[i]) {
      result = c(result, x[lowest])
      lowest = i
    } else {
      result = c(result, x[i])
    }
  }
  return(result)
}

#' takes a numeric vector of homework scores  and an
#optional logical argument drop, to compute a single homework value. If drop = TRUE, the
#lowest HW score must be dropped.
#'
#' @param x A numeric vector
#' @param drop boolean
#' @return the average of the homework scores
#' @title score_homework
score_homework = function(x, drop) {
  if (drop) {
    x = drop_lowest(x)
  }
  return(get_average(x, TRUE))
}

#' takes a numeric vector of quiz scores of length n, and an optional
#' logical argument drop, to compute a single quiz value.
#'
#' @param x A numeric vector
#' @param drop boolean
#' @return the average of the quiz
#' @title score_quiz
score_quiz = function(x, drop) {
  if (drop) {
    x = drop_lowest(x)
  }
  return(get_average(x, TRUE))
}

#' returns the quiz score
#'
#' @param x A numeric value
#' @return a numeric value of quiz score
#' @title score_lab
score_lab = function(x) {
  if (x > 10) {
    return(100)
  } else if (x == 10) {
    return(80)
  } else if (x >= 9) {
    return(60)
  } else if (x >= 8) {
    return(40)
  } else if (x >= 7) {
    return(20)
  } else {
    return(0)
  }
}


