# ===================================================================
# Title: Cleaning Data
# Description:
#   This script performs cleaning tasks and transformations on
#   various columns of the raw data file.
# Input(s): data files 'rawscores.csv'
# Output(s): data file 'cleanscores.csv'
# Author: Tony Hsu
# Date: 11-07-2017
# ===================================================================

# packages/functions
library(readr)    # importing data
library(dplyr)    # data wrangling
source("functions.R") #data cleaning functions

# read csv files
raw = read_csv("../data/rawdata/rawscores.csv")

# output the rawscore data frame and its summary
sink(file = "../output/summary-rawscores.txt")
str(raw)
print(noquote(""))
for (i in 1:ncol(raw)) {
  print(names(raw)[i])
  stat = summary_stats(as.data.frame(raw[, i], drop = TRUE)[[1]])
  print_stats(stat)
  print(noquote(""))
}
sink()

# replacing NA with 0
for (i in 1:ncol(raw)) {
  for (j in 1:nrow(raw)) {
    if (is.na(raw[j, i])) {
      raw[j, i] = 0
    }
  }
}

#rescaling columns
raw = mutate(raw, QZ1 = rescale100(raw$QZ1, 0, 12))
raw = mutate(raw, QZ2 = rescale100(raw$QZ2, 0, 18))
raw = mutate(raw, QZ3 = rescale100(raw$QZ3, 0, 20))
raw = mutate(raw, QZ4 = rescale100(raw$QZ4, 0, 20))
raw = mutate(raw, Test1 = rescale100(raw$EX1, 0, 80))
raw = mutate(raw, Test2 = rescale100(raw$EX2, 0, 90))

#inserting homework
Homework = c()
for (i in 1:nrow(raw)) {
  temp = c()
  for (j in 1:9) {
    temp[j] = raw[[j]][i]
  }
  individual_hw = score_homework(temp, TRUE)
  Homework[i] = individual_hw
}
raw = mutate(raw, Homework = Homework)

#inserting Quiz
Quiz_all = c()
for (i in 1:nrow(raw)) {
  temp = c()
  count = 1
  for (j in 11:14) {
    temp[count] = raw[[j]][i]
    count = count + 1
  }
  individual_quiz = score_quiz(temp, TRUE)
  Quiz_all[i] = individual_quiz
}
raw = mutate(raw, Quiz = Quiz_all)

#grading Lab
lab = sapply(as.vector(raw$ATT), score_lab)
raw = mutate(raw, Lab = lab)

#assigning overall
overall = c()
for (i in 1:nrow(raw)) {
  value = (0.3 * raw$Homework[i]) +
    0.1 * raw$Lab[i] +
    0.15 * raw$Quiz[i] +
    0.2 * raw$Test1[i] +
    0.25 * raw$Test2[i]
  overall[i] = value
}
raw = mutate(raw, Overall = overall)

#grading function
Grading = function(x) {
  if (x < 50) {
    return("F")
  } else if ((x >= 50) & (x < 60)) {
    return ("D")
  } else if ((x >= 60) & (x < 70)) {
    return ("C-")
  } else if ((x >= 70) & (x < 77.5)) {
    return ("C")
  } else if ((x >= 77.5) & (x < 79.5)) {
    return ("C+")
  } else if ((x >= 79.5) & (x < 82)) {
    return ("B-")
  } else if ((x >= 82) & (x < 86)) {
    return ("B")
  } else if ((x >= 86) & (x < 88)) {
    return ("B+")
  } else if ((x >= 88) & (x < 90)) {
    return ("A-")
  } else if ((x >= 90) & (x < 95)) {
    return ("A")
  } else if (x >= 95) {
    return ("A+")
  }
}

#assigning grades
grades = c()
for (i in 1:nrow(raw)) {
  grade = Grading(raw$Overall[i])
  grades[i] = grade
}
raw = mutate(raw, Grade = grades)

#output result
for (i in c("ATT", "Homework", "Quiz", "Test1", "Test2", "Overall")) {
  column = raw[[i]]
  stats = summary_stats(column)
  if (i == "ATT") {
    output_string = "Lab-stats.txt"
    stats = summary_stats(raw[["Lab"]])
  } else {
    output_string = paste0(i, "-stats.txt")
  }
  sink(file = paste0("../output/", output_string))
  print_stats(stats)
  sink()
}

#sinking output
sink(file = "../output/summary-cleanscores.txt")
str(raw)
sink()

#sinking clean data frame
write_csv(raw, path = "../data/cleandata/cleanscores.csv")
