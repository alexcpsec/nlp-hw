## NLP
## HW 2 - Part 1
## Rare word substition

setwd("~/Dropbox/Courses/Coursera - NLP/nlp-hw/PA2/assignment")
library(rjson)
library(hash)

fileTrain = "parse_train_vert.dat"
jsonTrain = readLines(fileTrain, n = -1)

tabTrain = fromJSON(jsonTrain)

## Let's read the counts:
fileCounts = readLines("parse_train_vert.count", n = -1)
splitCounts = strsplit(fileCounts, " ")

hashWords = hash()
hashSymbols = hash()

extractWords <- function(row) {
  if (row[2] == "UNARYRULE") {
    word = as.character(row[4])
    symbol = as.character(row[3])
    count = as.numeric(row[1])
    if (has.key(word, hashWords)) {
      hashWords[[word]] = hashWords[[word]] + count
    } else {
      hashWords[[word]] = count
    }
    if (has.key(symbol, hashSymbols)) {
      hashSymbols[[symbol]] = hashSymbols[[symbol]] + count
    } else {
      hashSymbols[[symbol]] = count
    }
  }
}

sapply(splitCounts, extractWords)
freqCounts = values(hashWords)

replaceRares <- function(idxJson) {
  jStr = jsonTrain[idxJson]
  jObj = fromJSON(jStr)
  
  hashJson = hash()
  
  findRareWords = function(jObj) {
    if (class(jObj) == "list") {
      for (i in 1:length(jObj)) {
        findRareWords(jObj[[i]])
      } 
    } else {
      if (length(jObj) == 2) {
        strW = jObj[2]
        if (freqCounts[strW] < 5) {
          hashJson[[strW]] = freqCounts[strW] 
        }
      }
    }
  }
  
  findRareWords(jObj)
  
  replJson = keys(hashJson)
  for (i in 1:length(replJson)) {
    jStr = gsub(paste0("\"", replJson[i], "\""), "\"_RARE_\"", jStr, fixed = TRUE)
  }
  return(jStr)
}

newJson = sapply(1:length(jsonTrain), replaceRares)
writeLines(newJson, con="parse_train_vert_rare.dat")

