## NLP
## HW 2 - Part 2
## CKY Algorithm

setwd("~/Dropbox/Courses/Coursera - NLP/nlp-hw/PA2/assignment")
library(hash)
library(compiler)
compilePKGS(T)
enableJIT(2)

fileGrammar = readLines("parse_train.counts.out", n = -1)
splitGrammar = strsplit(fileGrammar, " ")

## Calculating the counts
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

sapply(splitGrammar, extractWords)

freqWords = values(hashWords)

## Calulating the distributions
hashNonterm = hash()
countNonterm = function(row) {
  if (row[2] == "NONTERMINAL") {
    key = row[3]
    hashNonterm[[key]] = as.numeric(row[1])
  }
}

hashUnary = hash()
countUnary = function(row) {
  if (row[2] == "UNARYRULE") {
    key = paste0(row[3], "%%", row[4])
    hashUnary[[key]] = as.numeric(row[1])
  }
}

hashBinary = hash()
hashRules = hash()
countBinary = function(row) {
  if (row[2] == "BINARYRULE") {
    key = paste0(row[3], "%%", row[4], "%%", row[5])
    hashBinary[[key]] = as.numeric(row[1])
    
    if (has.key(row[3], hashRules)) {
      ruleList = hashRules[[row[3]]]
      ruleList = c(ruleList, key)
      hashRules[[row[3]]] = ruleList
    } else {
      hashRules[[row[3]]] = key
    }
  }
}

sapply(splitGrammar, countNonterm)
sapply(splitGrammar, countUnary)
sapply(splitGrammar, countBinary)

q_emission = function(X, w) {
  if (is.na(freqWords[w])) w = "_RARE_"
  key = paste0(X, "%%", w)
  if (has.key(key, hashUnary)) {
    hashUnary[[key]] / hashNonterm[[X]]
  } else { 0 }
}

q_rule = function(X, Y1, Y2) {
  key = paste0(X, "%%", Y1, "%%", Y2)
  if (has.key(key, hashBinary)) {
    hashBinary[[key]] / hashNonterm[[X]]
  } else { 0 }
}

N = keys(hashNonterm)



CKY <- function(sentence) {
  print(paste(sentence))
  ## Creating the Pi matrix
  pi = array(rep(0), 
             dim = c(length(sentence), length(sentence), length(N)),
             dimnames = list(1:length(sentence), 1:length(sentence), N))
  bps = array(rep(0), 
             dim = c(length(sentence), length(sentence), length(N)),
             dimnames = list(1:length(sentence), 1:length(sentence), N))
  bprule = array(rep(0), 
              dim = c(length(sentence), length(sentence), length(N)),
              dimnames = list(1:length(sentence), 1:length(sentence), N))
  
  
  ## Starting up the pi matrix
  for (i in 1:length(sentence)) {
    word = sentence[i]
    for (j in 1:length(N)) {
      symbol = N[j]
      value = q_emission(symbol, word)
      if (value > 0) {
        pi[i,i,symbol] = value
      }
    }
  }
  
  # calculating CKY algorithm
  n = length(sentence)
  for (l in 1:(n-1)) {
    for (i in 1:(n-l)) {
      j = i + l
      for (symbol in N) {
        for (rule in keys(hashBinary)) {
          listRule = strsplit(rule, "%%")
          X = listRule[[1]][1]; Y = listRule[[1]][2]; Z = listRule[[1]][3];
          if (X == symbol) {
            for (s in i:(j-1)) {
              val = q_rule(X, Y, Z) * pi[i,s,Y] * pi[s+1,j,Z]
              if ( pi[i,j,X] < val ) {
                pi[i,j,X] = val
                bps[i,j,X] = s
                bprule[i,j,X] = rule
              }
            }
          }
        }
      }
    }
  }
  
  exportJson <- function(i, j, symbol) {
    if (i == j) {
      return(paste0("[\"", symbol, "\", \"", sentence[i], "\"]"))
    } else {
      s = bps[i,j,symbol]
      listRule = strsplit(bprule[i,j,symbol], "%%")
      Y = listRule[[1]][2]; Z = listRule[[1]][3];
      str = paste0("[\"", symbol, "\", ",
                   exportJson(i,s,Y), ", ",
                   exportJson(s+1,j,Z), "]")
      return(str)
    }
  }
  exportJson(1,n, "SBARQ")
}

fileSentence = readLines("parse_dev.dat", n = -1)
splitSentence = strsplit(fileSentence, " ")
parsedList = lapply(splitSentence, CKY)
parsedFile = unlist(parsedList)
writeLines(parsedFile, con="parse_dev.p2.out")

fileSentence = readLines("parse_test.dat", n = -1)
splitSentence = strsplit(fileSentence, " ")
parsedList = lapply(splitSentence, CKY)
parsedFile = unlist(parsedList)
writeLines(parsedFile, con="parse_test.p2.out")
