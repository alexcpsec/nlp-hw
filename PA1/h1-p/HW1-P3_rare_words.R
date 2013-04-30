# HW1-P3_rare_words.R

setwd("~/Dropbox/Courses/Coursera - NLP/nlp-hw/PA1/h1-p")

# Working with gene.train now
word_lines_train = readLines("gene.train", n = -1)
split_train = strsplit(word_lines_train, " ")

# Calculating the word frequencies
words_train = vector(mode = "character")
extractWords <- function(row) {
  return(row[1])
} 
word_train = sapply(split_train, extractWords)

freq_table_train = table(word_train)

classifyRare <- function(word) {
  if ( length(grep("[0123456789]", word)) > 0 ) return("_NUMERIC_")
  if ( length(grep("[:lower:]", word)) == 0 ) return("_ALLCAPS_")
  if ( toupper(substr(word,nchar(word),nchar(word))) == 
         substr(word,nchar(word),nchar(word))) return("_LASTCAP_")
  return("_RARE_")
}

replace_rare <- function(row) {
  if (length(row) == 0) return("")
  
  word = row[1]
  if (freq_table_train[word] < 5) {
    return(paste0(classifyRare(word), " ", row[2]))
  } else {
    return(paste0(row[1], " ", row[2]))
  }
}

word_rare_train = sapply(split_train, replace_rare)
writeLines(word_rare_train, con="gene.train.rare_p3")

