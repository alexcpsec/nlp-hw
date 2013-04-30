# HW1-P1_rare_words.R

setwd("~/Dropbox/Courses/Coursera - NLP/nlp-hw/PA1/h1-p")
# Open the gene.dev file, convert to gene.dev.rare:
word_lines_dev = readLines("gene.dev", n = -1)
freq_table_dev = table(word_lines_dev)

for (i in 1:length(word_lines_dev)) {
  if (word_lines_dev[i] == "") next
  if (freq_table_dev[word_lines_dev[i]] < 5) {
    word_lines_dev[i] = "_RARE_"
  }
}

writeLines(word_lines_dev, con="gene.dev.rare")

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

replace_rare <- function(row) {
  if (length(row) == 0) return("")
  if (freq_table_train[row[1]] < 5) {
    return(paste0("_RARE_", " ", row[2]))
  } else {
    return(paste0(row[1], " ", row[2]))
  }
}

word_rare_train = sapply(split_train, replace_rare)
writeLines(word_rare_train, con="gene.train.rare")


