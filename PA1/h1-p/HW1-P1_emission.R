## HW1-P1_emission.R

library(hash)
setwd("~/Dropbox/Courses/Coursera - NLP/nlp-hw/PA1/h1-p")

emission_count_train = readLines("gene.train.rare.counts", n = -1)
split_emission = strsplit(emission_count_train, " ")
words_train = readLines("gene.train.rare", n = -1)
split_train = strsplit(words_train, " ")

# Calculating the NonTerminals count
extractNonterm <- function(row) {
  if (length(row) > 0) {
    return(row[2])  
  } else {
    return("")
  }
} 
nonterm_train = sapply(split_train, extractNonterm)
count_nonterm = table(nonterm_train)

# Extracting the Word List
extractWords <- function(row) {
  return(row[1])
} 
word_vector = sapply(split_train, extractWords)
count_words = table(word_vector)

## Calculating the e(x|y) = emission counts / count_nonterm
e_word_O <- hash()
e_word_I_GENE <- hash()
emission_count <- function(row, nonTerm, hash_count) {
  if (length(row) > 0 && row[2] == "WORDTAG") {
    if (row[3] == nonTerm) {
      hash_count[[row[4]]] = as.numeric(row[1]) / count_nonterm[nonTerm]
    }
  }
}

sapply(split_emission, emission_count, nonTerm = "O", hash_count = e_word_O)
sapply(split_emission, emission_count, nonTerm = "I-GENE", hash_count = e_word_I_GENE)

getTagProb <- function(word, tag_hash) {
  if (has.key(word, tag_hash)) {
    return(tag_hash[[word]])
  } else {
    return(0)
  }
}

getWordTags <- function(word) {
  if (word == "") return("")  
  q_word = word
  if (is.na(count_words[word])) q_word = "_RARE_" # rare words, not found in train
  
  O_prob = getTagProb(q_word, e_word_O)
  I_GENE_prob = getTagProb(q_word, e_word_I_GENE)
  if(I_GENE_prob > O_prob) {
    return(paste0(word, " I-GENE"))
  } else {
    return(paste0(word, " O"))
  }
}

# Running against gene.dev
words_dev = readLines("gene.dev", n = -1)
words_tagged = sapply(words_dev, getWordTags)
writeLines(words_tagged, con="gene_dev.p1.out")

# Running against gene.test
words_dev = readLines("gene.test", n = -1)
words_tagged = sapply(words_dev, getWordTags)
writeLines(words_tagged, con="gene_test.p1.out")