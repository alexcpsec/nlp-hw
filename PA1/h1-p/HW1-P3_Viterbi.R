## HW1-P2_Viterbi.R

library(hash)
setwd("~/Dropbox/Courses/Coursera - NLP/nlp-hw/PA1/h1-p")

emission_count_train = readLines("gene.train.rare_p3.counts", n = -1)
split_emission = strsplit(emission_count_train, " ")
words_train = readLines("gene.train.rare_p3", n = -1)
split_train = strsplit(words_train, " ")

classifyRare <- function(word) {
  if ( length(grep("[0123456789]", word)) > 0 ) return("_NUMERIC_")
  if ( length(grep("[:lower:]", word)) == 0 ) return("_ALLCAPS_")
  if ( toupper(substr(word,nchar(word),nchar(word))) == 
         substr(word,nchar(word),nchar(word))) return("_LASTCAP_")
  return("_RARE_")
}

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

e <- function(word, nonterm) {
  if (nonterm == "O") e_word = e_word_O
  if (nonterm == "I-GENE") e_word = e_word_I_GENE
  if (is.na(count_words[word])) word = classifyRare(word) # rare words, not found in train
  
  if (has.key(word, e_word)) {
    return(e_word[[word]])
  } else {
    return(0)
  }
}

## Calculating the 2-Gram and 3-Gram counts
count_2gram <- hash()
count_3gram <- hash()
calc2Gram <- function(row) {
  if (length(row) > 0 && row[2] == "2-GRAM") {
    key = paste0(row[3], ";", row[4]) 
    count_2gram[[key]] = as.numeric(row[1])
  }
}

calc3Gram <- function(row) {
  if (length(row) > 0 && row[2] == "3-GRAM") {
    key = paste0(row[3], ";", row[4], ";", row[5]) 
    count_3gram[[key]] = as.numeric(row[1])
  }
}

sapply(split_emission, calc2Gram)
sapply(split_emission, calc3Gram)

q <- function(yi_2, yi_1, yi) {
  key_2gram = paste0(yi_2, ";", yi_1)
  key_3gram = paste0(yi_2, ";", yi_1, ";", yi)
  
  if(has.key(key_3gram, count_3gram)) {
    retval = count_3gram[[key_3gram]] / count_2gram[[key_2gram]]
  } else {
    retval = 0
  }
  return(retval)
}

createSentencesList <- function(word_vector) {
  sentence_list = list()
  list_idx = 1
  sentence_list[[list_idx]] = vector(mode = "character")
  vec_idx = 1
  for (i in 1:length(word_vector)) {
    if (word_vector[i] == "") {
      list_idx = list_idx + 1
      sentence_list[[list_idx]] = vector(mode = "character")
      vec_idx = 1
      next
    }
    sentence_list[[list_idx]][vec_idx] = word_vector[i]
    vec_idx = vec_idx + 1
  }
  return(sentence_list)
}

S = c("O", "I-GENE")

viterbi <- function(sentence, pi, bp) {
  n = length(sentence)
  tags = vector(mode = "character", length = n)
  for (k in 1:n) {
    # Setting the Sk_2, Sk_1 and Sk
    Sk = Sk_1 = Sk_2 = S
    if (k == 1) { Sk_1 = Sk_2 = "*"}
    if (k == 2) { Sk_2 = "*"}
    for (u in Sk_1) {
      for (v in Sk) {
        bp[as.character(k), u, v] = "O"
        for (w in Sk_2) {
          val = pi[as.character(k-1), w, u] * q(w,u,v) * e(sentence[k], v)
          if (val > pi[as.character(k), u, v]) {
            pi[as.character(k), u, v] = val
            bp[as.character(k), u, v] = w
          }
        }
      }
    }
  }
  maxSTOP = 0
  tags[n-1] = "O"
  tags[n] = "O"
  for (u in Sk) {
    for (v in Sk) {
      val = pi[as.character(n), u, v] * q(u, v, "STOP")
      if (val > maxSTOP) {
        maxSTOP = val
        tags[n-1] = u
        tags[n] = v
      }
    }
  }
  
  if (n > 2) {
    for (k in (n-2):1) {
      tags[k] = bp[as.character(k+2), tags[k+1], tags[k+2]]
    }
  }
  return(tags)
}  

applyViterbi <- function(sentence) {
  if(length(sentence) == 0) return("")
  pi = array(rep(0), 
             dim = c(length(sentence) +1, length(S) +1, length(S) +1),
             dimnames = list(as.character(0:length(sentence)), c("*", S), c("*", S) ))
  pi["0","*","*"] = 1
  bp = array(rep(0), 
             dim = c(length(sentence) +1 , length(S) +1, length(S) +1),
             dimnames = list(as.character(0:length(sentence)), c("*", S), c("*", S) ))
  
  tags = viterbi(sentence, pi, bp)
}

# Running against gene.dev
#words_dev = readLines("gene.dev", n = -1)
#sentence_dev = createSentencesList(words_dev)
#tags_dev = lapply(sentence_dev, applyViterbi)

#output_dev = vector(mode = "character", length = length(words_dev))
#output_index = 1

#for (i in 1:length(sentence_dev)) {
#  len = length(sentence_dev[[i]])
#  if (len > 0) {
#    output_dev[output_index:(output_index+len-1)] = paste0(sentence_dev[[i]], " ", tags_dev[[i]])
#    output_dev[(output_index+len)] = ""
#    output_index = output_index + len + 1
#  }
#}

#writeLines(output_dev, con="gene_dev.p3.out")

# Running against gene.test
words_test = readLines("gene.test", n = -1)
sentence_test = createSentencesList(words_test)
tags_test = lapply(sentence_test, applyViterbi)

output_test = vector(mode = "character", length = length(words_test))
output_index = 1

for (i in 1:length(sentence_test)) {
  len = length(sentence_test[[i]])
  if (len > 0) {
    output_test[output_index:(output_index+len-1)] = paste0(sentence_test[[i]], " ", tags_test[[i]])
    output_test[(output_index+len)] = ""
    output_index = output_index + len + 1
  }
}

writeLines(output_test, con="gene_test.p3.out")