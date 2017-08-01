#Scraping -- Analysis

#POS Tagging the Titles----
library(NLP)
library(openNLP)
library(openNLPmodels.en)
library(tm)
library(plyr)

# Function taking in a string and replacing the words in it with their corresponding part of speech tags, returning that new string.
justPOS <- function(x){
  z <- as.String(x)
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- annotate(z, list(sent_token_annotator, word_token_annotator))
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  a3 <- annotate(z, pos_tag_annotator, a2)
  a3w <- subset(a3, type=="word")
  tags <- sapply(a3w$features, `[[`, "POS")
  newZ <- paste(sprintf("%s/%s", z[a3w], tags), collapse=' ')
  newZ <- gsub("[A-Z]*[a-z]*/", "", newZ)
  return(newZ)
}

parse1 <- data.frame("Titles Parsed")
names(parse1) <- c("Titles Parsed")
  
for (z in titleholder$Title) {
  parse2 <- data.frame(c(justPOS(z)))
  names(parse2) <- c("Titles Parsed")
  parse1 <- rbind(parse1, parse2)
}
parse1 <- parse1[-1,,drop=F]

titleholder <- data.frame(append(titleholder, parse1, after=match("Title", names(titleholder))), check.names=FALSE)

#@Posts Finding most frequent ngrams (n=3, 4, 5) in the post content----
library(ngram)
library(stringr)

txt <- titleholder$PostContent[1:NROW(titleholder$PostContent)]
txt <- str_trim(txt)
txt <- gsub("[^[:alnum:][:space:]']", "", txt)
txt <- as.character(preprocess(as.String(txt), case="lower"))
ng3 <- ngram(txt, n=3)
ng4 <- ngram(txt, n=4)
ng5 <- ngram(txt, n=5)

dataFramedNG3 <- data.frame(get.phrasetable(ng3))
dataFramedNG3 <- data.frame(dataFramedNG3$ngrams, dataFramedNG3$freq) 
dataFramedNG3 <- dataFramedNG3[1:match(10, dataFramedNG3$dataFramedNG3.freq),] 

dataFramedNG4 <- data.frame(get.phrasetable(ng4))
dataFramedNG4 <- data.frame(dataFramedNG4$ngrams, dataFramedNG4$freq)
dataFramedNG4 <- dataFramedNG4[1:match(20, dataFramedNG4$dataFramedNG4.freq),]  

dataFramedNG5 <- data.frame(get.phrasetable(ng5))
dataFramedNG5 <- data.frame(dataFramedNG5$ngrams, dataFramedNG5$freq)
dataFramedNG5 <- dataFramedNG5[1:match(2, dataFramedNG5$dataFramedNG5.freq),]

colnames(dataFramedNG3)[1] <- "N=3.NGrams"
colnames(dataFramedNG3)[2] <- "N=3.Frequency"
colnames(dataFramedNG4)[1] <- "N=4.NGrams"
colnames(dataFramedNG4)[2] <- "N=4.Frequency"
colnames(dataFramedNG5)[1] <- "N=5.NGrams"
colnames(dataFramedNG5)[2] <- "N=5.Frequency"

# Getting first and last sentences of the posts----
txtt <- titleholder$PostContent[1:NROW(titleholder$PostContent)]
txtt <- str_trim(txtt)
firstSFrame<- data.frame("First")
names(firstSFrame) <- c("FirstSentence")

for (i in 1:NROW(txtt) ){
  firstSentence <- str_extract(txtt[i], "([^.]*\\.|[^.]*\\?)")
  firstSFrame2 <- data.frame(firstSentence)
  names(firstSFrame2) <- c("FirstSentence")
  firstSFrame <- rbind(firstSFrame, firstSFrame2)
}
firstSFrame <- firstSFrame[-1,,drop=F]

lastSFrame<- data.frame("Last")
names(lastSFrame) <- c("LastSentence")

for (i in 1:NROW(txtt) ){
  sentenceSplit <- (strsplit(as.String(txtt[i]), "((?<=\\.)|(?<=\\?))", perl=T))[[1]]
  lastSentence <- sentenceSplit[length(sentenceSplit)]
  if (lastSentence[1] == " ]" | lastSentence[1] == "]" | lastSentence[1] == ")" | lastSentence[1] == " )" | lastSentence[1] == "”") { #known issue - .] and .) - solve via lookaround?
    lastSentence <- paste0(sentenceSplit[(length(sentenceSplit))-1], sentenceSplit[(length(sentenceSplit))])
  }
  lastSFrame2 <- data.frame(lastSentence)
  names(lastSFrame2) <- c("LastSentence")
  lastSFrame <- rbind(lastSFrame, lastSFrame2)
}
lastSFrame <- lastSFrame[-1,,drop=F]
rm(firstSFrame2, lastSFrame2)

sentences <- data.frame(append(firstSFrame, lastSFrame), check.names=FALSE)

# Tagging the first and last sentences by part of speech (POS)----
firstSPOS <- data.frame("firstSPOS")
names(firstSPOS) <- c("firstSPOS")
lastSPOS <- data.frame("lastSPOS")
names(firstSPOS) <- c("lastSPOS")

for (j in sentences$FirstSentence[1:NROW(sentences$FirstSentence)]) {
 firstSPOS2 <- data.frame(c(justPOS(j)))
 names(firstSPOS2) <- names(firstSPOS)
 firstSPOS <- rbind(firstSPOS, firstSPOS2)
}
firstSPOS <- firstSPOS[-1,,drop=F]

for (k in sentences$LastSentence[1:(NROW(sentences$LastSentence))]) {
  lastSPOS2 <- data.frame(c(justPOS(k)))
  names(lastSPOS2) <- names(lastSPOS)
  lastSPOS <- rbind(lastSPOS, lastSPOS2)
}

lastSPOS <- data.frame(lastSPOS[-1,,drop=T])

sentences <- data.frame(append(sentences, firstSPOS))
sentences <- data.frame(append(sentences, lastSPOS))
colnames(sentences)[3] <- "First.Sentence.POS.Tags"
colnames(sentences)[4] <- "Last.Sentence.POS.Tags"

titleholder <- data.frame(append(titleholder,sentences))
rm(lastSPOS, firstSPOS, firstSFrame, lastSFrame, firstSPOS2, lastSPOS2)

#@NGram frequency tagging: title POS, first, first POS, last, last POS----

titlePOS <- titleholder$Titles.Parsed[1:NROW(titleholder$Titles.Parsed)]
titlePOS <- as.character(preprocess(as.String(titlePOS), case="lower"))

# Looking at the most frequent ngrams in the first sentences of the posts
fsTxt <- sentences$FirstSentence[1:NROW(sentences$FirstSentence)]
fsTxt <- gsub("[^[:alnum:][:space:]']", "", fsTxt)
fsTxt <- as.character(preprocess(as.String(fsTxt), case="lower"))

fsnng3 <- ngram(fsTxt, n=3)
fsNG3 <- data.frame(get.phrasetable(fsnng3))
fsNG3 <- data.frame(fsNG3$ngrams, fsNG3$freq)  
fsNG3 <- fsNG3[1:match(2, fsNG3$fsNG3.freq),]

fsNG4 <- data.frame(get.phrasetable(fsnng4))
fsNG4 <- data.frame(fsNG4$ngrams, fsNG4$freq)  
fsNG4 <- fsNG4[1:match(2, fsNG4$fsNG4.freq),]

# Looking at the most frequent ngrams in the last sentences of the posts
lsTxt <- sentences$LastSentence[1:NROW(sentences$LastSentence)]
lsTxt <- gsub("[^[:alnum:][:space:]']", "", lsTxt)
lsTxt <- as.character(preprocess(as.String(lsTxt), case="lower"))

lsnng3 <- ngram(lsTxt, n=3)
lsNG3 <- data.frame(get.phrasetable(lsnng3))
lsNG3 <- data.frame(lsNG3$ngrams, lsNG3$freq)  
lsNG3 <- lsNG3[1:match(3, lsNG3$lsNG3.freq),]

lsnng4 <- ngram(lsTxt, n=4)
lsNG4 <- data.frame(get.phrasetable(lsnng4))
lsNG4 <- data.frame(lsNG4$ngrams, lsNG4$freq)  
lsNG4 <- lsNG4[1:match(2, lsNG4$lsNG4.freq),]

# First sentence POS frequency
fsPTxt <- sentences$First.Sentence.POS.Tags[1:NROW(sentences$First.Sentence.POS.Tags)]
fsPTxt <- as.character(preprocess(as.String(fsPTxt), case="lower"))

fspng3 <- ngram(fsPTxt, n=3)
fspNG3 <- data.frame(get.phrasetable(fspng3))
fspNG3 <- data.frame(fspNG3$ngrams, fspNG3$freq)  
fspNG3 <- fspNG3[1:match(3, fspNG3$fspNG3.freq),]

fspng4 <- ngram(fsPTxt, n=4)
fspNG4 <- data.frame(get.phrasetable(fspng4))
fspNG4 <- data.frame(fspNG4$ngrams, fspNG4$freq)  
fspNG4 <- fspNG4[1:match(2, fspNG4$fspNG4.freq),]

# Last sentence POS frequency
lsPTxt <- sentences$Last.Sentence.POS.Tags[1:NROW(sentences$Last.Sentence.POS.Tags)]
lsPTxt <- as.character(preprocess(as.String(lsPTxt), case="lower"))

lspng3 <- ngram(lsPTxt, n=3)
lspNG3df <- data.frame(get.phrasetable(lspng3))
lspNG3df <- data.frame(lspNG3df$ngrams, lspNG3df$freq)  
lspNG3df <- lspNG3df[1:match(3, lspNG3df$lspNG3df.freq),]

lspng4 <- ngram(lsPTxt, n=4)
lspNG4df <- data.frame(get.phrasetable(lspng4))
lspNG4df <- data.frame(lspNG4df$ngrams, lspNG4df$freq)
lspNG4df <- lspNG4df[1:match(3, lspNG4df$lspNG4df.freq),]

colnames(fsNG3)[1] <- "FirstSentence.N=3.NGrams"
colnames(fsNG3)[2] <- "FirstSentence.N=3.Frequency"
colnames(fspNG3)[1] <- "FirstSentence.POS.N=3.NGrams"
colnames(fspNG3)[2] <- "FirstSentence.POS.N=3.Frequency"
colnames(fsNG4)[1] <- "FirstSentence.N=4.NGrams"
colnames(fsNG4)[2] <- "FirstSentence.N=4.Frequency"
colnames(fspNG4)[1] <- "FirstSentence.POS.N=4.NGrams"
colnames(fspNG4)[2] <- "FirstSentence.POS.N=4.Frequency"
colnames(lsNG3)[1] <- "LastSentence.N=3.NGrams"
colnames(lsNG3)[2] <- "LastSentence.N=3.Frequency"
colnames(lspNG3df)[1] <- "LastSentence.POS.N=3.NGrams"
colnames(lspNG3df)[2] <- "LastSentence.POS.N=3.Frequency"
colnames(lsNG4)[1] <- "LastSentence.N=4.NGrams"
colnames(lsNG4)[2] <- "LastSentence.N=4.Frequency"
colnames(lspNG4df)[1] <- "LastSentence.POS.N=4.NGrams"
colnames(lspNG4df)[2] <- "LastSentence.POS.N=4.Frequency"

#Writing to excel----
library(xlsx) 
wb = createWorkbook()
sheet = createSheet(wb, "Blogposts")
addDataFrame(titleholder, sheet=sheet, startColumn=1, row.names=FALSE)

sheet = createSheet(wb, "Post Content NGrams")
addDataFrame(dataFramedNG3, sheet=sheet, startColumn=1, row.names=FALSE)
addDataFrame(dataFramedNG4, sheet=sheet, startColumn=3, row.names=FALSE)
addDataFrame(dataFramedNG5, sheet=sheet, startColumn=5, row.names=FALSE)

sheet = createSheet(wb, "nGram Frequency")
addDataFrame(fsNG3, sheet=sheet, startColumn=1, row.names=FALSE)
addDataFrame(fspNG3, sheet=sheet, startColumn=3, row.names=FALSE)
addDataFrame(fsNG4, sheet=sheet, startColumn=5, row.names=FALSE)
addDataFrame(fspNG4, sheet=sheet, startColumn=7, row.names=FALSE)
addDataFrame(lsNG3, sheet=sheet, startColumn = 9, row.names=FALSE)
addDataFrame(lspNG3df, sheet=sheet, startColumn=11, row.names=FALSE)
addDataFrame(lsNG4, sheet=sheet, startColumn=13, row.names=FALSE)
addDataFrame(lspNG4df, sheet=sheet, startColumn=15, row.names=FALSE)

saveWorkbook(wb, "Blogposts IX.xlsx")