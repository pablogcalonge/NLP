install.packages("quanteda")
install.packages("quanteda.textmodels")
install.packages("quanteda.textstats")
install.packages("quanteda.textplots")
install.packages("udpipe")
install.packages("spacyr")
install.packages("tidyverse")
install.packages("stopwords")
install.packages("python_executable")

library(tm)
library(ggplot2)
library(wordcloud)
library(RWeka)
library(reshape2)
library(udpipe)
library(spacyr)
library(utf8)
library(tidyverse)
library(stopwords)
library(spacyr)


urlRepo <- "https://raw.githubusercontent.com/pablogcalonge/NLP/main/NLP_NBA.txt"


lines <- readLines(urlRepo, encoding="UTF-8")

grep(pattern = "S1", lines, fixed = TRUE)

#CUantas lineas hay => 257
length(lines)

#Todas las lineas estan en UFT-8 => character(0)
lines[!utf8_valid(lines)]

linesQ_NFC <- utf8_normalize(lines)

sum(linesQ_NFC != lines)


limpiezaInicial <- as.data.frame(linesQ_NFC)
limpiezaInicial<-subset(limpiezaInicial, linesQ_NFC!="")
library(stringr)
listaLimpiezaInicial <- NULL
for (i in seq_len(nrow(limpiezaInicial))) {
  listaLimpiezaInicial <- c(listaLimpiezaInicial, str_replace_all(limpiezaInicial[i,1], "[Ss0-9]+: ", ""))
}
limpiezaInicial$linesQ_NFC <- listaLimpiezaInicial


stringQ <- paste(lines, collapse = "\n") 

paragraphs <- unlist(strsplit(stringQ, "\\n"))

parEmpty <- which(paragraphs == "")
length(paragraphs)

####### PoS #######
library(udpipe)
model <- udpipe_download_model("english-ewt") 
udmodel_es <- udpipe_load_model(file = model$file_model)
txt <- c(linesQ_NFC)
anno <- udpipe_annotate(udmodel_es, x = txt)
df <- as.data.frame(anno)

df <- subset(df, lemma!="s1")
df <- subset(df, lemma!="s2")
df <- subset(df, lemma!="s3")
df <- subset(df, lemma!="S3")
df <- subset(df, lemma!="s4")
df <- subset(df, lemma!="s5")
df <- subset(df, lemma!="S5")
df <- subset(df, lemma!="s6")
df <- subset(df, lemma!="s7")
df <- subset(df, lemma!="s8")
df <- subset(df, lemma!="s9")
df <- subset(df, lemma!="s10")
df <- subset(df, lemma!="s11")
df <- subset(df, lemma!="s12")
df <- subset(df, lemma!=":")
df <- subset(df, lemma!=".")
df <- subset(df, lemma!=",")

library(lattice)
stats <- txt_freq(df$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")

## PRONOUN
stats <- subset(df, upos %in% c("PRON")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most occurring pronoun", xlab = "Freq")

# NOUNS
stats <- subset(df, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most occurring nouns", xlab = "Freq")


sportLeaguesNBA <- subset(df, lemma=="NBA")
sportLeaguesNFL <- subset(df, lemma=="NFL")
sportLeaguesNHL <- subset(df, lemma=="NHL")
sportLeagues <- rbind(sportLeaguesNBA, sportLeaguesNFL)#, sportLeaguesNHL)
sportLeagues <- rbind(sportLeagues, sportLeaguesNHL)

stats <- subset(sportLeagues) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most occurring League", xlab = "Freq")

