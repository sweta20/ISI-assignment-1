library("tm")
library(RWeka)
TEXTFILE = "/media/sweta/7A12BD8F12BD50BD/8th sem/Intelligent system and interfaces/assignment_1/austen-emma.txt"
shakespeare = readLines(TEXTFILE)
length(shakespeare)

shakespeareWords <- sum(sapply(gregexpr("\\S+", shakespeare), length))
tt = Corpus(VectorSource(shakespeare))
cleanSample <- tm_map(tt, content_transformer(tolower))
cleanSample <- tm_map(cleanSample, content_transformer(removePunctuation))
cleanSample <- tm_map(cleanSample, content_transformer(removeNumbers))
cleanSample <- tm_map(cleanSample, stripWhitespace)
cleantext<-data.frame(text=unlist(sapply(cleanSample, `[`, "content")), stringsAsFactors=F)

onetoken <- NGramTokenizer(cleantext, Weka_control(min = 1, max = 1))
bitoken <- NGramTokenizer(cleantext, Weka_control(min = 2, max = 2, delimiters = " \\r\\n\\t.,;:\"()?!"))
tritoken <- NGramTokenizer(cleantext, Weka_control(min = 3, max = 3, delimiters = " \\r\\n\\t.,;:\"()?!"))

one <- data.frame(table(onetoken))
two <- data.frame(table(bitoken))
tri <- data.frame(table(tritoken))

onesorted <- one[order(one$Freq,decreasing = TRUE),]
twosorted <- two[order(two$Freq,decreasing = TRUE),]
trisorted <- tri[order(tri$Freq,decreasing = TRUE),]
one30 <- onesorted[1:30,]
colnames(one30) <- c("Word","Frequency")
head (one30)

two30 <- twosorted[1:20,]
colnames(two30) <- c("Word","Frequency")
head (two30)

tri30 <- trisorted[1:10,]
colnames(tri30) <- c("Word","Frequency")
head (tri30)

ggplot(one30, aes(x=Word,y=Frequency), ) + geom_bar(stat="Identity", fill="blue") +geom_text(aes(label=Frequency), vjust=-0.2)
ggplot(two30, aes(x=Word,y=Frequency), ) + geom_bar(stat="Identity", fill="blue") +geom_text(aes(label=Frequency), vjust=-0.2)
ggplot(tri30, aes(x=Word,y=Frequency), ) + geom_bar(stat="Identity", fill="blue") +geom_text(aes(label=Frequency), vjust=-0.2)
