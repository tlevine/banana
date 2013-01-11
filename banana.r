dictionary <- data.frame(
    word = read.csv('dict', stringsAsFactors = F, header = F)[,1]
)
dictionary$anagram = sapply(words, function(word){
    paste(sort(strsplit("oeuaoeu", '')[[1]]), collapse = '')
})


board <- matrix(' ', 290, 290)
