require(tm)
require(ngramrr)
require(magrittr)
testdata <- c("ABC", "ADD", "ZBBA", "ABD")
nirvana <- c("hello hello hello how low", "hello hello hello how low",
"hello hello hello how low", "hello hello hello",
"with the lights out", "it's less dangerous", "here we are now", "entertain us",
"i feel stupid", "and contagious", "here we are now", "entertain us",
"a mulatto", "an albino", "a mosquito", "my libido", "yeah", "hey yay")

chinese <- c("中華人民共和國","香港特別行政區","你老母","香港")
chinese2 <- c("中華人民共和國","香港特別行政區","你老母","香港", "中華民國", "香港行政長官","你老母臭大你", "香港大學", "中文大學", "香港中文大學")

# expected output should be
# "中華人民共和國" x1 "香港" x2 "特別行政區" x1 "你老母" x1


TermDocumentMatrix(Corpus(VectorSource(chinese2)), control = list(tokenize = function(x) ngramrr(x, char = TRUE, ngmax = 10), wordLengths = c(1, Inf))) -> testtdm

frequency <- apply(testtdm, 1, sum)

# When x = x., x is redundant.
# ab = abc, ab should be removed
# ad = add, ad should be removed
# bb = bba, bb should be removed
# z = zb = zbb, z and zb should be removed

singleton <- allterms[grep('^.$', allterms)]


require(tau)
sum(frequency[grep("^a.$", names(frequency))])

isRedundantChild <- function(x, freq) {
    x == sum(freq[grep(paste0("^",names(x),".$"), names(freq))])
}


isRedundantParent <- function(x, freq) {
    x == sum(freq[grep(paste0("^.",names(x),"$"), names(freq))])
}



uniqueterms <- function(frequency, redunantfx) {
    return(frequency[!sapply(1:length(frequency), function(x) redunantfx(frequency[x], freq = frequency))])
}

frequency <- apply(testtdm, 1, sum)
x <- uniqueterms(frequency, isRedundantChild)
y <- uniqueterms(x, isRedundantParent)
