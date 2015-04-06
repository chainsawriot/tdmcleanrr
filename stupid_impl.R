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

TermDocumentMatrix(Corpus(VectorSource(chinese)), control = list(tokenize = function(x) ngramrr(x, char = TRUE, ngmax = 10), wordLengths = c(1, Inf))) -> testtdm

frequency <- apply(testtdm, 1, sum)

# When x = x., x is redundant.
# ab = abc, ab should be removed
# ad = add, ad should be removed
# bb = bba, bb should be removed
# z = zb = zbb, z and zb should be removed

singleton <- allterms[grep('^.$', allterms)]


require(tau)
sum(frequency[grep("^a.$", names(frequency))])

isRedundant <- function(x, freq) {
    x == sum(freq[grep(paste0("^",names(x),".$"), names(freq))])
}


uniqueterms <- function(tdm) {
    frequency <- apply(tdm, 1, sum)
    return(names(frequency[!sapply(1:length(frequency), function(x) isRedundant(frequency[x], freq = frequency))])
)
}

uniqueterms(testtdm)

