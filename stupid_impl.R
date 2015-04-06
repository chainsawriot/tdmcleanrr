require(tm)
require(ngramrr)
require(magrittr)
testdata <- c("ABC", "ADD", "ZBBA")
TermDocumentMatrix(Corpus(VectorSource(testdata)), control = list(tokenize = function(x) ngramrr(x, char = TRUE, ngmax = 3), wordLengths = c(1, Inf))) -> testtdm

rownames(testtdm) -> allterms
apply(testtdm, 1, sum)

# When x = x., x is redundant.
# ab = abc, ab should be removed
# ad = add, ad should be removed
# bb = bba, bb should be removed
# z = zb = zbb, z and zb should be removed

singleton <- allterms[grep('^.$', allterms)]

require(igraph)

