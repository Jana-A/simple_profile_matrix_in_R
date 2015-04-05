## Creating a list of nucleotide sequences of same length
sequenceStrings <- list(seq1='ACGATGACGC', seq2='GTGCAGCTCT', seq3='GCTGACGCTA', seq4='ACGATCGGCT', seq5='ACGATCGACT')
stopifnot(length(unique(lapply(sequenceStrings, nchar))) == 1)

## Next we want to create a matrix which has in every cell 1 nucleotide such that every row represents a string of sequence. So if there are 5 sequences of length 10 each, the matrix will be a 5 by 10
convertStringToVector <- function(astring){
    result <- c()
    for(i in 1:nchar(astring)){
        result[i] <- substr(astring, i, i)
    }
    return(result)
}

sequenceMatrix <- t(sapply(sequenceStrings, convertStringToVector))

## Now it is easy to compute the frequency matrix or profile matrix that counts the occurrence of the 4 nucleotides (A, C, G and T) at every position of the sequences

frequencyMatrix <- matrix(c(0), nrow=4, ncol=nchar(sequenceStrings[[1]]))
rownames(frequencyMatrix) <- c("A", "C", "G", "T")

for(i in 1:ncol(sequenceMatrix)){
    frequencyMatrix[1,i] <- length(which(sequenceMatrix[,i] == "A"))
    frequencyMatrix[2,i] <- length(which(sequenceMatrix[,i] == "C"))
    frequencyMatrix[3,i] <- length(which(sequenceMatrix[,i] == "G"))
    frequencyMatrix[4,i] <- length(which(sequenceMatrix[,i] == "T"))
}

## To get the motif or the most frequently occurring bases at every position
for(i in 1:ncol(frequencyMatrix)){
    cat(rownames(frequencyMatrix)[which.max(frequencyMatrix[,i])])
}
