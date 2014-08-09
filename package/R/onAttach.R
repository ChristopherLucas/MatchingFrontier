.onAttach <- function(lib, pkg){
    dcf <- read.dcf(file.path(lib, pkg, "DESCRIPTION"))

    version.msg <- paste('\n', 'Loading MatchingFrontier Version ', dcf[, 'Version'], sep = '')

    cite.msg <- "King, Gary, Christopher Lucas, and Richard Nielsen. 2014. \"Optimizing Balance and Sample Size in Matching Methods for Causal Inference.\" Working paper."
    cite.msg <- paste(strwrap(cite.msg), collapse = "\n")

    bib.msg <- "@article{King14,\n\ttitle={Optimizing Balance and Sample Size in Matching Methods for Causal Inference},\n\tauthor={King, Gary and Lucas, Christopher and Nielsen, Richard},\n\tjournal={Working Paper},\n\tyear={2014}\n}\n"
        
    cite.msg <- paste('## Citation ##\n', cite.msg, sep = '')

    bib.msg <- paste('## BibTeX ##\n', bib.msg, sep = '')
    msg <- paste(version.msg, cite.msg, bib.msg, sep = '\n\n')
    packageStartupMessage(msg)
}
