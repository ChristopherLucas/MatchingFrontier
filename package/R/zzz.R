.onAttach <- function(lib, pkg){
    dcf <- read.dcf(file.path(lib, pkg, "DESCRIPTION"))

    version.msg <- paste('\n', 'Loading MatchingFrontier Version ', dcf[, 'Version'], sep = '')
    
    cite.msg <- paste("King, Gary, Christopher Lucas, and Richard Nielsen. \"MatchingFrontier: R Package for Computing the Matching Frontier.\" R package version ", dcf[, 'Version'], '.', sep = '')
    cite.msg <- paste(strwrap(cite.msg), collapse = "\n")

    version.ref <- paste('R package version', dcf[, 'Version'])
    
    bib.msg <- paste("@article{King14,\n\ttitle={MatchingFrontier: R Package for Computing the Matching Frontier},\n\tauthor={King, Gary and Lucas, Christopher and Nielsen, Richard},\n\tyear={2014},\n\tnote={", version.ref, "}\n}\n")
        
    cite.msg <- paste('## Citation ##\n', cite.msg, sep = '')

    bib.msg <- paste('## BibTeX ##\n', bib.msg, sep = '')
    msg <- paste(version.msg, cite.msg, bib.msg, sep = '\n\n')
    packageStartupMessage(msg)
}
