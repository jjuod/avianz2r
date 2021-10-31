## Internal - cleanly format a message string
## (useful when listing an explanation and a vector of bad ids/examples)
txtmsg <- function(...){
    paste(c(...), collapse=" ")
}
