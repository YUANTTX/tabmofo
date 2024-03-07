#' varsVenn
#'
#' This is varsVenn.
#'
#' @param indat A list of sets
#' @return A table
varsVenn <- function(indat) {
    SamNum <- length(indat)
    SamNM <- names(indat)
    inall <- c()
    for (i in seq_len(SamNum)) {
        assign(paste0("len", i), indat[[i]])
        inall <- unique(c(inall, get(paste0("len", i))))
    }
    medat <- data.frame(.allvars = inall)
    rownames(medat) <- medat$.allvars
    for (k in seq_len(SamNum)) {
        checkVars <- get(paste0("len", k))
        medat <- within(medat, {
            tempname <- ifelse(inall %in% checkVars, 1, 0) # nolint
        })
        names(medat)[k + 1] <- SamNM[k]
    }
    medat <- medat[, -1]
    medat <- cbind(medat, RowSum = rowSums(medat))
    return(medat)
}


#' char2numf
#'
#' This is char2numf. charatrs to numbers
#'
#' @param indat A list of sets
#' @return A table
char2numf <- function(indat) {
    NumDat <- indat
    NumDat <- apply(as.matrix(NumDat), 2, function(x) as.numeric(x))
    NumDat <- as.data.frame(NumDat)
    return(NumDat)
}


#' replace the NA
#'
#' This is char2numf. charatrs to numbers
#'
#' @param indat A list of sets
#' @return A table
NA2Numf <- function(indat) {
    NADat <- as.data.frame(indat)
    NADat <- apply(NADat, 2, function(x) {
        x[is.na(x)] <- mean(x, na.rm = TRUE)
    })
}