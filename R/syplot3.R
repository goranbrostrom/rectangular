syplot3 <- function(live = TRUE){
    library(Sweden)
    dd <- getDeaths(ages = 60:100)
    pp <- getPop(ages = 60:100, years = 1968:2014)
    haz <- dd / pp
    Haz <- apply(haz, 2, cumsum)
    S <- exp(-Haz)
    dd <- haz * S
    ##return(Haz)
    oldpar <- par(mfrow = c(1, 2))
    on.exit(par(oldpar))
    if (live) {
        tsize <- 2
        sequence <- 2:NCOL(dd)
    }else{
        tsize <- 1
        sequence <- NCOL(dd)
    }
    for(i in sequence){
        plot(dd[, 1], type = "s", col = 1, ylim = c(0, 0.042),
             xlab = "Age", ylab = "Density", main = "Sweden, death ages")
        text(40, 0.02, as.character(1968 + i - 1), cex = tsize, col = "red")
        text(40, 0.03, as.character(1968), cex = tsize, col = "black")
        abline(h = 0)
        lines(dd[, i], type = "s", col = "red")
        plot(0:101, c(1, S[, 1]), type = "s", col = 1, ylim = c(0, 1),
             xlab = "Age", ylab = "Surviving fraction",
             main = "Sweden, death ages")
        text(40, 0.5, as.character(1968 + i - 1), cex = tsize, col = "red")
        text(40, 0.7, as.character(1968), cex = tsize, col = "black")
        abline(h = 0, v = 0)
        lines(0:101, c(1, S[, i]), type = "s", col = "red", ylim = c(0, 0.042))
        Sys.sleep(0.1)
    }
    
}
