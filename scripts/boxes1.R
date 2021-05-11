# Marco Giunta, exercise 1 implemented properly

library(tidyverse)

compute.posterior <- function(prior, data) {
    likelihood <- abs(data - 0:5/5)
    likelihood.times.prior <- likelihood*prior
    evidence <- sum(likelihood.times.prior)
    posterior <- likelihood.times.prior/evidence
    return (posterior)
}

get.user.input <- function() {
    input <- readline(prompt = 'enter extracted color ("w" or "b") or "stop" ')
    if (input == "w") x <- 0
    else {
        if (input == "b") x <- 1
        else x <- NA
    }
    if (input == "stop") proceed <- FALSE
    else proceed <- TRUE
    
    return (c(x, proceed))
}

plot.posterior <- function(posterior, index) {
    df.ggplot <- data.frame(0:5, posterior)
    names(df.ggplot) <- c("hypothesis", "posterior")
    ggplot(data = df.ggplot, aes(x = hypothesis, y = posterior)) + geom_point() + geom_line() + ggtitle(sprintf("Extraction n. %d", index))
}

proceed <- TRUE
prior <- rep(1, times = 6)/6

df <- data.frame(matrix(0, nrow = 1, ncol = 6))
for (i in 0:5) names(df)[i+1] <- sprintf("H%d",i) 
df[1,] <- prior
extracted.colors.history <- c(NA)

#x11()
while (proceed) {
    v <- get.user.input()
    extracted.color <- v[1]; proceed <- v[2]
    #print(x)
    if (!is.na(extracted.color)) {
        prior <- unlist(tail(df, 1)) # running prior
        posterior <- compute.posterior(prior, data = extracted.color)
        
        new.row.index <- length(unlist(df[1]))+1
        df[new.row.index,] <- posterior
        print("running posterior:")
        print(posterior); flush.console()
        extracted.colors.history[new.row.index] <- extracted.color
        print(plot.posterior(posterior, new.row.index-1)) # https://stackoverflow.com/questions/15678261/ggplot-does-not-work-if-it-is-inside-a-for-loop-although-it-works-outside-of-it
    }
}

event.col <- rep(NA, times = new.row.index)
event.col[extracted.colors.history == 1] = "B"
event.col[extracted.colors.history == 0] = "W"

print("Inference history:")
print(cbind(E = event.col, df))
