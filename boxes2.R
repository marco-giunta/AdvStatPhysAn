# Marco Giunta, exercise 2 implemented properly

library(tidyverse)

compute.posterior <- function(prior, data) { # B.T.
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

# Simulation
set.seed(1234)

n.white <- sample(0:5)[1]
print(sprintf("The unknown box contains %d white balls and %d black balls (hypothesis H%d)", n.white, (5-n.white), n.white))
box <- c(rep(0, times = n.white), rep(1, times = 5-n.white))
rbox <- function(box) sample(box)[1]
colors.array <- c("white", "black")

prior <- rep(1, times = 6)/6
df <- data.frame(matrix(0, nrow = 1, ncol = 6))
for (i in 0:5) names(df)[i+1] <- sprintf("H%d",i) 
df[1,] <- prior

verbose <- FALSE #TRUE

n.draws <- 10
print(sprintf("Simulating %d draws", n.draws))
if (verbose) {
    print("prior:")
    print(prior)
} 

extracted.colors.history <- c(NA)

for (i in 1:10) {
    extracted.color <- rbox(box)
    extracted.colors.history[i+1] <- extracted.color
    print(sprintf("%s ball extracted", colors.array[extracted.color+1]))
    
    prior <- unlist(tail(df, 1)) # running prior
    posterior <- compute.posterior(prior, data = extracted.color)
    df[length(unlist(df[1]))+1,] <- posterior
    
    print(plot.posterior(posterior, i))
    
    if (verbose) {
        print("running posterior:")
        print(posterior)
    }

    Sys.sleep(1.5) # we wait 1.5 seconds to have enough time to appreciate each plot
}

event.col <- rep(NA, times = length(extracted.colors.history))
event.col[extracted.colors.history == 1] = "B"
event.col[extracted.colors.history == 0] = "W"

print("Inference history:")
print(cbind(E = event.col, df))

plot.posterior(posterior, n.draws)