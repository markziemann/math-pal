clc <- function() cat(rep("\n", 50))
args = commandArgs(trailingOnly=TRUE)

# fix Rscript mode
user.input <- function(prompt) {
  if (interactive()) {
    return(readline(prompt))
  } else {
    cat(prompt)
    return(readLines("stdin", n=1))
  }
}

clc()

if (length(args)==0) {
  message("Welcome to Times Tables Trainer, by Mark Ziemann.")
  Sys.sleep(1)
  message("")
  myname <- user.input(prompt="What is your name?: ")
} else {
  myname <- paste(args,"")
}
message("")
message("Hello ", myname,", let's practice some times tables!!!")
Sys.sleep(1)

message("")
message("If you ever need to exit the program, press Ctrl+C to get your R cursor back.")
Sys.sleep(1)

message("")
suppressWarnings(quiztype <- as.integer(user.input(prompt="Which mode do you want, set time (1) or set number of questions (2)?: ")))
Sys.sleep(1)

if ( quiztype != 1 & quiztype != 2 ) {
  message("Sorry I didn't understand that. Your answer needs to be a number 1 or 2.")
  suppressWarnings(quiztype <- as.integer(user.input(prompt="Which mode do you want, set time (1) or set number of questions (2)?: ")))
  Sys.sleep(1)
}


##### Set amount of time
if ( quiztype == 1 ) {

message("")
suppressWarnings(maxtime <- as.numeric(user.input(prompt="How much time do you want (in minutes)?: ")))

while ( is.na(maxtime) ) {
  message("")
  message("Sorry I didn't understand that. Your answer needs to be a number.")
  suppressWarnings(maxtime <- as.numeric(user.input(prompt="How much time do you want (in minutes)?: ")))
  Sys.sleep(1)
}

message("")
message("OK, let's ask questions for ",maxtime," minutes!")
Sys.sleep(1)

i <- 1
endtime <- Sys.time() + ( maxtime * 60 )
numcorrect = 0
numwrong = 0
sums = NULL

while ( Sys.time() < endtime ) {
  message("")
  message("Question ",i)

  i = i + 1
  Sys.sleep(1)

  n1 <- sample(1:12,1)
  n2 <- sample(1:12,1)

  message("")
  message("What is ",n1, " x " , n2,"?")
  Sys.sleep(0.1)

  message("")
  suppressWarnings(myans <- as.integer(user.input(prompt="Answer: ")))
  Sys.sleep(1)

  while ( is.na(myans) ) {
    message("")
    message("Sorry I didn't understand that. Your answer needs to be a number.")
    suppressWarnings(myans <- as.integer(user.input(prompt="Answer: ")))
    Sys.sleep(1)
  }

  if ( myans == n1 * n2 ) {
    message("")
    message("Correct! ", n1, " x ", n2, " = ", n1*n2)
    numcorrect = numcorrect + 1
    mysum <- paste(n1 ,"x", n2, "Correct")
  }

  if ( myans != n1 * n2 ) {
    message("")
    message("Whoops, that's incorrect. ", n1, " x " , n2 , " = ", n1*n2 )
    numwrong = numwrong + 1
    mysum <- paste(n1 ,"x", n2, "Incorrect")
  }

  sums <- c(sums, mysum)
  Sys.sleep(1)

  timeleft <- difftime(endtime , Sys.time(), units = "secs")[[1]]
  message("")
  if ( timeleft < 0 ) {
    message("Time's up!")
    message("Your final score is ", numcorrect , " / " , numcorrect + numwrong )
  } else {
    message("There's ",round(timeleft)," seconds left")
    message("")
    message("Your running score is ", numcorrect , " / " , numcorrect + numwrong )
  }
}
}

##### Set number of questions
if ( quiztype == 2 ) {


message("")
suppressWarnings(numq <- as.integer(user.input(prompt="How many time table questions do you want to answer?: ")))

while ( is.na(numq) ) {
  message("")
  message("Sorry I didn't understand that. Your answer needs to be a number.")
  suppressWarnings(numq <- as.integer(user.input(prompt="Number of questions: ")))
  Sys.sleep(1)
}

message("")
message("OK, let's do ",numq," time table questions!")
Sys.sleep(1)

i <- 1
numleft <- numq
numcorrect = 0
numwrong = 0
sums = NULL

while (numleft > 0) {
  message("")
  message("Question number ",i," out of ",numq)

  numleft = numleft - 1
  i = i + 1
  Sys.sleep(1)

  n1 <- sample(1:12,1)
  n2 <- sample(1:12,1)

  message("")
  message("What is ",n1, " x " , n2,"?")
  Sys.sleep(0.1)

  message("")
  suppressWarnings(myans <- as.integer(user.input(prompt="Answer: ")))
  Sys.sleep(1)

  while ( is.na(myans) ) {
    message("")
    message("Sorry I didn't understand that. Your answer needs to be a number.")
    suppressWarnings(myans <- as.integer(user.input(prompt="Answer: ")))
    Sys.sleep(1)
  }

  if ( myans == n1 * n2 ) {
    message("")
    message("Correct! ", n1, " x ", n2, " = ", n1*n2)
    numcorrect = numcorrect + 1
    mysum <- paste(n1 ,"x", n2, "Correct")
  } 

  if ( myans != n1 * n2 ) {
    message("")
    message("Whoops, that's incorrect. ", n1, " x " , n2 , " = ", n1*n2 )
    numwrong = numwrong + 1
    mysum <- paste(n1 ,"x", n2, "Incorrect")
  } 

  sums <- c(sums, mysum)
  Sys.sleep(1)

  message("")
  if ( numleft > 0 ) {
    message("Your running score is ", numcorrect , " / " , numcorrect + numwrong )
  } else {
    message("")
    message("Now for a summary of your results:")
    message("Your final score is ", numcorrect , " / " , numcorrect + numwrong )
  }
}
}

## Final results summary
message("")
if (numcorrect / ( numcorrect + numwrong ) == 1 ) {
  message("Well done! You got all questions correct :)")
} else if (numcorrect / ( numcorrect + numwrong ) >= 0.8 ) {
  message("Well done! You got >=80% of questions correct :)")
} else {
  message("Oh dear, you got <80%. More practice required")
}  

message("")
message("Here is a summary of questions attempted:")
print(as.data.frame(sums))

message("")
suppressWarnings(play <- user.input(prompt="Would you like to play again? (y/n): "))
play <- toupper(substring(play,first=1,last=1))
if (play == "Y" ) {
  cmd <- paste("Rscript tt.R",paste(myname,""))
  system(cmd)
} else {
  message("Thanks for playing",myname,"! Goodbye!")
  quit(save="no")
}
