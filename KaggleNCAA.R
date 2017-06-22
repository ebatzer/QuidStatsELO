devtools::install_github('zachmayer/kaggleNCAA')

library('kaggleNCAA')
data(sample_submission)

f <- tempfile()
write.csv(sample_submission, f, row.names=F)
dat <- parseBracket(f)
unlink(f)

sim <- simTourney(dat, 100, year=2017, progress=TRUE)
bracket <- extractBracket(sim)

str(bracket)
printableBracket(bracket)

View(sample_submission)
View(dat)
unique(c(dat$team_1, dat$team_2))
