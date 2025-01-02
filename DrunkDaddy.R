DrunkDaddy <- function(AdjMat, Step = 10000, Home = 1, Neg = FALSE) {
  Adj <- AdjMat
  Path <- rep(NA, Step)
  Path[1] <- Home

  # DrunkDaddy's walk path
  for (i in 2:Step)
  {
    Path[i] <- ifelse(length(which(Adj[Path[i - 1], ] == 1)) == 1,
      which(Adj[Path[i - 1], ] == 1),
      sample(which(Adj[Path[i - 1], ] == 1), 1)
    )
  }

  # drunk santa claus
  # number of visits at each node
  VisitSum <- tabulate(Path, nbins = dim(Adj)[1])
  # Breaks <- 1:(ncol(Adj) + 1) - 0.5
  # H <- hist(Path, breaks = Breaks, plot = F)
  # VisitSum <- H$counts

  # very drunk santa claus
  VisitSumNeg <- as.vector(-diff(table(
    rep_len(c(1, -1), Step),
    factor(Path, seq(dim(Adj)[1]))
  )))

  return(list(
    Path = Path,
    VisitSum = VisitSum,
    VisitSumNeg = VisitSumNeg,
    Step = Step,
    Home = Home
  ))
}
