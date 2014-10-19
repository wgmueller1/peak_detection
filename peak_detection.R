PeakCycle <- function(Data, SearchFrac){
  # using package "wmtsa"
  #the SearchFrac parameter just controls how much to look to either side 
  #of wavCWTPeaks()'s estimated maxima for a bigger value
  #see dRange
  Wave <- wavCWT(Data)
  WaveTree <- wavCWTTree(Wave)
  WavePeaks <- wavCWTPeaks(WaveTree, snr.min=5)
  WavePeaks_Times <- attr(WavePeaks, which="peaks")[,"iendtime"]
  
  NewPeakTimes <- c()
  dRange <- round(SearchFrac*length(Data))
  for(i in 1:length(WavePeaks_Times)){
    NewRange <- max(c(WavePeaks_Times[i]-dRange, 1)):min(c(WavePeaks_Times[i]+dRange, length(Data)))
    NewPeakTimes[i] <- which.max(Data[NewRange])+NewRange[1]-1
  }
  
  return(matrix(c(NewPeakTimes, Data[NewPeakTimes]), ncol=2, dimnames=list(NULL, c("PeakIndices", "Peaks"))))
}

plot(seq_along(as.vector(sunspots)), as.vector(sunspots), type="l")
Sunspot_Ext <- PeakCycle(sunspots,0.2)
points(Sunspot_Ext, col="blue", pch=20)