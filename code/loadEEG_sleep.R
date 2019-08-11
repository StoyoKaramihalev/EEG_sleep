library(zoo) #locf function

loadEEG_sleep = function(obj, onlyBands = F, onlyValid = F, groups = 'all', mice = 'all', days = 'all', recordings = 'all', 
                   dir = 'telemetry/EEG/', timeResMin = 1/60, slowWaveOnly =F, verbose = T,
                   channels = c('FFT12', 'FFT23'), ...) {
  
  if(is.null(obj$EEG$files)) {
    stop('obj$EEG$files is empty, please run obj = eegFiles(obj) first...')
  }
  
  files = obj$EEG$files
  
  if(! days == 'all'){
    files = subset(files, Day %in% days)
  }
  if (!groups == 'all') { 
    files = subset(files, GroupNumber %in% groups)
  }
  if (!mice == 'all') {
    files = subset(files, MouseNumber %in% mice)
  }
  if (!recordings == 'all') {
    files = subset(files, Recording %in% recordings)
  }
  
  files$File = as.character(files$File)
  # files = subset(files, Channel %in% channels)
  
  res = lapply(files$File, function(f) { 
    if(verbose) {print(sprintf('reading in file %s out of %s', which(files$File == f), length(files$File)))}
    if(verbose) {print(f)}
    r = readRDS( paste0(dir, f)) 
    r$Day = files$Day[which(files$File == f)]
    r$Channel = files$Channel[which(files$File == f)]
    
    # sync to video start time
    rec = subset(obj$EEG$recordings, recordingStart == r$startTime[1] & MouseNumber == r$MouseNumber[1])
    if(nrow(rec) == 0) {
      warning(paste('no record of recording', files[i], 'with start time', r$startTime[1])) 
      r$Day = NA
      r$Stage = NA
      r$Recording = NA
      r$Valid = NA
    } else {
      if(!is.na(rec$videoStart)){
        r = subset(r, Time >= rec$videoStart)
        r$sec = difftime(r$Time, rec$videoStart, units = 'sec')
        r$Day = NA
        r$Stage = NA
        r$Recording = NA
        r$Valid = NA
      } else { warning(paste('no record of video start time for recording', files[i])) }
      r$Day = rec$Day
      r$Stage = rec$Stage
      r$Recording = rec$Recording
      r$Valid = rec$Valid
      
      # compensate for the differences in Time sync between the EEG PC and the video PC
      if(is.na(rec$syncDiffFromVideo_Cornelia)) {
        # if(is.na(rec$syncDiffFromVideo)) {
        if(verbose) {print(paste('No estimate of the time/sync difference between the video and EEG computer for file', f))}
      } else {
        r$eTime = r$eTime - rec$syncDiffFromVideo_Cornelia/60
        r$Time = r$Time - rec$syncDiffFromVideo_Cornelia
        r$sec = r$sec - rec$syncDiffFromVideo_Cornelia
        r$Time = as.POSIXct(round(r$Time, 'sec'))
      }
    }
    
    # remove higher frequencies? 
    freqs = which(colnames(r)[1:256] %in% as.character(seq(15.25, 64, 0.25)))
    r = r[,-freqs]
    freqs = as.character(seq(0.25, 15, 0.25))
    
    # remove higher frequencies? 
    # freqs = as.character(seq(0.25, 64, 0.25))
    
    
    # replace pre-existing rows with zeros with NAs
    if(verbose) {print('replacing zeros with NAs')}
    zeroRows = unique(which(r[,freqs] == 0, arr.ind = T)[,1])
    if(verbose) {print(paste('removing rows which contain a zero:', length(zeroRows)))}
    r[zeroRows, freqs] = NA
    
    ### remove artefacts: Cornelia's method
    globalMean = mean(rowSums(r[,freqs], na.rm = T), na.rm = T)
    
    # for the FFT23 epoch files: 1:64 Hz in quarter-Hz steps
    outlierRows = r[,1] > (globalMean/4) | r[, 2] > (globalMean/4) | r[, 3] > (globalMean/4) | r[, 4] > (globalMean/4)
    outlierRows[is.na(outlierRows)] = F
    
    if(verbose) {print(paste('removing rows with artifact behavior:', sum(outlierRows)))}
    r[which(outlierRows == T), freqs] = NA
    r[outlierRows,] = NA
    
    
    globalMean = mean(rowSums(r[,freqs], na.rm = T), na.rm = T)
    globalSum = sum(r[, freqs], na.rm = T)
    meanRowSums = mean(rowSums(r[, freqs], na.rm = T), na.rm = T)
    # r[, 1:256] = r[, 1:256] / mean(rowSums(r[, 1:256], na.rm = T), na.rm = T)
    
    # Normalize
    # r[, freqs] = r[, freqs] / globalSum
    r[, freqs] = r[, freqs] / meanRowSums

    # r[, as.character(seq(0.5, 64, 0.25))] = r[, as.character(seq(0.5, 64, 0.25))] /  mean(rowSums(r[, as.character(seq(0.5, 64, 0.25))], na.rm = T), na.rm = T)
    
    r$globalMean = globalMean
    r$globalSum = globalSum
    r$meanRowSums = meanRowSums
    r$outlierRows = sum(outlierRows)
    r$outlierRowFraction = sum(outlierRows)/nrow(r)
    r$zeroRows = length(zeroRows)
    r$zeroRowFraction = length(zeroRows) / nrow(r)
    
    r$slowWave = rowSums(r[,as.character(seq(0.5, 4, 0.25))], na.rm = T)
    
    if(slowWaveOnly) {
      r = r[, !colnames(r) %in% as.character(seq(0.25, 64, 0.25))]
    }
    # r$totalMean = mean(r[, 2:128], na.rm = T)
    return(r)
  })
  
  x = do.call('rbind', res)
  return(x)
}