eegFiles <- function(obj,
                      dir = 'telemetry/EEG/', ...) {
  
  files = list.files(dir, pattern = '_FFT', full.names = T)
  files.short = list.files(dir, pattern = '_FFT', full.names = F)
  files = files[grepl('.Rds', files, ignore.case = T)]
  files.short = files.short[grepl('.Rds', files.short, ignore.case = F)]
  
  print(paste('FFT  files:', length(files)))
  
  f = lapply(files.short, function(f) {
    n = unlist(strsplit(f, split = '_'))
    
    MouseNumber = n[1]
    GroupNumber = obj$mice$GroupNumber[obj$mice$MouseNumber == n[1]]
    
    File = f
    
    Channel = sub('s.txt.Rds', '', n[4])
    
    # start.time = as.POSIXct(sprintf("%04.0f", as.numeric(n[3])), format='%H%M%S')
    startTime = paste(n[2], '-2017', ' ', n[3], sep = '')
    startTime = strptime(as.character(startTime), "%d-%m-%Y %H%M%S")
    
    Date = lubridate::floor_date(startTime, 'days')
    Date = as.POSIXct(Date)
    startTime = as.POSIXct(startTime)
    
    # sync to video start time
    rec = subset(obj$EEG$recordings, recordingStart == startTime & MouseNumber == n[1])
    if(nrow(rec) == 0) {
      warning(paste('no record of recording', File, 'with start time', startTime[1])) 
      Day = NA
      Stage = NA
      Recording = NA
    } else {
      Day = rec$Day
      Stage = rec$Stage
      Recording = rec$Recording
    }
    
    data.frame(Recording, Day, Stage, MouseNumber, GroupNumber, Date, Channel, startTime, File)
    
    })
  f = as.data.frame(do.call('rbind', f))
  
  obj$EEG$files = f
  return(obj)
  
}

    