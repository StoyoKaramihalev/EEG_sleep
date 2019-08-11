# minDuration = 1 # minimum duration of current state

pseudoCausality <- function(dat, minDuration = 0, verbose = T,
                            startingState = 'awake', 
                            timePeriods = c(1:10)) {
  
  
  # create epochs
  dat = dat %>% dplyr::group_by(MouseNumber, Recording) %>% arrange(eTime) %>%
    dplyr::mutate(epoch = 1:n())
  
  # label nMice
  dat = dat %>% dplyr::group_by(Recording, GroupNumber, epoch) %>%
    dplyr::mutate(
      nMice = length(unique(MouseNumber)),
      nAwake = sum(vigilance == 'awake'), 
      nAsleep = sum(sleep == 'asleep'))
  
  # loop over mice
  res = lapply(unique(dat$MouseNumber), function(m) {
    if(verbose) {print(paste('Mouse: ', m))}
    g = obj$mice$GroupNumber[obj$mice$MouseNumber == m]
    
    vv = dat %>% dplyr::group_by(MouseNumber) %>%
      dplyr::mutate(
        awaken = newBout == T & vigilance == startingState
      )
    
    # set minimum duration
    awakenings = vv$epoch[vv$MouseNumber == m & vv$awaken == T & vv$duration >= minDuration]
    bouts = vv$bout[vv$MouseNumber == m & vv$awaken == T & vv$duration >= minDuration]
    if(verbose) {print(paste0('...awakenings: ', length(awakenings)))}
    
    res = lapply(awakenings, function(t) {
      # apply over timePeriods
      res = lapply(timePeriods, function(timePeriod) {
        t2 = t + timePeriod
        
        # keep only the relevant time period for the other 3 mice
        vvv = subset(vv, epoch >= t & epoch <= t2)
        
        # keep only the mice which were asleep at t0
        asleepMice = vvv$MouseNumber[vvv$epoch == t & vvv$vigilance %in% c('nrem', 'rem')]
        if(length(asleepMice) == 0) {
          if(verbose) {print('no other mice that were asleep at the time')}
          return()
        }
        vvv = subset(vvv, MouseNumber %in% c(m, asleepMice))
        
        # calculate the number of awakenings
        s = vvv %>% 
          dplyr::group_by(MouseNumber) %>% 
          dplyr::summarize(N = sum(awaken))
        
        colnames(s)[1] = 'MouseNumber2'
        s = s[s$MouseNumber2 != m,]
        bout = bouts[which(awakenings == t)[1]]
        
        s$GroupType = 'different'
        if(sum(s$MouseNumber2 %in% obj$mice$MouseNumber[obj$mice$GroupNumber == g]) == 0) {
          warning('no awake mice available from the same group...')
          return()
        }
        
        s$GroupType[s$MouseNumber2 %in% obj$mice$MouseNumber[obj$mice$GroupNumber == g]] = 'same'
        
        # randomly select n mice from different groups
        if(length(s$MouseNumber2[s$GroupType == 'different']) < length(s$MouseNumber2[s$GroupType == 'same'])) {
          if(verbose) {print(s)}
          return()
        }
        others = sample(s$MouseNumber2[s$GroupType == 'different'], 
                        length(s$MouseNumber2[s$GroupType == 'same']), replace = F)
        
        s = subset(s, GroupType == 'same' | MouseNumber2 %in% others)
        s = s %>% dplyr::group_by(GroupType) %>%
          dplyr::mutate(
            nMice = length(MouseNumber2),
            Po = sum( N > 0 ), 
            P.atLeastOne = Po > 0,
            P.atLeastTwo = Po > 1,
            P.atLeastThree = Po > 2,
            P.atLeastOneWeighted = (Po > 0) / nMice
          ) 
        s = s %>% dplyr::mutate(
          MouseNumber = m, 
          t = t, t2 = t2, timePeriod = timePeriod, bout = bout)
        return(s)
      }) 
      res = do.call('rbind', res)
    }) 
    res = do.call('rbind', res)
  })
  res = do.call('rbind', res)
  return(res)
}



