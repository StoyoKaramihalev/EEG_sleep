# permutePairs: 
#   F - use only unique combinations of pairs
#   T - use each pair twice, e.g. 1-2 and 2-1 (inefficient)

pairwiseCorrelations <- function(dat, permutePairs = F) {
  nMice = length(unique(dat$MouseNumber))
  dat$MouseNumberNumeric = plyr::mapvalues(dat$MouseNumber, unique(dat$MouseNumber), 1:length(unique(dat$MouseNumber)))
  
  if(permutePairs) {
    perm = gtools::permutations(nMice, 2, repeats.allowed = F)
  } else {
    perm = gtools::combinations(nMice, 2, repeats.allowed = F)
  }
  
  res = lapply(1:nrow(perm), function(p) {
    r = perm[p,]
    d1 = subset(dat, MouseNumberNumeric == r[1], select = c('vigilance'))
    d2 = subset(dat, MouseNumberNumeric == r[2], select = c('vigilance'))
    
    if(nrow(d1) > 1 & nrow(d2) > 1) {
      d1$Time = 1:nrow(d1)
      d2$Time = 1:nrow(d2)
      colnames(d2)[1] = 'vigilance2'
      
      res = suppressMessages(inner_join(d1, d2))
      Correlation = cor(res$vigilance, res$vigilance2, method = 'pearson')
      
      g1 = unique(dat$GroupNumber[dat$MouseNumberNumeric == r[1]])
      g2 = unique(dat$GroupNumber[dat$MouseNumberNumeric == r[2]])
      
      GroupType = ifelse(g1 == g2, 'same', 'different')
      
      res = tibble(
        pair = paste0(r[1],'-', r[2]),
        MouseNumberNumeric = r[1],
        MouseNumberNumeric2 = r[2],
        MouseNumber = unique(dat$MouseNumber[dat$MouseNumberNumeric == r[1]]),
        MouseNumber2 = unique(dat$MouseNumber[dat$MouseNumberNumeric == r[2]]),
        Correlation = Correlation, 
        GroupType = GroupType)
      res$GroupType = as.character(GroupType)
      
      return(res)
    } else {return(NULL)}
  })
  
  res = do.call('rbind', res)
}
