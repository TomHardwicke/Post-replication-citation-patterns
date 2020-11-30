# Preprocessing

# identify the five different case studies we are looking at
caseNames <- c('baumeister', 'sripada', 'strack', 'carter', 'caruso')

## Citation data

### Load files
d_citations <- data.frame() # create empty list to hold data frames for each case
for(i in caseNames){ # loop through case names
  filePath <- here('data','primary', paste0(i,'Citations.csv')) # create file path
  d <- read_csv(filePath, col_types = cols(.default = "c")) # load the data
  d <- d %>% mutate(case = i)
  d_citations <- bind_rows(d_citations, d) # append to dataframe
}

### Apply munging
d_citations <- d_citations %>%
  mutate(pubYear = as.numeric(PY))

### Save file
save(d_citations, file = here('data','processed','/d_citations.rds'))

## Reference data

### Load files
d_reference <- data.frame() # create empty list to hold data frames for each case
for(i in caseNames){ # loop through case names
  filePath <- here('data','primary', paste0(i,'ReferenceClass.csv')) # create file path
  d <- read_csv(filePath) # load the data
  d <- d %>% mutate(case = i)
  d_reference <- bind_rows(d_reference, d) # append to dataframe
}

### Apply munging
d_reference <- d_reference %>%
  mutate(pubYear = as.numeric(year))

### Save file
save(d_reference, file = here('data','processed','/d_reference.rds'))

## Content analysis data

### Load files
d_contentAnalysis <- data.frame() # create empty list to hold data frames for each case
for(i in caseNames){ # loop through case names
  filePath <- here('data','primary', paste0('contentAnalysis_',i,'.csv')) # create file path
  d <- read_csv(filePath, col_types = cols(.default = "c")) # load the data
  d <- d %>% mutate(case = i)
  d_contentAnalysis <- bind_rows(d_contentAnalysis, d) # append to dataframe
}

### Apply munging
d_contentAnalysis <- d_contentAnalysis %>% 
  mutate_at(vars( # change the columns below
    case,
    firstCoder,
    secondCoder,
    exclusionReason,
    articleType,
    citationClassificationOriginal,
    citationClassificationAgreed
  ),factor) %>% # to the factor type 
mutate(pubYear = as.numeric(pubYear))

# identify years pre/post contradictory replication
d_contentAnalysis <- d_contentAnalysis %>%
  mutate(timePeriod = case_when(
    case %in% c("baumeister", "sripada", "strack") & pubYear < 2016 ~ 'pre',
    case %in% c("baumeister", "sripada", "strack") & pubYear > 2016 ~ 'post',
    case %in% c("caruso", "carter") & pubYear < 2014 ~ 'pre',
    case %in% c("caruso", "carter") & pubYear > 2014 ~ 'post',
    TRUE ~ 'ERROR'
  ))

### Save file
save(d_contentAnalysis, file = here('data','processed','/d_contentAnalysis.rds'))

# Tidy up
rm(list = ls()) # remove all objects from the R environment
   