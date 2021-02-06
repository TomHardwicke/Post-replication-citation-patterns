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
write_csv(d_citations, path = here('data','processed','/d_citations.csv'))
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
write_csv(d_reference, path = here('data','processed','/d_reference.csv'))
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
    citationClassificationAgreed,
    counterArguments
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

# homogenize article type labels
d_contentAnalysis <- d_contentAnalysis %>%
  mutate(articleType = recode(articleType,
                              'Data synthesis - Meta-analysis' = 'Data synthesis - meta-analysis',
                              'Empirical data -laboratory study' = 'Empirical data - laboratory study',
                              'Empirical data -field study' = 'Empirical data - field study',
                              'Review' = 'No empirical data',
                              'Data synthesis - Systematic Review' = 'No empirical data',
                              'Data synthesis' = 'Data synthesis - meta-analysis'))

### Save file
write_csv(d_contentAnalysis, path = here('data','processed','/d_contentAnalysis.csv'))
save(d_contentAnalysis, file = here('data','processed','/d_contentAnalysis.rds'))

## Coauthor data

### Load files
d_coauthors <- data.frame() # create empty list to hold data frames for each case
for(i in caseNames){ # loop through case names
  filePath <- here('data','primary', paste0(i,'Coauthors.csv')) # create file path
  d <- read_csv(filePath, col_types = cols(.default = "c")) # load the data
  d <- d %>% mutate(case = i)
  d_coauthors <- bind_rows(d_coauthors, d) # append to dataframe
}

### Apply munging

### Save file
write_csv(d_coauthors, path = here('data','processed','/d_coauthors.csv'))
save(d_coauthors, file = here('data','processed','/d_coauthors.rds'))
   