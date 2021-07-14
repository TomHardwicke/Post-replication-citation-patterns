# Preprocessing

# specify custom notin function
`%notin%` = function(x,y) !(x %in% y)

# identify the five different case studies we are looking at
caseNames <- c("baumeister", "sripada", "strack", "carter", "caruso")

## Citation data

### Load files
d_citations <- data.frame() # create empty list to hold data frames for each case
for (i in caseNames) { # loop through case names
  filePath <- here("data", "primary", paste0(i, "Citations.csv")) # create file path
  d <- read_csv(filePath, col_types = cols(.default = "c")) # load the data
  d <- d %>% mutate(case = i)
  d_citations <- bind_rows(d_citations, d) # append to dataframe
}

### Apply munging
d_citations <- d_citations %>%
  mutate(pubYear = as.numeric(PY))

### Save file
write_csv(d_citations, file = here("data", "processed", "/d_citations.csv"))
save(d_citations, file = here("data", "processed", "/d_citations.rds"))

## Reference data

### Load files
d_reference <- data.frame() # create empty list to hold data frames for each case
for (i in caseNames) { # loop through case names
  filePath <- here("data", "primary", paste0(i, "ReferenceClass.csv")) # create file path
  d <- read_csv(filePath) # load the data
  d <- d %>% mutate(case = i)
  d_reference <- bind_rows(d_reference, d) # append to dataframe
}

### Apply munging
d_reference <- d_reference %>%
  mutate(pubYear = as.numeric(year))

### Save file
write_csv(d_reference, file = here("data", "processed", "/d_reference.csv"))
save(d_reference, file = here("data", "processed", "/d_reference.rds"))

## Content analysis data

### Load files
d_contentAnalysis <- data.frame() # create empty list to hold data frames for each case
for (i in caseNames) { # loop through case names
  filePath <- here("data", "primary", paste0("contentAnalysis_", i, ".csv")) # create file path
  d <- read_csv(filePath, col_types = cols(.default = "c")) # load the data
  d <- d %>% mutate(case = i)
  d_contentAnalysis <- bind_rows(d_contentAnalysis, d) # append to dataframe
}

#### also load the articles that became accessible or were translated during round 2 revisions
d_round2access <- read_csv(here("data", "primary", "contentAnalysis_round2access.csv"), col_types = cols(.default = "c")) # load the data

#### save a version of the round 2 access data for reporting how many we could or could not access
d_round2access_meta <- d_round2access %>% select(wos_id, access, source, emailContacted, emailBounced, emailResponseReceived, translationNeeded, translationNote)
write_csv(d_round2access_meta, file = here("data", "processed", "/d_round2access.csv"))
save(d_round2access_meta, file = here("data", "processed", "/d_round2access.rds"))

#### integrate the newly accessible/translated article content analysis data with the previous data
#### firstly align columns in the newly accessible articles df with the content analysis df
d_round2access <- d_round2access %>% select(firstCoder, secondCoder, doi, authors,	pubYear,	excluded,	exclusionReason,	articleType,	citesReplication,	citationClassificationOriginal = citationClassificationFirstCoder, citationClassificationAgreed, counterArguments,	evidenceCounter,	evidenceVerbatim,	methodsCounter,	methodsVerbatim,	expertiseCounter,	expertiseVerbatim, UT =	wos_id,	case)

#### remove the cases excluded for access or translation reasons from the content analysis dataframe and replace with those from the newly accessible articles dataframe

d_contentAnalysis <- d_contentAnalysis %>% mutate(replace = ifelse(exclusionReason %in% c("No access", "Non-English language"), T, F)) # tag articles to replace
d_contentAnalysis <- d_contentAnalysis %>% filter(replace == F) %>% select(-replace) # remove articles to be replaced
d_contentAnalysis <- rbind(d_contentAnalysis,d_round2access) # combine newly accessible/translated articles with content analysis data

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
  ), factor) %>% # to the factor type
  mutate(pubYear = as.numeric(pubYear))

# identify years pre/post contradictory replication
d_contentAnalysis <- d_contentAnalysis %>%
  mutate(timePeriod = case_when(
    case %in% c("baumeister", "sripada", "strack") & pubYear < 2016 ~ "pre",
    case %in% c("baumeister", "sripada", "strack") & pubYear > 2016 ~ "post",
    case %in% c("caruso", "carter") & pubYear < 2014 ~ "pre",
    case %in% c("caruso", "carter") & pubYear > 2014 ~ "post",
    TRUE ~ "ERROR"
  ))

# homogenize article type labels
d_contentAnalysis <- d_contentAnalysis %>%
  mutate(articleType = recode(articleType,
                              "Data synthesis - Meta-analysis" = "Data synthesis - meta-analysis",
                              "Empirical data -laboratory study" = "Empirical data - laboratory study",
                              "Empirical data -field study" = "Empirical data - field study",
                              "Review" = "No empirical data",
                              "Data synthesis - Systematic Review" = "No empirical data",
                              "Data synthesis" = "Data synthesis - meta-analysis"
  ))

### Save file
write_csv(d_contentAnalysis, file = here("data", "processed", "/d_contentAnalysis.csv"))
save(d_contentAnalysis, file = here("data", "processed", "/d_contentAnalysis.rds"))

## Coauthor data

### Load files
d_coauthors <- data.frame() # create empty list to hold data frames for each case
for (i in caseNames) { # loop through case names
  filePath <- here("data", "primary", paste0(i, "Coauthors.csv")) # create file path
  d <- read_csv(filePath, col_types = cols(.default = "c")) # load the data
  d <- d %>% mutate(case = i)
  d_coauthors <- bind_rows(d_coauthors, d) # append to dataframe
}

### Save file
write_csv(d_coauthors, file = here("data", "processed", "/d_coauthors.csv"))
save(d_coauthors, file = here("data", "processed", "/d_coauthors.rds"))

## Replication study citation data [analyses requested by a reviewer]

### Load files
d_repCitations <- data.frame() # create empty list to hold data frames for each case
for (i in c('klein','hagger','wagenmakers')) { # loop through case names
  filePath <- here("data", "primary", paste0(i, "Citations.csv")) # create file path
  d <- read_csv(filePath, col_types = cols(.default = "c")) # load the data
  d <- d %>% mutate(case = i)
  d_repCitations <- bind_rows(d_repCitations, d) # append to dataframe
}

### Apply munging
d_repCitations <- d_repCitations %>%
  mutate(pubYear = as.numeric(PY)) %>%
  filter(pubYear < 2020) # remove any citations after 2019

### Save file
write_csv(d_repCitations, file = here("data", "processed", "/d_repCitations.csv"))
save(d_repCitations, file = here("data", "processed", "/d_repCitations.rds"))