# functions

# function to standardize citations
# standardizes the citation counts for the target article and reference class against the year in which the maximum number of citations to the target article were received

standardizeCitations <- function(citationData, referenceData, contentData, replicationYear){
  
  # summarize the content data on whether articles cite the replication study
  repData <- contentData %>% 
    filter(excluded == F, timePeriod == 'post') %>% 
    count(pubYear, citesReplication) %>% 
    pivot_wider(id_cols = 'pubYear', 
                names_from = citesReplication, 
                values_from = n, 
                values_fill = list(n=0)) %>% # use zero for 'missing' values
    rename("citesRep_no" = `FALSE`, "citesRep_yes" = `TRUE`)
  
  # summarize the content data on citation classifications
  classificationData <- contentData %>% 
    filter(excluded == F) %>% 
    count(pubYear, citationClassificationAgreed) %>% 
    pivot_wider(id_cols = 'pubYear', 
                names_from = citationClassificationAgreed, 
                values_from = n, 
                values_fill = list(n=0)) # use zero for 'missing' values
  
  # summarize yearly exclusions for content data
  exclusionData <- contentData %>%
    count(pubYear, excluded) %>%
    pivot_wider(id_cols = 'pubYear',
                names_from = excluded,
                values_from = n, 
                values_fill = list(n=0)) %>%
    rename("excluded_no" = `FALSE`, "excluded_yes" = `TRUE`)
  
  # summarize counter arguments for content data
  counterargData <- contentData %>% 
    mutate(counterArguments = factor(counterArguments)) %>% # make a factor so we can retain levels even if count is zero
    filter(excluded == F, 
           timePeriod == 'post', 
           citesReplication == T,
           citationClassificationAgreed == 'favourable') %>% 
    count(pubYear, counterArguments, .drop = F) %>% 
    pivot_wider(id_cols = 'pubYear', 
                names_from = counterArguments, 
                values_from = n, 
                values_fill = list(n=0)) %>% # use zero for 'missing' values
    rename("counterargs_no" = `FALSE`, "counterargs_yes" = `TRUE`)
    
  # merge the summarized content data
  contentDataSummarized <- left_join(classificationData, repData, by = 'pubYear') %>%
    left_join(exclusionData, by = 'pubYear') %>%
    left_join(counterargData, by = 'pubYear')
  
  # summarize to get number of citations per year to target study
  citationDataSummarized <- citationData  %>%
    count(pubYear) %>%
    rename('citesTarget' = n)
  
  # identify year with highest citations in citation data to use as standard year
  ## NB !!! No longer using this. Now using replication year as standardYear
  standardYear <- citationDataSummarized %>%
    filter(citesTarget == max(citesTarget)) %>%
    pull(pubYear)
  #standardYear <- replicationYear
  
  # extract the number of citations in the standard year for the reference data
  standardCitationsRef <- referenceData %>% 
    filter(pubYear == standardYear) %>% 
    pull(totalCitations)
  
  # extract the number of citations in the standard year for the citation data
  standardCitationsCite <- citationDataSummarized %>% 
    filter(pubYear == standardYear) %>% 
    pull(citesTarget)
  
  # calculate standardization factor to convert the standard year's citations to 100 citations
  standardizationFactorRef <- 100/standardCitationsRef
  standardizationFactorCite <- 100/standardCitationsCite
  
  # now apply standardization across all years
  
  # for reference class
  referenceDataSummarized <- referenceData %>%
    rename(citesRef = totalCitations) %>% # rename citations to reference class
    mutate(citesRef_std = citesRef*standardizationFactorRef) # add std citation counts
  
  # for target article
  citationDataSummarized <- citationDataSummarized %>%
    mutate(citesTarget_std = citesTarget*standardizationFactorCite) # add std citation counts
  
  # combine target citation data, reference citation data, and content analysis data
  d <- full_join(citationDataSummarized, referenceDataSummarized, by = "pubYear") %>%
    replace_na(list( # For any years with no citations, replace NAs with zeros
      citesTarget = 0, 
      citesTarget_std = 0)) %>%
    left_join(contentDataSummarized, by = 'pubYear') %>%
    select(case, everything()) # change column order

  d <- d %>% # add columns to identify the...
    mutate(standardizationYear = # standardization year
             ifelse(pubYear == standardYear, TRUE, FALSE),
           replicationYear = # replication year
             ifelse(pubYear == replicationYear, TRUE, FALSE)) 
  
  # add proportions and standardize the citesRep and citationClassification information
  d <- d %>%
    mutate(favourable_prop = favourable/citesTarget,
           favourable_std = favourable_prop*citesTarget_std,
           unfavourable_prop = unfavourable/citesTarget,
           unfavourable_std = unfavourable_prop*citesTarget_std,
           unclassifiable_prop = unclassifiable/citesTarget,
           unclassifiable_std = unclassifiable_prop*citesTarget_std,
           equivocal_prop = equivocal/citesTarget,
           equivocal_std = equivocal_prop*citesTarget_std,
           citesRep_no_prop = citesRep_no/citesTarget,
           citesRep_yes_prop = citesRep_yes/citesTarget,
           citesRep_no_std = citesRep_no_prop*citesTarget_std,
           citesRep_yes_std = citesRep_yes_prop*citesTarget_std,
           excluded_no_prop = excluded_no/citesTarget,
           excluded_yes_prop = excluded_yes/citesTarget,
           excluded_no_std = excluded_no_prop*citesTarget_std,
           excluded_yes_std = excluded_yes_prop*citesTarget_std)

  return(d)
}

# this function plots a citation curve
citationCurve <- function(
  thisCase, # identify case (string)
  zoom = F, # show full citation history (F) or zoom in on years around replication (T)
  areaPlot = F, # add area plot with 'citesReplication' or 'classification'. F = no area plot
  plotReference = T, # plot the reference class (T) or not (F)
  standardized = T # use standarized citations (T) or not (F)
  ){
  
  d <- d_summary %>% filter(case == thisCase) # extract the data for this case
  
  replicationYear <- d %>% # identify the replication year for this case
    filter(replicationYear == T) %>% 
    pull(pubYear)
  
  # set the top of the y-axis and increments depending on case (some have many citations, some have few)
  if(standardized == T){ # if standardizing then all cases can use same y axis
    yMax <- 100
    yInc <- 25
  }else if(thisCase == 'baumeister'){
    yMax <- 250
    yInc <- 50
  }else if(thisCase == 'strack'){
    yMax <- 70
    yInc <- 10
  }else{
    yMax <- 20
    yInc <- 5
  }
  
  # set the x-axis limits and increments depending on case (length of citation histories varies)
  if(thisCase == 'baumeister'){ # if standardizing then all cases can use same y axis
    xmin <- 1998
    xmax <- 2020
    xInc <- 5
  }else if(thisCase == 'strack'){
    xmin <- 1988
    xmax <- 2020
    xInc <- 5
  }else if(thisCase == 'sripada'){
    xmin <- 2014
    xmax <- 2020
    xInc <- 2
  }else if(thisCase == 'carter'){
    xmin <- 2011
    xmax <- 2019
    xInc <- 2
  }else if(thisCase == 'caruso'){
    xmin <- 2013
    xmax <- 2019
    xInc <- 2
  }
  
  # re-organise the data frame for plotting
  d <- d %>%
    select(
      case, 
      pubYear,
      excluded_yes_std,
      citesTarget,
      citesTarget_std,
      citesRef_std,
      citesRep_yes_std,
      citesRep_no_std,
      favourable_std,
      unfavourable_std,
      equivocal_std,
      unclassifiable_std) %>%
    pivot_longer(
      cols = c(
        excluded_yes_std,
        citesTarget,
        citesTarget_std,
        citesRef_std,
        citesRep_yes_std,
        citesRep_no_std,
        favourable_std,
        unfavourable_std,
        equivocal_std,
        unclassifiable_std),
      names_to = 'var') %>%
    mutate(var = fct_recode(var,
                            'Excluded' = 'excluded_yes_std',
                            'Target study (raw)' = 'citesTarget',
                            'Target study' = 'citesTarget_std',
                            'Reference studies' = 'citesRef_std',
                            'Cites replication' = 'citesRep_yes_std',
                            'Does not cite replication' = 'citesRep_no_std',
                            'Unclassifiable' = 'unclassifiable_std',
                            'Favourable' = 'favourable_std',
                            'Equivocal' = 'equivocal_std',
                            'Unfavourable' = 'unfavourable_std'),
           var = fct_relevel(var,
                             'Excluded',
                             'Target study (raw)',
                             'Target study',
                             'Reference studies',
                             'Cites replication',
                             'Does not cite replication',
                             'Unclassifiable',
                             'Favourable',
                             'Equivocal',
                             'Unfavourable'))
  
  # set up base ggplot
  basePlot <- ggplot(data = d, aes(x = pubYear, y = value))
  
  if(zoom==T){
    d <- d %>%
      filter(pubYear %in% seq(replicationYear-3, max(d$pubYear), 1))
    basePlot <- basePlot +
      scale_x_continuous(
        breaks = seq(replicationYear-3, max(d$pubYear), 1),
        limits = c(replicationYear-3,max(d$pubYear))) # adjust x axis
  }else{
    basePlot <- basePlot +
      scale_x_continuous(
        breaks = seq(xmin, xmax, xInc)) # adjust x axis
  }
  
  ## the following is to account for the absence of qualitative data in the replication year
  ## we add data points at dummy 'years' just before and just after the replication year
  ## this keeps the area_plot lines perpendicular to the y axis
  priorYear <- d %>%
    filter(pubYear == replicationYear-1) %>% # get data from prior year
    mutate(pubYear = as.numeric(replicationYear-.5))
  
  postYear <- d %>%
    filter(pubYear == replicationYear+1) %>% # get data from subsequent year
    mutate(pubYear = as.numeric(replicationYear+.5))
  
  d <- rbind(d,priorYear,postYear) %>%
    mutate(dummyYears = ifelse(
      pubYear %in% c(replicationYear-.5,replicationYear+.5), TRUE, FALSE))
  ## end accounting for absence of qual data in rep year
  
  if(plotReference == T){ # if user wants to plot the reference class
    basePlot <- basePlot +
      geom_line(
      data = d %>% # plot reference class citations
        filter(var == 'Reference studies',
               dummyYears == F), 
      colour = "grey", 
      linetype = 'dashed', 
      size = .5)
  }

  if(areaPlot == 'classification'){
    basePlot <- basePlot +
      geom_area(
        aes(fill=var, colour = var),
        data = d %>%
          filter(
            pubYear %in% c(replicationYear+.5,seq(replicationYear+1,max(pubYear))),
            var %in% c(
              'Excluded',
              'Unclassifiable',
              'Favourable',
              'Equivocal',
              'Unfavourable')) %>%
          mutate(var = fct_drop(var)))  +
      geom_area(
        aes(fill=var, colour = var),
        data = d %>%
          filter(
            pubYear %in% c(replicationYear-1,replicationYear-.5),
            var %in% c(
              'Excluded',
              'Unclassifiable',
              'Favourable',
              'Equivocal',
              'Unfavourable')) %>%
          mutate(var = fct_drop(var))) +
      scale_fill_manual(name='', values = p1) +
      scale_colour_manual(name = '', values = p1, guide = F)
  }
  
  if(areaPlot == 'citesReplication'){
    basePlot <- basePlot +
      geom_area(
        aes(fill=var),
        data = d %>%
          filter(
            dummyYears == F,
            pubYear %in% c(seq(replicationYear+1,max(pubYear))),
            var %in% c(
              'Excluded',
              'Cites replication',
              'Does not cite replication')) %>%
          mutate(var = fct_drop(var))) +
      scale_fill_manual(name = '', values = p2)
  }
  
  if(standardized == T){
    basePlot <- basePlot +
      geom_line(
        data = d %>% # plot citations to target study
          filter(var == 'Target study'),
        colour = 'black',
        linetype = 'solid', 
        size = 1)
  }
  
  if(standardized == F){
    basePlot <- basePlot +
      geom_line(
        data = d %>% # plot citations to target study
          filter(var == 'Target study (raw)'),
        colour = 'black',
        linetype = 'solid', 
        size = 1)
  }
  
  basePlot <- basePlot +
    scale_y_continuous(breaks = seq(0, yMax, yInc), expand = c(0,0)) + # adjust y axis
    annotate( # plot arrow marking replication year
      'segment',
      x = replicationYear, 
      y = yMax + yMax/80,
      xend = replicationYear,
      yend = yMax/80,
      size = .5,
      arrow = arrow(length = unit(0.25,"cm"), type="closed")) +
    theme_apa() + # apply APA theme
    theme( # other theme adjustments
      axis.line = element_line(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = 'bottom') +
    ggtitle(str_to_upper(thisCase)) # add title
  
  return(basePlot)
}
