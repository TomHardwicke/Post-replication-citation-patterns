# functions

# function to standardize citations
# standardizes the citation counts for the target article and reference class against the year in which the maximum number of citations to the target article were received

standardizeCitations <- function(citationData, referenceData, contentData, replicationYear, thisCase) {

  # summarize the content data on whether articles cite the replication study
  repData <- contentData %>%
    filter(excluded == F, timePeriod == "post") %>%
    count(pubYear, citesReplication) %>%
    pivot_wider(
      id_cols = "pubYear",
      names_from = citesReplication,
      values_from = n,
      values_fill = list(n = 0)
    ) %>% # use zero for 'missing' values
    rename("citesRep_no" = `FALSE`, "citesRep_yes" = `TRUE`)

  # summarize the content data on citation classifications
  classificationData <- contentData %>%
    filter(excluded == F) %>%
    count(pubYear, citationClassificationAgreed) %>%
    pivot_wider(
      id_cols = "pubYear",
      names_from = citationClassificationAgreed,
      values_from = n,
      values_fill = list(n = 0)
    ) # use zero for 'missing' values

  # summarize yearly exclusions for content data
  exclusionData <- contentData %>%
    count(pubYear, excluded) %>%
    pivot_wider(
      id_cols = "pubYear",
      names_from = excluded,
      values_from = n,
      values_fill = list(n = 0)
    ) %>%
    rename("excluded_no" = `FALSE`, "excluded_yes" = `TRUE`)

  # summarize counter arguments for content data
  counterargData <- contentData %>%
    mutate(counterArguments = factor(counterArguments)) %>% # make a factor so we can retain levels even if count is zero
    filter(
      excluded == F,
      timePeriod == "post",
      citesReplication == T,
      citationClassificationAgreed == "favourable"
    ) %>%
    count(pubYear, counterArguments, .drop = F) %>%
    pivot_wider(
      id_cols = "pubYear",
      names_from = counterArguments,
      values_from = n,
      values_fill = list(n = 0)
    ) %>% # use zero for 'missing' values
    rename("counterargs_no" = `FALSE`, "counterargs_yes" = `TRUE`)

  # merge the summarized content data
  contentDataSummarized <- left_join(classificationData, repData, by = "pubYear") %>%
    left_join(exclusionData, by = "pubYear") %>%
    left_join(counterargData, by = "pubYear")

  # summarize to get number of citations per year to target study
  citationDataSummarized <- citationData %>%
    count(pubYear) %>%
    rename("citesTarget" = n)

  # select standardization year
  ## standardize to the replication year
  standardYear <- replicationYear

  # otherwise standardize to year with most citations (no longer using this method)
  # standardYear <- citationDataSummarized %>%
  #   filter(citesTarget == max(citesTarget)) %>%
  #   pull(pubYear)

  # extract the number of citations in the standard year for the reference data
  standardCitationsRef <- referenceData %>%
    filter(pubYear == standardYear) %>%
    pull(totalCitations)

  # extract the number of citations in the standard year for the citation data
  standardCitationsCite <- citationDataSummarized %>%
    filter(pubYear == standardYear) %>%
    pull(citesTarget)

  # calculate standardization factor to convert the standard year's citations to 100 citations
  standardizationFactorRef <- 100 / standardCitationsRef
  standardizationFactorCite <- 100 / standardCitationsCite

  # now apply standardization across all years

  # for reference class
  referenceDataSummarized <- referenceData %>%
    rename(citesRef = totalCitations) %>% # rename citations to reference class
    mutate(citesRef_std = citesRef * standardizationFactorRef) # add std citation counts

  # for target article
  citationDataSummarized <- citationDataSummarized %>%
    mutate(citesTarget_std = citesTarget * standardizationFactorCite) # add std citation counts

  # combine target citation data, reference citation data, and content analysis data
  d <- full_join(citationDataSummarized, referenceDataSummarized, by = "pubYear") %>%
    replace_na(list( # For any years with no citations, replace NAs with zeros
      citesTarget = 0,
      citesTarget_std = 0
    )) %>%
    left_join(contentDataSummarized, by = "pubYear") %>%
    select(case, everything()) # change column order

  d <- d %>% # add columns to identify the...
    mutate(
      standardizationYear = # standardization year
      ifelse(pubYear == standardYear, TRUE, FALSE),
      replicationYear = # replication year
      ifelse(pubYear == replicationYear, TRUE, FALSE)
    )
  
  # add a row summarise the entire post-replication period
  
  # define post-replication period
  postRepPeriod <- as.character(seq(replicationYear+1,2019,1))

  d <- rbind(d, d %>% 
    filter(year %in% postRepPeriod) %>%
    summarise(case = thisCase, 
              pubYear = 'post-replication', 
              citesTarget = sum(citesTarget), 
              citesTarget_std =sum(citesTarget_std), 
              year = 'post-replication', 
              citesRef = sum(citesRef),
              citesRef_std = sum(citesRef_std), 
              equivocal = sum(equivocal), 
              favourable = sum(favourable), 
              unclassifiable = sum(unclassifiable),
              unfavourable = sum(unfavourable),
              citesRep_no = sum(citesRep_no), 
              citesRep_yes = sum(citesRep_yes), 
              excluded_no = sum(excluded_no),
              excluded_yes = sum(excluded_yes),
              counterargs_no = sum(counterargs_no),
              counterargs_yes =sum(counterargs_yes), 
              standardizationYear = FALSE, 
              replicationYear = FALSE))

  # compute extrapolations for the Baumeister case
  ## because there were so many citations for the Baumeister case, we only classified a random sample of 40% of relevant citations across each of  the pre-refutation period and post-refutation period
  ## because random samples were drawn on three separate occasions as the post-refutation period was expanded, the actual proportion coded in a given year is not actually 40% (though across the whole period it is 40%).
  ## therefore, to extrapolate from the sample to all citations, we need to multiple the number of classifications by the reciprocal of the proportion actually coded in each year

  ## get the proportion actually coded in each year
  contentProp <- d %>%
    filter(pubYear %in% c("2015", "2017", "2018", "2019", "post-replication")) %>%
    mutate(contentProp = (favourable + unfavourable + equivocal + unclassifiable + excluded_yes) / citesTarget) %>%
    pull(contentProp)

  ## now to extrapolate, multiply the counts for each classification type by the reciprocal of the proportion actually coded
  d <- d %>%
    mutate(favourable_extra = ifelse(
      case == "baumeister", case_when( # for baumeister case
        pubYear == "2015" ~ favourable * (1 / contentProp[1]),
        pubYear == "2017" ~ favourable * (1 / contentProp[2]),
        pubYear == "2018" ~ favourable * (1 / contentProp[3]),
        pubYear == "2019" ~ favourable * (1 / contentProp[4]),
        pubYear == "post-replication" ~ favourable * (1 / contentProp[5]),
      ),
      NA # for other cases nothing is needed
    )) %>% 
    mutate(unfavourable_extra = ifelse(
      case == "baumeister", case_when( # for baumeister case
        pubYear == "2015" ~ unfavourable * (1 / contentProp[1]),
        pubYear == "2017" ~ unfavourable * (1 / contentProp[2]),
        pubYear == "2018" ~ unfavourable * (1 / contentProp[3]),
        pubYear == "2019" ~ unfavourable * (1 / contentProp[4]),
        pubYear == "post-replication" ~ unfavourable * (1 / contentProp[5]),
      ),
      NA # for other cases nothing is needed
    )) %>% 
    mutate(equivocal_extra = ifelse(
      case == "baumeister", case_when( # for baumeister case
        pubYear == "2015" ~ equivocal * (1 / contentProp[1]),
        pubYear == "2017" ~ equivocal * (1 / contentProp[2]),
        pubYear == "2018" ~ equivocal * (1 / contentProp[3]),
        pubYear == "2019" ~ equivocal * (1 / contentProp[4]),
        pubYear == "post-replication" ~ equivocal * (1 / contentProp[5]),
      ),
      NA # for other cases nothing is needed
    )) %>% 
    mutate(unclassifiable_extra = ifelse(
      case == "baumeister", case_when( # for baumeister case
        pubYear == "2015" ~ unclassifiable * (1 / contentProp[1]),
        pubYear == "2017" ~ unclassifiable * (1 / contentProp[2]),
        pubYear == "2018" ~ unclassifiable * (1 / contentProp[3]),
        pubYear == "2019" ~ unclassifiable * (1 / contentProp[4]),
        pubYear == "post-replication" ~ unclassifiable * (1 / contentProp[5]),
      ),
      NA # for other cases nothing is needed
    )) %>%
    mutate(citesRep_no_extra = ifelse(
      case == "baumeister", case_when( # for baumeister case
        pubYear == "2015" ~ citesRep_no * (1 / contentProp[1]),
        pubYear == "2017" ~ citesRep_no * (1 / contentProp[2]),
        pubYear == "2018" ~ citesRep_no * (1 / contentProp[3]),
        pubYear == "2019" ~ citesRep_no * (1 / contentProp[4]),
        pubYear == "post-replication" ~ citesRep_no * (1 / contentProp[5]),
      ),
      NA # for other cases nothing is needed
    )) %>%
    mutate(citesRep_yes_extra = ifelse(
      case == "baumeister", case_when( # for baumeister case
        pubYear == "2015" ~ citesRep_yes * (1 / contentProp[1]),
        pubYear == "2017" ~ citesRep_yes * (1 / contentProp[2]),
        pubYear == "2018" ~ citesRep_yes * (1 / contentProp[3]),
        pubYear == "2019" ~ citesRep_yes * (1 / contentProp[4]),
        pubYear == "post-replication" ~ citesRep_yes * (1 / contentProp[5]),
      ),
      NA # for other cases nothing is needed
    )) %>%
    mutate(excluded_no_extra = ifelse(
      case == "baumeister", case_when( # for baumeister case
        pubYear == "2015" ~ excluded_no * (1 / contentProp[1]),
        pubYear == "2017" ~ excluded_no * (1 / contentProp[2]),
        pubYear == "2018" ~ excluded_no * (1 / contentProp[3]),
        pubYear == "2019" ~ excluded_no * (1 / contentProp[4]),
        pubYear == "post-replication" ~ excluded_no * (1 / contentProp[5]),
      ),
      NA # for other cases nothing is needed
    )) %>%
    mutate(excluded_yes_extra = ifelse(
      case == "baumeister", case_when( # for baumeister case
        pubYear == "2015" ~ excluded_yes * (1 / contentProp[1]),
        pubYear == "2017" ~ excluded_yes * (1 / contentProp[2]),
        pubYear == "2018" ~ excluded_yes * (1 / contentProp[3]),
        pubYear == "2019" ~ excluded_yes * (1 / contentProp[4]),
        pubYear == "post-replication" ~ excluded_yes * (1 / contentProp[5]),
      ),
      NA # for other cases nothing is needed
    ))

  # now compute proportions and standardize the citesRep and citationClassification information
  ## for the baumeister case, use the extrapolated numbers
  
  d <- d %>%
    mutate(
      favourable_prop = ifelse(case=='baumeister', favourable_extra / citesTarget, favourable / citesTarget),
      favourable_std = favourable_prop * citesTarget_std,
      unfavourable_prop = ifelse(case=='baumeister', unfavourable_extra / citesTarget, unfavourable / citesTarget),
      unfavourable_std = unfavourable_prop * citesTarget_std,
      unclassifiable_prop = ifelse(case=='baumeister', unclassifiable_extra / citesTarget, unclassifiable / citesTarget),
      unclassifiable_std = unclassifiable_prop * citesTarget_std,
      equivocal_prop = ifelse(case=='baumeister', equivocal_extra / citesTarget, equivocal / citesTarget),
      equivocal_std = equivocal_prop * citesTarget_std,
      citesRep_no_prop = ifelse(case=='baumeister', citesRep_no_extra / citesTarget, citesRep_no / citesTarget),
      citesRep_yes_prop = ifelse(case=='baumeister', citesRep_yes_extra / citesTarget, citesRep_yes / citesTarget),
      citesRep_no_std = citesRep_no_prop * citesTarget_std,
      citesRep_yes_std = citesRep_yes_prop * citesTarget_std,
      excluded_no_prop = ifelse(case=='baumeister', excluded_no_extra / citesTarget, excluded_no / citesTarget),
      excluded_yes_prop = ifelse(case=='baumeister', excluded_yes_extra / citesTarget, excluded_yes / citesTarget),
      excluded_no_std = excluded_no_prop * citesTarget_std,
      excluded_yes_std = excluded_yes_prop * citesTarget_std
    )

  return(d)
}

# this function plots a citation curve
citationCurve <- function(
                          thisCase, # identify case (string)
                          thisTitle, # define graph title (string)
                          zoom = F, # show full citation history (F) or zoom in on years around replication (T)
                          areaPlot = F, # add area plot with 'citesReplication' or 'classification'. F = no area plot
                          plotReference = T, # plot the reference class (T) or not (F)
                          standardized = T # use standarized citations (T) or not (F)
) {
  d <- d_summary %>% 
    filter(case == thisCase) %>% # extract the data for this case
    filter(year != 'post-replication') %>% # remove the summary row for the post-replication period
    mutate(pubYear = as.numeric(pubYear)) # reset pubyear to numeric

  replicationYear <- d %>% # identify the replication year for this case
    filter(replicationYear == T) %>%
    pull(pubYear)

  # set the top of the y-axis and increments depending on case (some have many citations, some have few)
  if (standardized == T) { # if standardizing then all cases can use same y axis (except for Caruso when we standardize to the replication year...)
    if (thisCase == "caruso") {
      yMax <- 300
      yInc <- 25
    } else {
      yMax <- 125
      yInc <- 25
    }
  } else if (thisCase == "baumeister") {
    yMax <- 250
    yInc <- 50
  } else if (thisCase == "strack") {
    yMax <- 70
    yInc <- 10
  } else {
    yMax <- 20
    yInc <- 5
  }

  # set the x-axis limits and increments depending on case (length of citation histories varies)
  if (thisCase == "baumeister") { # if standardizing then all cases can use same y axis
    xmin <- 1998
    xmax <- 2019
    xInc <- 1
  } else if (thisCase == "strack") {
    xmin <- 1988
    xmax <- 2019
    xInc <- 1
  } else if (thisCase == "sripada") {
    xmin <- 2014
    xmax <- 2019
    xInc <- 1
  } else if (thisCase == "carter") {
    xmin <- 2011
    xmax <- 2019
    xInc <- 1
  } else if (thisCase == "caruso") {
    xmin <- 2013
    xmax <- 2019
    xInc <- 1
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
      unclassifiable_std
    ) %>%
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
        unclassifiable_std
      ),
      names_to = "var"
    ) %>%
    mutate(
      var = fct_recode(var,
        "Excluded" = "excluded_yes_std",
        "Target study (raw)" = "citesTarget",
        "Target study" = "citesTarget_std",
        "Reference studies" = "citesRef_std",
        "Cites replication" = "citesRep_yes_std",
        "Does not cite replication" = "citesRep_no_std",
        "Unclassifiable" = "unclassifiable_std",
        "Favourable" = "favourable_std",
        "Equivocal" = "equivocal_std",
        "Unfavourable" = "unfavourable_std"
      ),
      var = fct_relevel(
        var,
        "Excluded",
        "Target study (raw)",
        "Target study",
        "Reference studies",
        "Cites replication",
        "Does not cite replication",
        "Unclassifiable",
        "Unfavourable",
        "Equivocal",
        "Favourable"
      )
    )

  # set up base ggplot
  basePlot <- ggplot(data = d, aes(x = pubYear, y = value))

  if (zoom == T) {
    d <- d %>%
      filter(pubYear %in% seq(replicationYear, max(d$pubYear), 1))
    basePlot <- basePlot +
      scale_x_continuous(
        breaks = seq(replicationYear, max(d$pubYear), 1),
        limits = c(replicationYear, max(d$pubYear)),
        expand = c(0, 0.5)
      ) # adjust x axis
  } else {
    basePlot <- basePlot +
      scale_x_continuous(
        breaks = seq(xmin, xmax, xInc),
        expand = c(0, 0.5)
      ) # adjust x axis
  }

  ## the following is to account for the absence of qualitative data in the replication year
  ## we add data points at dummy 'years' just before and just after the replication year
  ## this keeps the area_plot lines perpendicular to the y axis
  priorYear <- d %>%
    filter(pubYear == replicationYear - 1) %>% # get data from prior year
    mutate(pubYear = as.numeric(replicationYear - .5))

  postYear <- d %>%
    filter(pubYear == replicationYear + 1) %>% # get data from subsequent year
    mutate(pubYear = as.numeric(replicationYear + .5))

  d <- rbind(d, priorYear, postYear) %>%
    mutate(dummyYears = ifelse(
      pubYear %in% c(replicationYear - .5, replicationYear + .5), TRUE, FALSE
    ))
  ## end accounting for absence of qual data in rep year

  if (plotReference == T) { # if user wants to plot the reference class
    basePlot <- basePlot +
      geom_line(
        data = d %>% # plot reference class citations
          filter(
            var == "Reference studies",
            dummyYears == F
          ),
        colour = "grey",
        linetype = "dashed",
        size = .5
      )
  }

  if (areaPlot == "classification") {
    basePlot <- basePlot +
      geom_area(
        alpha = .5,
        aes(fill = var, colour = var),
        data = d %>%
          filter(
            pubYear %in% c(replicationYear + .5, seq(replicationYear + 1, max(pubYear))),
            var %in% c(
              "Excluded",
              "Unclassifiable",
              "Favourable",
              "Equivocal",
              "Unfavourable"
            )
          ) %>%
          mutate(var = fct_drop(var))
      ) +
      geom_area(
        alpha = .5,
        aes(fill = var, colour = var),
        data = d %>%
          filter(
            pubYear %in% c(replicationYear - 1, replicationYear - .5),
            var %in% c(
              "Excluded",
              "Unclassifiable",
              "Favourable",
              "Equivocal",
              "Unfavourable"
            )
          ) %>%
          mutate(var = fct_drop(var))
      ) +
      scale_colour_manual(name = "", values = p1, guide = F)

    # include legend for top row plots only
    if (thisCase %in% c("baumeister", "strack")) { # if standardizing then all cases can use same y axis
      basePlot <- basePlot +
        scale_fill_manual(name = "", values = p1)
    } else {
      basePlot <- basePlot +
        scale_fill_manual(name = "", values = p1, guide = F)
    }
  }

  if (areaPlot == "citesReplication") {
    basePlot <- basePlot +
      geom_area(
        alpha =.5,
        aes(fill = var),
        data = d %>%
          filter(
            dummyYears == F,
            pubYear %in% c(seq(replicationYear + 1, max(pubYear))),
            var %in% c(
              "Excluded",
              "Cites replication",
              "Does not cite replication"
            )
          ) %>%
          mutate(var = fct_drop(var))
      ) +
      scale_colour_manual(name = "", values = p2, guide = F)
    
    # include legend for top row plots only
    if (thisCase %in% c("baumeister", "strack")) { # if standardizing then all cases can use same y axis
      basePlot <- basePlot +
        scale_fill_manual(name = "", values = p2)
    } else {
      basePlot <- basePlot +
        scale_fill_manual(name = "", values = p2, guide = F)
    }
  }

  if (standardized == T) {
    basePlot <- basePlot +
      geom_line(
        data = d %>% # plot citations to target study
          filter(var == "Target study"),
        colour = "black",
        linetype = "solid",
        size = 1
      )
  }

  if (standardized == F) {
    basePlot <- basePlot +
      geom_line(
        data = d %>% # plot citations to target study
          filter(var == "Target study (raw)"),
        colour = "black",
        linetype = "solid",
        size = 1
      )
  }

  basePlot <- basePlot +
    scale_y_continuous(breaks = seq(0, yMax, yInc), expand = c(0, 0)) + # adjust y axis
    annotate( # plot arrow marking replication year
      "segment",
      x = replicationYear,
      y = 125,
      xend = replicationYear,
      yend = 3,
      size = 1,
      arrow = arrow(length = unit(.5, "cm"), type = "closed")
    ) +
    theme_apa(base_size = 20) + # apply APA theme
    theme( # other theme adjustments
      axis.line = element_line(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    ) +
    ggtitle(thisTitle) # add title

  return(basePlot)
}
