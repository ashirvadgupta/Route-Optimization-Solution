    rm(list = ls())
    #setting working directory
    setwd("C:/Insofe/Project/test")
    
    #loading necessary libraries for the project
    library(RMySQL)
    library(animation)
    library(igraph)
    library(networkD3)
    library(magrittr)
    library(ggplot2)
    library(reshape2)
    library(plotly)
    library(htmlwidgets)
    
    #turning warnings off
    options(warn=-1)
    
    #connecting to mysql  and get the data
    db_details = c("gupas05-e7440", "root", "ashirvad", "spring_clean")
    connection = dbConnect(
      MySQL(),
      user = db_details[2],
      password = db_details[3],
      dbname = db_details[4],
      host = db_details[1]
    )
    
    #Listing the tables in the database
    dbListTables(connection)
    
    #readiing tables
    traveltime_info = dbReadTable(connection, "travel_time_matrix_db")
    stops_info = dbReadTable(connection, "stops_info_db")
    
    #fnGetParameters will return constraints values after fetching from mysql tables
    fnGetParameters = function()
    {
      connection = dbConnect(
        MySQL(),
        user = db_details[2],
        password = db_details[3],
        dbname = db_details[4],
        host = db_details[1]
      )
      parameters = dbReadTable(connection, "parameters_info_db")
      parameters2 = dbGetQuery(
        connection,
        "SELECT truncate(time_to_sec(TIMEDIFF(EXPECTED_NOT_AFTER,EXPECTED_NOT_BEFORE))/60,0) as routetimewindow FROM spring_clean.stops_info_db;"
      )
      stops_rs = dbGetQuery(
        connection,
        "select sum(TIME_TO_COMPLETE_WORK) as sum_stops_execution_time,count(STOP_ID) as number_stops from stops_info_db where STOP_ID like 'STP%'"
      )
      sum_stops_execution_time = as.numeric(stops_rs[1])
      number_stops = as.numeric(stops_rs[2])
      uniqueroutetimewindow = unique(parameters2)
      parameters = data.frame(parameters,
                              (uniqueroutetimewindow),
                              sum_stops_execution_time,
                              number_stops)
      return(parameters)
    }
    
    attach(traveltime_info)
    
    #checking structure of data: traveltime_info and stops_info
    head(traveltime_info)
    str(traveltime_info)
    str(stops_info)
    head(stops_info)
    summary(stops_info)
    plot_ly(stops_info,y=~stops_info$TIME_TO_COMPLETE_WORK,color=~stops_info$TIME_TO_COMPLETE_WORK,type="scatter",size=~stops_info$TIME_TO_COMPLETE_WORK) %>% layout(title="TIME TO COMPLETE WORK",xaxis=list(title="STOPS"),yaxis=list(title="TIME TO COMPLETE WORK"))
    
    #total execution time is 10995
    sum(stops_info$TIME_TO_COMPLETE_WORK)

    #checking summary of traveltime_info    
    summary(traveltime_info)
    
    #no of unique stops
    length(unique(FROM_STOP_ID))
    length(unique(TO_STOP_ID))
    
    
    #check missing values in the data
    ##finding the missing stops
    missingstops = unique(TO_STOP_ID)[!(unique(TO_STOP_ID)) %in% (unique(FROM_STOP_ID))]
    
    #imputing the missing stop values with reverse lookup
    missingstops_type1 = traveltime_info[which(traveltime_info$TO_STOP_ID %in% missingstops), ]
    colnames(missingstops_type1) = c("TO_STOP_ID", "FROM_STOP_ID", "TRAVEL_TIME")
    missingstops_type1 = cbind(missingstops_type1[, 2], missingstops_type1[, c(1, 3)])
    colnames(missingstops_type1)[1] = c("FROM_STOP_ID")
    nrow(missingstops_type1)
    missingstops_na = data.frame(
      FROM_STOP_ID = rep(missingstops, times = length(missingstops)),
      TO_STOP_ID = rep(missingstops, each = length(missingstops)),
      TRAVEL_TIME = NA)
    missingstops_na$TRAVEL_TIME[missingstops_na$FROM_STOP_ID == missingstops_na$TO_STOP_ID] =0
    finalstops = rbind(traveltime_info, missingstops_type1, missingstops_na)
    finalstops = finalstops[!finalstops$FROM_STOP_ID == finalstops$TO_STOP_ID, ]
    summary(missingstops_na)
    head(finalstops)
    summary(finalstops)
    
    #no of missing values=72
    sum(is.na(finalstops))
    #Rows with missing values
    subset(finalstops, is.na(finalstops$TRAVEL_TIME))
    
    #funtion to fill missing values with nearest neighbor
    fNFindMissingValues = function(f) {
      missing_stops = subset(f, is.na(f$TRAVEL_TIME))
      if (nrow(missing_stops) > 0) {
        for (i in 1:nrow(missing_stops))
        {
          a = f[f$FROM_STOP_ID == as.character(missing_stops[i, ][1]), ]
          b = a[order(a$TRAVEL_TIME), ][1, ]
          c = f[f$FROM_STOP_ID == as.character(missing_stops[i, ][2]) &
                  f$TO_STOP_ID == b$TO_STOP_ID, ]
          d = f[f$FROM_STOP_ID == as.character(missing_stops[i, ][2]), ]
          e = d[order(d$TRAVEL_TIME), ][1, ]
          g = f[f$FROM_STOP_ID == as.character(missing_stops[i, ][1]) &
                  f$TO_STOP_ID == e$TO_STOP_ID, ]
          t = round((
            b$TRAVEL_TIME + g$TRAVEL_TIME + e$TRAVEL_TIME + c$TRAVEL_TIME
          ) / 2)
          f[f$FROM_STOP_ID == as.character(missing_stops[i, ][1]) &
              f$TO_STOP_ID == as.character(missing_stops[i, ][2]), ]$TRAVEL_TIME = t
        }
      }
      return(f)
    }
    
    #filling missing values
    finalstops = fNFindMissingValues(finalstops)
    sum(is.na(finalstops))
    
    #converting it to distance matrix for more efficient lookup
    traveltime_matrix = acast(data = finalstops, FROM_STOP_ID ~ TO_STOP_ID, fill = 0)
    View(traveltime_matrix)
    
    sum(is.na((traveltime_matrix)))
    dim(traveltime_matrix)
    rm(missingstops, missingstops_na, missingstops_type1)
    #Now distance matrix has no missing values
    
    
    #function to output stops and depots based on type
    #if type=stops, then it will return stops otherwise depots
    calcdepostops <- function(stopid, type) {
      depots = vector()
      stops = vector()
      for (i in unique(stopid)) {
        if (startsWith(i, "DEP"))
          depots = c(depots, i)
        else
          stops = c(stops, i)
      }
      if (type == "stops")
        return(stops)
      else
        return(depots)
    }
    
    #getting stops
    servicestops = calcdepostops(stops_info$STOP_ID, type = "stops")
    
    #getting depots
    depots = calcdepostops(stops_info$STOP_ID, type = "depots")
    
    summary(t(traveltime_matrix[depots, servicestops]))
    
    #function to find minimum values in a vector and randomly return one of the index if more than one values or minimum
    fNMinimum = function(x) {
      minimum = which(x == min(x))
      mins = sample(length(minimum), 1)
      return(minimum[mins])
    }
    
    #funtction to get nearest stops per depots
    #if the distance is from one stop to two or more depots, then it will randomly assign the depots among those depots
    #if names ==TRUE then it will return list of list of stops with names as depots, otherwise it will simply return list of slist of stops nearer to depots
    fNnearestdepots = function(traveltime_matrix,
                               depots,
                               servicestops,
                               name = TRUE) {
      depotstopsdf = as.data.frame(t(traveltime_matrix[depots, servicestops]))
      nearestdepots = list(names(depotstopsdf)[apply(depotstopsdf, 1, fNMinimum)], rownames(depotstopsdf))
      listneareststops = list()
      for (i in 1:length(depots))
      {
        an = list(servicestops[which(nearestdepots[[1]] == depots[i])])
        listneareststops = c(listneareststops, an)
      }
      if (name == TRUE)
        names(listneareststops) = depots
      return(listneareststops)
    }
    
    #function to find probability of selection of depots based upon number of stops near to a depot
    fnProbabilityDepots = function(listneareststops, depots, middlestops) {
      probdepots = vector()
      for (j in 1:length(depots))
      {
        probdepots = c(probdepots,
                       length(listneareststops[[j]]) / length(middlestops))
      }
      return(probdepots)
    }
    
    #function to get job service time in a particular stop
    fNGetServiceTime = function(stp) {
      return(stops_info[stops_info$STOP_ID == stp, c("TIME_TO_COMPLETE_WORK")])
    }
    
    #function to get a dataframe which is a subset of traveltime_matrix containing information about those depots and list of stops which are passed to it in arguments
    fNGetAllTime <- function(depotname, stopsvector) {
      listvec = c(depotname, stopsvector)
      #ls1=setdiff(names,ls)
      t = traveltime_matrix[rownames(traveltime_matrix) %in% listvec, colnames(traveltime_matrix) %in% listvec]
      df = setNames(melt(t), c('FROM_STOP_ID', 'TO_STOP_ID', 'TRAVEL_TIME'))
      df = df[df$FROM_STOP_ID != df$TO_STOP_ID,]
      return(df)
    }
    
    #function will take min and max and total as arguments and return a vector which contains randomly generated elements between min and max arguments which sums upto total value
    #last element can take any value which is left in last
    fNrandomVector = function(min , max , total)
    {
      ran_vec = vector()
      while (total > 0) {
        temp = sample(min:max, 1, replace = TRUE)
        if (total - temp > 0)
          ran_vec = c(ran_vec, temp)
        else
          ran_vec = c(ran_vec, total)
        total = total - temp
      }
      return(ran_vec)
    }
    
    #function which implemented nearest depots neighbour algorithm
    #fNDepotsNearestNeighbor will take minimum and maximum stops as input
    #this function will return routes based upon stops vicinity with respect to depots
    fNDepotsNearestNeighbor = function(minstop, maxstop) {
      listneareststops = fNnearestdepots(traveltime_matrix, depots, servicestops, name = FALSE)
      listneareststopswithnames = fNnearestdepots(traveltime_matrix, depots, servicestops)
      routes = list()
      depnames = names(listneareststopswithnames)
      vehicles = 0
      for (i in 1:length(listneareststops))
      {
        au = vector()
        routesize = fNrandomVector(minstop, maxstop, length(listneareststops[[i]]))
        vehiclefleetsize = length(routesize)
        vehicles = vehicles + vehiclefleetsize
        lookupvec = unlist(listneareststops[i])
        depname = depnames[i]
        atime = fNGetAllTime(depname, lookupvec)
        EXECUTIONTIME = unlist(lapply(atime$TO_STOP_ID, fNGetServiceTime))
        atime = cbind(atime, EXECUTIONTIME)
        TOTALTIME = unlist(lapply(atime$TRAVEL_TIME + atime$EXECUTIONTIME, sum))
        atime = cbind(atime, TOTALTIME)
        atime = atime[order(atime$TOTALTIME), ]
        for (j in 1:vehiclefleetsize)
        {
          rt = vector()
          rt[1] = depname
          size = routesize[j] + 1
          atime = subset(atime,!atime$TO_STOP_ID %in% depots)
          counter = 1
          while ((size) > length(rt))
          {
            #assigning stops to the route based upon minimum distance from that stop
            atime = subset(atime,!atime$TO_STOP_ID %in% au)
            nrow(atime)
            rte = as.character(atime[atime$FROM_STOP_ID == rt[counter],][1,]$TO_STOP_ID)
            au = c(au, rte)
            rt = append(rt, rte)
            counter = counter + 1
          }
          rt = append(rt, depnames[i])
          #adding route to list of routes
          routes = append(routes, list(rt))
        }
      }
      return(routes)
    }
    
    #function will generate initial population of set of chromosome based upon nearest depot neighbors algorithm
    #it will take minstop,maxstop and initialPopulationSize as arguments
    fNGenerateInitalNN = function(minstop, maxstop, initialPopulationSize) {
      poplist = list()
      for (i in 1:initialPopulationSize)
      {
        #calling fNDepotsNearestNeighbor to get one set of chromosomes/routes
        routes = fNDepotsNearestNeighbor(minstop, maxstop)
        poplist = c(poplist, list(routes))
      }
      return(poplist)
    }
    
    
    #function will take two timestamps as arguments and return diiference between them in format minute and seconds
    fNMinutesSeconds = function(endTime, startTime) {
      dif = as.numeric(difftime(endTime, startTime, units = 'min'))
      return(paste(
        sprintf('%02d', as.integer(dif)),
        "minutes and"
        ,
        sprintf('%02.0f', (dif - as.integer(dif)) * 60),
        "seconds"
      ))
    }
    
    #function will take minstop,maxstop and initial Population size as arguments 
    #generate initial population randomly
    fnGenerateInitPop <- function(initPopSize, minstop, maxstop) {
      initPopulation = list()
      seed = Sys.time()
      set.seed(seed)
      print(seed)
      seeds = sample(6000:7000, initPopSize, replace = FALSE)
      listneareststops = fNnearestdepots(traveltime_matrix, depots, servicestops)
      probdepots = fnProbabilityDepots(listneareststops, depots, servicestops)
      for (i in 1:initPopSize) {
        set.seed(seeds[i])
        range_min = minstop
        range_max = maxstop
        chromosome_stopssize = fNrandomVector(range_min, range_max, length(servicestops))
        vehiclefleetsize = length(chromosome_stopssize)
        chromosome_depo = sample(1:length(depots),
                                 vehiclefleetsize,
                                 replace = TRUE,
                                 prob = probdepots)
        chromosome_stops = sample(1:length(servicestops), replace = FALSE)
        solution = list()
        chromosomes_count = 0
        for (j in 1:vehiclefleetsize)
        {
          chromosome = vector()
          #creating one route/chromosome
          chromosome = ifelse(
            chromosome_stopssize[j] > 0,
            c(depots[chromosome_depo[j]], servicestops[chromosome_stops[chromosomes_count +
                                                                          1:chromosome_stopssize[j]]], depots[chromosome_depo[j]]),
            c(depots[chromosome_depo[j]], depots[chromosome_depo[j]])
          )
          if (chromosome_stopssize[j] > 0)
            chromosome = c(depots[chromosome_depo[j]], servicestops[chromosome_stops[chromosomes_count +
                                                                                       1:chromosome_stopssize[j]]], depots[chromosome_depo[j]])
          else
            chromosome = c(depots[chromosome_depo[j]], depots[chromosome_depo[j]])
          chromosomes_count = chromosomes_count + chromosome_stopssize[j]
          #adding one route to set of routes
          solution = c(solution, list(chromosome))
        }
        #adding one set of chromosomes/routes to population list
        initPopulation = c(initPopulation, list(solution))
      }
      return(initPopulation)
    }
    
    #function will take time travelled in a route and maximum route time as input and return extra time window penalty
    fNExtraTimeWindowPenalty = function(timeTravelled, maxroutetime) {
      temp = timeTravelled - maxroutetime
      exceededminpenalty = 0
      routepenalty = 0
      #if timetravelled is less than maximum route time then penalty is 0
      if (temp < 0)
        return(0)
      
      for (i in 1:ceiling(temp / 60)) {
        if (temp > 60)
          exceededminpenalty = exceededminpenalty + 60 * i
        if (temp > 0 & temp <= 60)
          exceededminpenalty = exceededminpenalty + temp * i
        routepenalty = routepenalty + (timeTravelled * i * 0.1)
        temp = temp - 60
      }
      return(exceededminpenalty + routepenalty)
    }
    
    
    #function will take route length, total route time,minstop and maxstop and return extra/less stops penalty
    fNExtraStopPenalty = function(len, sums, minstop, maxstop) {
      #penalty for less/extra stops
      extraStopPenalty = ifelse((len - 2 - minstop) < 0,
                                0.1 * abs(len - 2 - minstop) * sums,
                                ifelse((maxstop - len + 2) < 0, 0.1 * abs(maxstop - len + 2) * sums, 0))
      return(extraStopPenalty)
    }
    
    #function will take route, threshold and route time window for serving a stop and return late time or early arrival penalty
    fNLateTimeArrivalPenalty = function(route, permissibletime, routetimewindow) {
      twpenalty = 0
      routetime = 0
      if (length(route) > 2) {
        for (j in 2:(length(route) - 2))
          ##Travel time per route calculation
        {
          routetime = routetime + traveltime_matrix[route[[j]], route[[j + 1]]] + stops_info[stops_info$STOP_ID == route[[j]], 2]
          penalty3 = fNExtraTimeWindowPenalty(routetime, 2 * permissibletime + routetimewindow)
          twpenalty = twpenalty + penalty3
        }
      }
      return(twpenalty)
    }
    
    #We define the Objective function as follows.
    #function will take chromosome/set of chromosomes, minstop,maxstop,permissible time,max route time, route time window as input
    #optional arguments are penalty,service and travel
    #function will return objective function fitness value if optional arguments are not given
    #function will return only penalty time if penalty =TRUE and other are set as FALSE
    #function will return only service time if service =TRUE and other are set as FALSE
    #function will return only travel time if travel =TRUE and other are set as FALSE
    fnEvaluate <-
      function(chromosome,
               minstop,
               maxstop,
               permissibletime,
               maxroutetime,
               routetimewindow,
               penalty = TRUE,
               service = TRUE,
               travel = TRUE) {
        sumtotal = 0
        routetime = 0
        for (i in chromosome)
        {
          latearrivalpenalty = 0
          stp = strsplit(i, split = " ")
          sums = 0
          #calculating route time
          for (j in 1:(length(stp) - 1))
          {
            if (service)
              sums = sums + stops_info[stops_info$STOP_ID == stp[[j]], 2]
            if (travel)
              sums = sums + traveltime_matrix[stp[[j]], stp[[j + 1]]]
          }
          #calculating penalties
          extratimepenalty = fNExtraTimeWindowPenalty(sums, maxroutetime)
          extrastoppenalty = fNExtraStopPenalty(length(stp), sums, minstop, maxstop)
          latearrivalpenalty = fNLateTimeArrivalPenalty(stp, permissibletime, routetimewindow)
          #print(paste("sum is",sums," penalty1 ",extratimepenalty," penalty2 ",extrastoppenalty," penalty3 ",latearrivalpenalty," k is ",length(stp)-2))
          if (penalty)
            sums = sums + extratimepenalty + extrastoppenalty + latearrivalpenalty
          sumtotal = sumtotal + sums
        }
        return(sumtotal)
      }
    
    #mutate function will take a route,minstop,maxstop,permissible time,max route time, route time window as input
    #function will return a route after random swapping if its fitness value is better than input
    #otherwise it will return input route
    mutate <-  function(individual,
                        minstop,
                        maxstop,
                        permissibletime,
                        maxroutetime,
                        routetimewindow) {
      #Interchange the values of two of the attributes
      tempindividual = individual
      individualfitness = fnEvaluate(
        list(individual),
        minstop,
        maxstop,
        permissibletime,
        maxroutetime,
        routetimewindow
      )
      if (length(individual) > 3) {
        indlen = length(individual) - 1
        rnd = sample(seq(2, indlen, 1), size = 2, replace = FALSE)
        temp = individual[rnd[1]]
        individual[rnd[1]] = individual[rnd[2]]
        individual[rnd[2]] = temp
      }
      
      individualfitness_mut = fnEvaluate(
        list(individual),
        minstop,
        maxstop,
        permissibletime,
        maxroutetime,
        routetimewindow
      )
      #checking if mutated chromosome is better than input chromosome
      if (individualfitness > individualfitness_mut)
        return(individual)
      else
        return(tempindividual)
    }
    #crossover function will take two routes,minstop,maxstop,permissible time,max route time, route time window as input
    #function will return two routes after swapping/cutting/adding the stops in between two routes if sum of fitness values of transformed routes is better than sum of fitness values of input routes
    crossOver = function(p1,
                         p2,
                         minstop,
                         maxstop,
                         permissibletime,
                         maxroutetime,
                         routetimewindow) {
      p1fitness = fnEvaluate(list(p1),
                             minstop,
                             maxstop,
                             permissibletime,
                             maxroutetime,
                             routetimewindow)
      p2fitness = fnEvaluate(list(p2),
                             minstop,
                             maxstop,
                             permissibletime,
                             maxroutetime,
                             routetimewindow)
      
      tempp1 = p1
      tempp2 = p2
      p1len = length(p1) - 1
      p2len = length(p2) - 1
      bool = length(p1) > length(p2)
      if (length(p1) == length(p2)) {
        swap = floor(length(p1) * 0.5)
        if (p1len != 2)
          pos = sample(2:p1len, swap, replace = F)
        else
          pos = 2
        for (l in pos) {
          v = p2[l]
          k = p1[l]
          p1[[l]] = v
          p2[[l]] = k
        }
      }
      else{
        if (bool) {
          if (p2len != 2)
            sp = sample(2:p2len, size = 1, replace = F)
          else
            sp = 2
        }
        else{
          if (p1len != 2)
            sp = sample(2:p1len, size = 1, replace = F)
          else
            sp = 2
        }
        if (length(p1) == 3)
        {
          p1temp = vector()
          p2temp = c(p2[1:(sp)], p1[sp], p2[(sp + 1):length(p2)])
        }
        else if (length(p2) == 3)
        {
          p1temp = c(p1[1:(sp)], p2[sp], p1[(sp + 1):length(p1)])
          p2temp = vector()
        }
        if (length(p1) != 3 & length(p1) < (minstop + 2))
        {
          p1temp = c(p1[1:sp], p2[sp], p1[(sp + 1):length(p1)])
          p2temp = c(p2[1:(sp - 1)], p2[(sp + 1):length(p2)])
        }
        else if (length(p2) != 3 & length(p2) < (minstop + 2))
        {
          p1temp = c(p1[1:(sp - 1)], p1[(sp + 1):length(p1)])
          p2temp = c(p2[1:sp], p1[sp], p2[(sp + 1):length(p2)])
        } else if (length(p2) > (maxstop + 2))
        {
          p1temp = c(p1[1:sp], p2[sp], p1[(sp + 1):length(p1)])
          p2temp = c(p2[1:(sp - 1)], p2[(sp + 1):length(p2)])
        }
        else if (length(p1) > (maxstop + 2))
        {
          p1temp = c(p1[1:(sp - 1)], p1[(sp + 1):length(p1)])
          p2temp = c(p2[1:sp], p1[sp], p2[(sp + 1):length(p2)])
        }
        else{
          p1temp = c(p1[1:(sp - 1)], p2[sp:(length(p2) - 1)], p1[1])
          p2temp = c(p2[1:(sp - 1)], p1[sp:(length(p1) - 1)], p2[1])
        }
        p1 = p1temp
        p2 = p2temp
        #print(p1temp)
        #print(p2temp)
      }
      p1tempfitness = fnEvaluate(list(p1),
                                 minstop,
                                 maxstop,
                                 permissibletime,
                                 maxroutetime,
                                 routetimewindow)
      p2tempfitness = fnEvaluate(list(p2),
                                 minstop,
                                 maxstop,
                                 permissibletime,
                                 maxroutetime,
                                 routetimewindow)
      #checking if childrens are better than parents
      if ((p1fitness + p2fitness) > (p1tempfitness + p2tempfitness))
        return(list(p1, p2))
      else
        return(list(tempp1, tempp2))
    }
    
    #plotGraph function will take fitnessVector and meanFitnessVector as input
    #function will plot a graph of fitness value vs iterations/generations
    plotGraph <- function(fitnessVector, meanfitnessVector) {
      i = length(fitnessVector)
      temp <-
        data.frame(
          Iterations = c(seq(1, i), seq(1, i)),
          Variable = c(rep("Mean", i), rep("Best", i)),
          FitnessValues = c(meanfitnessVector, fitnessVector)
        )
      pl <-
        ggplot(temp,
               aes(
                 x = Iterations,
                 y = FitnessValues,
                 group = Variable,
                 colour = Variable
               )) + geom_line() + scale_x_continuous(limits = c(0, length(fitnessVector)))
      p2 = pl + annotate(
        "text",
        x = i * 0.9,
        y = max(temp$FitnessValues) + i,
        hjust = 1,
        size = 3,
        color = "black",
        label = paste("Best solution:",  min(temp$FitnessValues))
      ) + scale_colour_brewer(palette = "Set1")
      print(p2)
    }
    
    #animatePlot function will take fitnessVector and meanFitnessVector as input
    #function will plot a animated graph of fitness value vs iterations/generations
    animatePlot <- function(fitnessVector, meanfitnessVector) {
      for (i in seq(1, length(fitnessVector))) {
        temp <-
          data.frame(
            Iterations = c(seq(1, i), seq(1, i)),
            Variable = c(rep("Mean", i), rep("Best", i)),
            FitnessValues = c(meanfitnessVector[1:i], fitnessVector[1:i])
          )
        pl <-
          ggplot(temp,
                 aes(
                   x = Iterations,
                   y = FitnessValues,
                   colour = Variable
                 )) + geom_line() + scale_x_continuous(limits = c(0, length(fitnessVector)))
        p2 = pl + annotate(
          "text",
          x = i * 0.9,
          y = max(temp$FitnessValues) + i,
          hjust = 1,
          size = 3,
          color = "black",
          label = paste("Best solution:",  min(temp$FitnessValues))
        ) + scale_colour_brewer(palette = "Set1")
        print(p2)
      }
    }
    
    #fnRunGeneticAlgo will take initPopulation, mutstartProbability,elitePercent, maxIterations,minstop,maxstop,permissibleTime,maxroutetime,routetimewindow,stopcriteria,imageName as input
    #function will run genetic algorithm and return best route after stopping criteria meets or max iterations.
    fnRunGeneticAlgo <-
      function(initPopulation,
               mutstartProbability,
               elitePercent,
               maxIterations,
               minstop,
               maxstop,
               permissibleTime,
               maxroutetime,
               routetimewindow,
               stopcriteria,
               imageName) {
        swf = FALSE
        counter = 0   # is used for stopping criteria
        Plist = list()
        fitnessVector = vector()
        meanfitnessVector = vector()
        cat("max iterations =", maxIterations, "\n")
        # How many winners from each generation?
        origPopSize = length(initPopulation)
        topElite = round(elitePercent * origPopSize, 0)
        fitN = mapply(
          fnEvaluate,
          initPopulation,
          minstop,
          maxstop,
          permissibleTime,
          maxroutetime,
          routetimewindow
        )
        initPopulation = list(initPopulation, fitN)
        sort.fitN = sort.list(fitN)
        sorted.fitN = fitN[sort.fitN]
        initPopulation = initPopulation[[1]][sort.fitN]
        initPopulation = list(initPopulation, sorted.fitN)
        # Main loop
        NewPopulation = initPopulation
        for (i in 1:maxIterations) {
          ElitePop = list(NewPopulation[[1]][1:topElite], NewPopulation[[2]][1:topElite])
          NewPopulation = list(list())
          if (i > 25)
            mut = (mutstartProbability * 2 * (i %/% 5)) / i
          else
            mut = (mutstartProbability) / i
          while (length(NewPopulation[[1]]) <= origPopSize)
          {
            # Mutation
            if (runif(1, 0, 1) < mut) {
              c = sample(1:topElite, 1)
              premutsol = ElitePop[[1]][[c]]
              vehiclefleetsize = length(premutsol)
              muts = sample(1:vehiclefleetsize, 0.1*vehiclefleetsize)
              j = 1
              while (j < length(muts))
              {
                premutsol[[muts[j]]] = mutate(
                  premutsol[[muts[j]]],
                  minstop,
                  maxstop,
                  permissibleTime,
                  maxroutetime,
                  routetimewindow
                )
                j = j + 1
              }
              NewPopulation[[1]][[length(NewPopulation[[1]]) + 1]] = premutsol
            }
            else
            {
              #crossover
              randomsample = sample(1:topElite, 1)
              randomsol = ElitePop[[1]][[randomsample]]
              randomsolobjective = initPopulation[[2]][[randomsample]]
              vehiclefleetsize = length(randomsol)
              crossoverroutes = sample(1:vehiclefleetsize, 0.2*vehiclefleetsize)
              if (length(crossoverroutes) %% 2 != 0)
                crossoverroutes = crossoverroutes[-c(length(crossoverroutes))]
              l = 1
              while (l < length(crossoverroutes))
              {
                crosssol = crossOver(
                  randomsol[[crossoverroutes[l]]],
                  randomsol[[crossoverroutes[l + 1]]],
                  minstop,
                  maxstop,
                  permissibleTime,
                  maxroutetime,
                  routetimewindow
                )
                if (length(crosssol[[1]]) == 2)
                {
                  randomsol[[crossoverroutes[l]]] = NULL
                  randomsol[[crossoverroutes[l + 1]]] = crosssol[[2]]
                }
                else if (length(crosssol[[2]]) == 2)
                {
                  randomsol[[crossoverroutes[l]]] = crosssol[[1]]
                  randomsol[[crossoverroutes[l + 1]]] = NULL
                }
                else
                {
                  randomsol[[crossoverroutes[l]]] = crosssol[[1]]
                  randomsol[[crossoverroutes[l + 1]]] = crosssol[[2]]
                }
                l = l + 2
              }
              NewPopulation[[1]][[length(NewPopulation[[1]]) + 1]] = randomsol
            }
          }
          fitN = mapply(
            fnEvaluate,
            NewPopulation[[1]],
            minstop,
            maxstop,
            permissibleTime,
            maxroutetime,
            routetimewindow
          )
          NewPopulation = list(NewPopulation[[1]], fitN)
          sort.fitN = sort.list(fitN)
          sorted.fitN = fitN[sort.fitN]
          NewPopulation = NewPopulation[[1]][sort.fitN]
          NewPopulation = list(NewPopulation, sorted.fitN)
          # stopping criteria
          if (i == 1 | i %% 25 == 0)
            cat(
              "Iteration:",
              i,
              " New Population Fitness: ",
              as.numeric(NewPopulation[[2]][[1]]),
              " vehicles: ",
              vehiclefleetsize,
              " counter is: ",
              counter + 1,
              "\n"
            )
          fitnessVector = c(fitnessVector, NewPopulation[[2]][[1]])
          meanfitnessVector = c(meanfitnessVector, mean(NewPopulation[[2]]))
          # stopping criteria
          if (i > 1) {
            if (NewPopulation[[2]][[1]] == Plist[[length(Plist)]][1]) {
              counter = counter + 1
              if (counter == stopcriteria) {
                break()
              }
            } else{
              counter = 0
            }
          }
          #adding population to Plist for comparision
          Plist = c(Plist, NewPopulation)
        }
        jpeg(file = imageName)
        plotGraph(fitnessVector, meanfitnessVector)
        dev.off()
        #animated plot
        if (swf == TRUE)
        {
          plot = animate_plot(fitnessVector, meanfitnessVector)
          saveSWF(
            expr = animate_plot(fitnessVector, meanfitnessVector),
            interval = 0.5,
            swf.name = paste0(imageName,".swf"),
            loop = 1,
            out.dir = getwd()
          )
        }
        # Print current best score
      #calculating travel time, penalty time and service time
        traveltime = fnEvaluate(
          NewPopulation[[1]][[1]],
          minstop,
          maxstop,
          permissibleTime,
          maxroutetime,
          routetimewindow,
          penalty = FALSE,
          service = FALSE
        )
        servicetime = fnEvaluate(
          NewPopulation[[1]][[1]],
          minstop,
          maxstop,
          permissibleTime,
          maxroutetime,
          routetimewindow,
          penalty = FALSE,
          travel = FALSE
        )
        penaltytime = fnEvaluate(
          NewPopulation[[1]][[1]],
          minstop,
          maxstop,
          permissibleTime,
          maxroutetime,
          routetimewindow,
          penalty = TRUE
        ) - traveltime - servicetime
        num_vehicles = length(NewPopulation[[1]][[1]])
        print(paste(
          "TOTAL TIME FOR THE SOLUTION IS:",
          fnEvaluate(
            NewPopulation[[1]][[1]],
            minstop,
            maxstop,
            permissibleTime,
            maxroutetime,
            routetimewindow
          )
        ))
        print(
          paste(
            "TRAVEL TIME FOR THE SOLUTION IS:",
            traveltime,
            "SERVICE TIME FOR THE SOLUTION IS: ",
            servicetime,
            "PENALTY TIME FOR THE SOLUTION IS: ",
            penaltytime,
            " TOTAL STOPS COVERED: ",
            length(unique(unlist(NewPopulation[[1]][[1]]))) - 6
          )
        )
        #final solution after GA
        final_list = list(
          NewPopulation[[1]][[1]],
          NewPopulation[[2]][[1]],
          traveltime,
          servicetime,
          penaltytime,
          num_vehicles
        )
        names(final_list) = c(
          "routes",
          "totaltime",
          "traveltime",
          "servicetime",
          "penaltytime",
          "numberOfVehicles"
        )
        return(final_list)
      }
    
    
    #closeMySQLconnections will close existing open mysql connections
    closeMySQLconnections = function() {
      dbListConnections(dbDriver(drv = "MySQL"))
      lapply(dbListConnections(dbDriver(drv = "MySQL")), dbDisconnect)
    }
    
    
    #fnExecuteMain will take initial Population Size, Mutation Start Probability,
    #Elite Percentage, Max no of iterations,stopping criteria,number of times as input
    #random is an optional input
    #if random is false then, initial population consists of solutionn generated by nearest depot neighbour algorithm
    #otherwise, initial Population consists of randomly generated set of solutions
    #function will return best routes for every set of parameter
    #function will run for every parametes based upon number of times
    #function will output set of routes for one solution in text file
    #function also store the total fitness value for a soulution and text file path in mysql table results
    fnExecuteMain <-
      function(initPopulationSize,
               mutstartProbability,
               elitePercent,
               maxIterations,
               stopCriteria,
               numberOfTimes,
               random=FALSE,dnn=FALSE) {
        closeMySQLconnections()
        parameters = fnGetParameters()
        solutionlist = list()
        result_df = as.data.frame(setNames(
          replicate(7, numeric(0), simplify = T),
          c(
            "MAX_STOPS_PER_ROUTE",
            "MIN_STOPS_PER_ROUTE",
            "MAX_ROUTE_TIME",
            "MAX_EARLY_TIMEWINDOW",
            "MAX_LATE_TIMEWINDOW",
            "OBJECTIVE_VALUE",
            "FILEPATH"
          )
        ))
        for (p in 1:nrow(parameters))
        {
          minstop = parameters[p, 2]
          maxstop = parameters[p, 1]
          permissibleTime = parameters[p, 4]
          maxroutetime = parameters[p, 3]
          routetimewindow = parameters[p, 6]
          for (j in 1:numberOfTimes)
          {
            old = Sys.time()
            #dnn==FALSE, then GA models will execute
            if(dnn==FALSE){
              #random==TRUE, then initial population is randomly generated
            if(random==FALSE)
            initPopulation = fNGenerateInitalNN(
              minstop = minstop,
              maxstop = maxstop,
              initialPopulation = initPopulationSize
            )
            else
              initPopulation = fnGenerateInitPop(initPopulationSize, maxstop = maxstop, minstop =
                                                   minstop)
            new = Sys.time()
            print(paste(
              "TIME TAKEN TO GENERATE INITIAL POPULATION:",
              fNMinutesSeconds(new, old)
            ))
            print(
              paste(
                "minstop:",
                minstop,
                " maxstop:",
                maxstop,
                " loop:",
                j,
                " permissible time ",
                permissibleTime
              )
            )
            old = Sys.time()
            imagename = paste0(minstop,
                               "_",
                               maxstop,
                               "_",
                               permissibleTime,
                               "_",
                               j,
                               ".jpeg")
            solution = fnRunGeneticAlgo(
              initPopulation = initPopulation,
              mutstartProbability = mutstartProbability,
              elitePercent = elitePercent,
              maxIterations = maxIterations,
              minstop = minstop,
              maxstop = maxstop,
              permissibleTime  = permissibleTime,
              maxroutetime = maxroutetime,
              routetimewindow = routetimewindow,
              stopcriteria  = stopCriteria,
              imageName = imagename
            )
            }
            else{
              routes=fNNearestDepotsNeighbor(minstop,maxstop)
              totaltime=fnEvaluate(routes,minstop,maxstop,permissibleTime,maxroutetime,routetimewindow)
              traveltime=fnEvaluate(routes,minstop,maxstop,permissibleTime,maxroutetime,routetimewindow,penalty =F,service=F)
              servicetime=fnEvaluate(routes,minstop,maxstop,permissibleTime,maxroutetime,routetimewindow,travel=F,penalty=F)
              penaltytime=totaltime-traveltime-servicetime
              numberOfVehicles=length(routes)
              solution = list(
                routes,
                totaltime,
                traveltime,
                servicetime,
                penaltytime,
                numberOfVehicles
              )
              names(solution) = c(
                "routes",
                "totaltime",
                "traveltime",
                "servicetime",
                "penaltytime",
                "numberOfVehicles"
              )
            }
            new = Sys.time()
            print(paste(
              "TIME TAKEN TO GENERATE SOLUTION:",
              fNMinutesSeconds(new, old)
            ))
            filename = paste0(
              minstop,
              "_",
              maxstop,
              "_",
              permissibleTime,
              "_",
              solution$totaltime,
              "_",
              j,
              ".txt"
            )
            lapply(solution$routes,
                   write,
                   filename,
                   append = TRUE,
                   ncolumns = 1000)
            filepath = paste0(getwd(), "/", filename)
            result_df = rbind(
              result_df,
              data.frame(
                maxstop,
                minstop,
                maxroutetime,
                permissibleTime,
                permissibleTime,
                solution$totaltime,
                filepath
              )
            )
            solutionlist = c(solutionlist, solution)
          }
        }
        colnames(result_df) <-
          c(
            "MAX_STOPS_PER_ROUTE",
            "MIN_STOPS_PER_ROUTE",
            "MAX_ROUTE_TIME",
            "MAX_EARLY_TIMEWINDOW",
            "MAX_LATE_TIMEWINDOW",
            "OBJECTIVE_VALUE",
            "FILEPATH"
          )
        conn = dbConnect(
          MySQL(),
          user = db_details[2],
          password = db_details[3],
          dbname = db_details[4],
          host = db_details[1]
        )
        #results are stored in mysql table results
        dbWriteTable(
          conn,
          value = result_df,
          name = "results",
          append = TRUE,
          row.names = FALSE,
          overwrite = FALSE
        )
        closeMySQLconnections()
        return(solutionlist)
      }
    
    
    #executing fnExecuteMain with dnn=TRUE and storing the results in finalsolutionNN list
    finalsolutionNN = fnExecuteMain(initPopulationSize = 100,mutstartProbability  = 0.8,
      elitePercent = 0.2,maxIterations  = 1000,stopCriteria = 70,numberOfTimes = 10,dnn=TRUE)
    
    #executing fnExecuteMain with random =FALSE and storing the results in finalsolution1 list
    finalsolution1 = fnExecuteMain(initPopulationSize = 100,mutstartProbability  = 0.8,
                                    elitePercent = 0.2,maxIterations  = 1000,stopCriteria = 70,numberOfTimes = 10)
    #executing fnExecuteMain with random population and storing the results in finalsol list
    finalsol = fnExecuteMain(initPopulationSize = 100,mutstartProbability  = 0.8,
                                    elitePercent = 0.2,maxIterations  = 1000,stopCriteria = 70,numberOfTimes = 10,random = TRUE)
    
    
    #fNDataFrame will be used by fNVisualization
    fNDataFrame = function(x) {
      dff <-
        as.data.frame(setNames(
          replicate(5, numeric(0), simplify = F),
          c("src", "target", "value", "route", "val")
        ))
      counter = 1
      cnt = 1
      for (i in 1:(length(x) - 1)) {
        if ((startsWith(x[i], "DEP") & startsWith(x[i + 1], "DEP"))) {
          counter = counter + 1
          cnt = 1
        }
        else
        {
          dff[i - counter + 1, 1] = x[i]
          dff[i - counter + 1, 2] = x[i + 1]
          dff[i - counter + 1, 3] = traveltime_matrix[x[i], x[i + 1]]
          dff[i - counter + 1, 4] = counter
          dff[i - counter + 1, 5] = cnt
          cnt = cnt + 1
        }
      }
      return(dff)
    }
    #fNTravelMatrix will be used by fNVisualization
    fNTravelMatrix = function(finalstops, x, traveltime_matrix) {
      k = length(finalstops)
      tm = matrix(rep(0, k * k), k, k)
      rownames(tm) = colnames(tm) = finalstops
      for (i in 1:(length(x) - 1))
      {
        if (!(startsWith(x[i], "DEP") & startsWith(x[i + 1], "DEP"))) {
          if (traveltime_matrix[x[i], x[i + 1]] == 0)
            tm[x[i], x[i + 1]] = 1
          else
            tm[x[i], x[i + 1]] = traveltime_matrix[x[i], x[i + 1]]
        }
      }
      return(tm)
    }
    #fNRoutes will be used by fNVisualization
    fNRoutes = function(nodes, df) {
      grp = rep(1, nrow(nodes))
      nodes = data.frame(nodes, grp)
      for (i in 1:nrow(df)) {
        if (startsWith(df[i, "target"], "ST"))
          nodes[nodes$unique_solvec == df[i, "target"],]$grp = df[i, "route"]
        else{
          nodes[nodes$unique_solvec == df[i, "target"],]$grp = max(df[, "route"]) +
            1
        }
      }
      return(nodes)
    }
    
    
    #function will take finalsolution and names of nodes(depots/stops) as input
    #function will create forced network graph for all the nodes in a solution in html files
    fNVisualizations = function(finalsol, names) {
      for (i in seq(1, length(finalsol), 6))
      {
        solvec = unlist(finalsol[i]$routes)
        unique_solvec = names
        nodes = data.frame(unique_solvec)
        tm = fNTravelMatrix(names, solvec, traveltime_matrix)
        nd = fNDataFrame(solvec)
        nodes = fNRoutes(nodes, nd)
        g <- graph.adjacency(tm, weighted = T)
        sN = simpleNetwork(
          nd,
          linkColour = "black",
          nodeColour = "green",
          opacity = 2,
          zoom = TRUE
        )
        filename_sn = paste0(
          "simpleNw_",
          round(finalsol[i + 1]$totaltime),
          "_",
          round(finalsol[i + 5]$numberOfVehicles),
          "_",
          i,
          ".html"
        )
        filename_fn = paste0(
          "forcedNw_",
          round(finalsol[i + 1]$totaltime),
          "_",
          round(finalsol[i + 5]$numberOfVehicles),
          "_",
          i,
          ".html"
        )
        #sN%>% saveNetwork(file = filename_sn)
        graphD3 <- igraph_to_networkD3(g, group = nodes)
        val = rep(1, nrow(graphD3$links))
        graphD3$links = data.frame(graphD3$links, val)
        # Create force directed network plot
        forcenet = forceNetwork(
          Links = graphD3$links,
          Nodes = graphD3$nodes,
          Source = 'target',
          Target = 'source',
          Value = "val",
          NodeID = 'name',
          Group = 'grp',
          opacity = 4,
          linkColour = "#000",
          zoom = TRUE,
          opacityNoHover = TRUE,
          #colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"),
          arrows = TRUE
        )
        forcenet %>% saveNetwork(file = filename_fn)
      }
    }
    
    #executing the fNVisualizations for finalsolutionNN
    fNVisualizations(finalsolutionNN,names)
    #executing the fNVisualizations for finalsolutionNN
    fNVisualizations(finalsolution1, names)  
    #executing the fNVisualizations for finalsolutionNN
    fNVisualizations(finalsol, names)  
    
    #fnVisualizeSolutionFitness will take finalsoltution, number of times you ran for each parameter set,
    #number of parameters, and imagename as input
    #function will save the different plots for a solution in working directory 
    fnVisualizeSolutionFitness = function(finalsol,numberOfTimes,numberOfParameters,imagename) {
      closeMySQLconnections()
      totalcost = vector()
      totaltime = vector()
      penaltytime = vector()
      traveltime = vector()
      servicetime = vector()
      numberOfVehicles = vector()
      parameters = fnGetParameters()
      parameter = vector()
      vehiclescostpermin = 17 / 60
      servicecostpermin = 14 / 60
      penaltycostpermin = 17 / 60
      ownershipcostperday=17.59
      for (j in 1:numberOfParameters)
      {
        for (i in seq((j - 1) * numberOfTimes * 6 + 1, j * 6 * numberOfTimes, 6))
        {
          parameter = c(
            parameter,
            paste0(
              "STOPS ",
              parameters$MIN_STOPS_PER_ROUTE[j],
              "-",
              parameters$MAX_STOPS_PER_ROUTE[j],
              " THRESHOLD TIME: ",
              parameters$MAX_EARLY_TIMEWINDOW[j]
            )
          )
          totalcost = c(
            totalcost,
            round(finalsol[i + 2]$traveltime * vehiclescostpermin + finalsol[i + 3]$servicetime *
                    servicecostpermin + round(finalsol[i + 4]$penaltytime) * penaltycostpermin+finalsol[i+5]$numberOfVehicles*ownershipcostperday)
          )
          totaltime = c(totaltime, finalsol[i + 1]$totaltime)
          traveltime = c(traveltime, finalsol[i + 2]$traveltime)
          servicetime = c(servicetime, finalsol[i + 3]$servicetime)
          penaltytime = c(penaltytime, round(finalsol[i + 4]$penaltytime))
          numberOfVehicles = c(numberOfVehicles, finalsol[i + 5]$numberOfVehicles)
        }
        
      }
        dataf = cbind(
        parameter,
        totaltime,
        traveltime,
        servicetime,
        penaltytime,
        numberOfVehicles,
        totalcost
      )
      dataf = as.data.frame(dataf)
      #write.csv(dataf,"sol3.csv")
      f <- list(family = "Calibri",size = 18,color = "#000")
      #number of vehicles vs total cost, size= reverse penalty
      p = plot_ly(dataf,x = numberOfVehicles, y = totalcost,text =  paste("Total Cost is: ",totalcost,"Number of Vehicles: ",numberOfVehicles,"Travel Time is:",traveltime,"Total Time is:",round(totaltime),"Penalty Time is:",round(penaltytime),
                                                                          "Parameters:",parameter),mode = "markers",color = parameter,size = -penaltytime,alpha = 1) %>% layout(title = "NUMBER OF VEHICLES VS TOTAL COST",xaxis = list(title = "NUMBER OF VEHICLES",titlefont = f), yaxis = list(title = "TOTAL COST",titlefont = f))
      
      saveWidget(p, paste0("cost-vehicless",imagename))
      #penaltytime vs total cost, size=numberofvehicles
      p7 = plot_ly(dataf,x = penaltytime, y = totalcost,text =  paste("Total Cost is: ",totalcost,"Number of Vehicles: ",numberOfVehicles,"Travel Time is:",traveltime,"Total Time is:",round(totaltime),"Penalty Time is:",round(penaltytime),
                                                                      "Parameters:",parameter),mode = "markers",color = parameter,size = numberOfVehicles,alpha = 1) %>% layout(title = "PENALTY TIME VS TOTAL COST") %>% layout(xaxis = list(title = "PENALTY TIME",titlefont = f), yaxis = list(title = "TOTAL COST",titlefont = f))
      
      saveWidget(p7, paste0("penalty-cost",imagename))
      #number of vehicles box plot, color=parameters
      p1 <- plot_ly(dataf,x=~numberOfVehicles, y = ~totalcost, color = ~parameter, type = "box") %>% layout(xaxis=list(title = "NUMBER OF VEHICLES",titlefont = f),yaxis=list(title = "TOTAL COST",titlefont = f)) 
      saveWidget(p1, paste0("vehicles-cost-box",imagename))
      p2 <- plot_ly(dataf,x=~numberOfVehicles, y = ~traveltime, color = ~parameter, type = "box") %>% layout(xaxis=list(title = "NUMBER OF VEHICLES",titlefont = f),yaxis=list(title = "TRAVEL TIME",titlefont = f)) 
      saveWidget(p2, paste0("vehicles-traveltime-box",imagename))
      #parameters number of vehicles box plot
      p4 <- plot_ly(dataf, y = ~numberOfVehicles, color = ~parameter, type = "box") %>% layout(yaxis=list(title = "NUMBER OF VEHICLES",titlefont = f)
                    ,xaxis=list(title = "PARAMETERS",titlefont = f),title=" NUMBER OF VEHICLES BOX PLOT")  
      saveWidget(p4, paste0("vehicles-box",imagename))
      #parameters total time box plot
      p5 <- plot_ly(dataf, y = ~totaltime, color = ~parameter, type = "box") %>% layout(yaxis=list(title = "TOTAL TIME",titlefont = f)
                    ,xaxis=list(title = "PARAMETERS",titlefont = f),title=" TOTAL TIME BOX PLOT")  
      saveWidget(p5, paste0("totaltime-box",imagename))
      #parameters travel time box plot
      p6 <- plot_ly(dataf, y = ~traveltime, color = ~parameter, type = "box")  %>% layout(yaxis=list(title = "TRAVEL TIME",titlefont = f)
                    ,xaxis=list(title = "PARAMETERS",titlefont = f),title=" TRAVEL TIME BOX PLOT") 
      saveWidget(p6, paste0("traveltime-box",imagename))
      #number of vehicles vs travel time
      p8 = plot_ly(dataf,x = numberOfVehicles, y = traveltime,text =  paste("Total Time is: ",totaltime,"Number of Vehicles: ",numberOfVehicles,"Travel Time is:",traveltime,"Total Time is:",round(totaltime),"Penalty Time is:",round(penaltytime),
                                                                            "Parameters:",parameter),mode = "markers",color = parameter,size = -penaltytime,alpha = 1) %>% layout(title = "NUMBER OF VEHICLES VS TOTAL TIME") %>% layout(xaxis = list(title = "NUMBER OF VEHICLES",titlefont = f), yaxis = list(title = "TOTAL TIME",titlefont = f))
      saveWidget(p8, paste0("totaltime-vehicles",imagename))
      totalcost_order = order(dataf$totalcost)
      besttotal = dataf[totalcost_order, ]
      return(besttotal)
    }
    
    #executing the fnVisualizeSolutionFitness for all the solutions
    viz=fnVisualizeSolutionFitness(finalsolutionNN,10,6, imagename="_2.html")
    viz1=fnVisualizeSolutionFitness(finalsolution1,10,6, imagename="_1.html")
    viz2=fnVisualizeSolutionFitness(finalsol,10,6, imagename="_3.html")
    finalsolutionNN
    #plots for model comparision
    sol1=read.csv("sol1.csv",header=T)
    Type=rep("Random Population GA Model",60)
    sol1=cbind(sol1,Type)
    sol2=read.csv("sol21.csv",header=T)
    Type=rep("DNN Population GA Model",60)
    sol2=cbind(sol2,Type)
    sol3=read.csv("sol3.csv",header=T)
    Type=rep("DNN",60)
    sol3=cbind(sol3,Type)
    compsol=rbind(sol1,sol2,sol3)
    #compsol=read.csv("models.csv",header = T)
    attach(compsol)
    #plotting the graphs using plotly
    t <- list(
      family = "calibri",
      size = 14,
      color = toRGB("grey50"))
    q = plot_ly(compsol,x = compsol$numberOfVehicles, y = compsol$totalcost,text =  paste("Model:",compsol$Type,"Total Cost is: ",compsol$totalcost,"Number of Vehicles: ",compsol$numberOfVehicles,"Travel Time is:",compsol$traveltime,"Total Time is:",round(compsol$totaltime),"Penalty Time is:",round(compsol$penaltytime),
                                                                        "Parameters:",compsol$parameter),mode = "markers",color = compsol$Type,size = -compsol$penaltytime,alpha = 1) %>% layout(title = "NUMBER OF VEHICLES AND PENALTY TIME VS TOTAL COST FOR ALL MODELS") %>% layout(xaxis = list(title = "NUMBER OF VEHICLES",titlefont = f), yaxis = list(title = "TOTAL COST",titlefont = f))
    q
    saveWidget(q, paste0("comp1.html"))
    q1 = plot_ly(compsol,x = compsol$numberOfVehicles, y = compsol$totaltime,text =  paste("Model:",compsol$Type,"Total Cost is: ",compsol$totalcost,"Number of Vehicles: ",compsol$numberOfVehicles,"Travel Time is:",compsol$traveltime,"Total Time is:",round(compsol$totaltime),"Penalty Time is:",round(compsol$penaltytime),
                                                                                          "Parameters:",compsol$parameter),mode = "markers",color = compsol$Type,size = -compsol$penaltytime,alpha = 1) %>% layout(title = "NUMBER OF VEHICLES AND PENALTY TIME VS TOTAL TIME FOR ALL MODELS") %>% layout(xaxis = list(title = "NUMBER OF VEHICLES",titlefont = f), yaxis = list(title = "TOTAL TIME/FITNESS VALUE",titlefont = f))
    q1
    saveWidget(q1, paste0("comp2.html"))
    
    
    q2 = plot_ly(compsol,x = compsol$penaltytime, y = compsol$totaltime,text =  paste("Model:",compsol$Type,"Total Cost is: ",compsol$totalcost,"Number of Vehicles: ",compsol$numberOfVehicles,"Travel Time is:",compsol$traveltime,"Total Time is:",round(compsol$totaltime),"Penalty Time is:",round(compsol$penaltytime),
                                                                                         "Parameters:",compsol$parameter),mode = "markers",color = compsol$Type,size = -compsol$traveltime,alpha = 1) %>% layout(title = "PENALTY TIME VS TOTAL COST FOR ALL MODELS") %>% layout(xaxis = list(title = "PENALTY TIME",titlefont = f), yaxis = list(title = "TOTAL COST",titlefont = f))
    q2
    saveWidget(q2, paste0("comp3.html"))
    
    q5 <- plot_ly(dataf, x= ~compsol$Type,y = ~compsol$totaltime, color = ~compsol$Type, type = "box")  %>% layout(yaxis=list(title = "TOTAL TIME/FITNESS VALUE",titlefont = f)
                                                                                       ,xaxis=list(title = "MODELS",titlefont = f),title=" TOTAL TIME BOX PLOT FOR ALL MODELS")   
    q5
    saveWidget(q5, paste0("comp4.html"))
    
    q6 <- plot_ly(dataf,x=~compsol$Type, y = ~compsol$totalcost, color = ~compsol$Type, type = "box")  %>% layout(text=totalcost,yaxis=list(title = "TOTAL COST",titlefont = f)
                                                                                                                              ,xaxis=list(title = "MODELS",titlefont = f),title=" TOTAL COST BOX PLOT FOR ALL MODELS")  
    q6
    saveWidget(q6, paste0("comp5.html"))
    
    
    q7 <- plot_ly(dataf,x=~compsol$numberOfVehicles, y = ~compsol$traveltime, color = ~compsol$Type, type = "box")  %>% layout(yaxis=list(title = "TRAVEL TIME",titlefont = f)
                                                                                                                              ,xaxis=list(title = "NUMBER OF VEHICLES",titlefont = f),title=" TRAVEL TIME BOX PLOT VS NUMBER OF VEHICLES FOR ALL MODELS")  
    q7
    saveWidget(q7, paste0("comp6.html"))
    