

# scenarios ---------------------------------------------------------------

scenario_income <- "LIC" #must be "LIC", "MIC-S", "MIC-I", or "HIC"
scenario_prod <- "HCA" #must be "HCA" or "FCA"
scenario_transmission <- "med" #must be "low", "med", "hi" or "max"
scenario_frequency <- "biennial" #must be "annual", "biennial" or "triennial"
scenario_farm_effect <- "med" #must be "min", "lo", "med", "hi" or "max"

## 
# Main Model --------------------------------------------------------

  inputs <- read.csv(here("inputs - general model.csv"))
  inputs <- as.data.table(inputs)
  colnames(inputs) <- c("parameter", "description", "HIC", "MIC-I", "MIC-S", "LIC", "Value", "Min", "Lo", "Med", "Hi", "Max", 
                        "LIC min", "LIC max", "MIC min", "MIC max", "HIC min", "HIC max")
  
  if(scenario_income == "HIC"){
    inputs[,"Value"] <- inputs[,"HIC"]
  } else if(scenario_income == "LIC"){
    inputs[,"Value"] <- inputs[,"LIC"]
  } else if(scenario_income == "MIC-I"){
    inputs[,"Value"] <- inputs[,"MIC-I"]
  } else if(scenario_income == "MIC-S"){
    inputs[,"Value"] <- inputs[,"MIC-S"]
  }
  
  inputs[ , Value := as.numeric(as.character(Value))]
  
  f_expvalue <- function(x,y,z){
    ## x is the epi matrix
    ## y is the cost matrix
    ## z is the reward matrix
    results <- matrix(c(sum(x*y),sum(x*z)),1,2)
    return(results)
    
  }
  
  f_di <- function(x,y){
    # function to apply a discount rate
    # x is cost
    # y is discount rate 
    x2 <- x - (x*y)
    return(x2)
  }
  
  n.t <- inputs[parameter=="n.t",Value] + 1
  
  
  # Population growth -------------------------------------------------------
  
  ## nn - haven't checked this bit in detail but but can check structure by checking
  # for e.g. first few years estimates do they match what you would expect
  # against WB estimates etc. 
  
  population <- c(rep(0,n.t+1))
  population[1] <- inputs[parameter == "pop", Value]
  for(i in 2:length(population)){
    population[i] <- population[i-1]*(1+inputs[parameter=="pop_growth", Value])
  }
  
  popchange <- c(rep(0,n.t))
  for(i in 1:n.t){
    popchange[i] <- population[i+1]-population[i]
  }
  
  # Human Epi Model ---------------------------------------------------------
  
  #building parameter matrix
  state_names <- c("well", "res","sus","dead", "afterlife", "seq") ## the compartments
  transition_names  <- c("birth","r","s","mort_r", "mort_s","mort_w", "rec_r","rec_s", "dead_aft", "r_seq", "s_seq")  ## the transition probabilities
  parameter_names <- c(state_names, transition_names)
  
  state_i <- c(inputs[parameter=="pop",Value], rep(0,length=length(state_names)-1))
  #initial state vector
  
  tm <- array(data=0,dim=c(n.t, 
                               3, length(parameter_names)),
                   dimnames= list(1:n.t, c("base", "2disease",
                                                     "recover.dead"),
                                  parameter_names)) 
  
## vector for growth in resistance overtime:
  amr_growth <- inputs[parameter=="amr_grow", Value]
  r.vec <- c(inputs[parameter=="portion_res",Value],rep(0, n.t-1))
  for (i in 2:n.t){
  r.vec[i] <- r.vec[i-1]*amr_growth
} #TE what about maximum resistance prevalence?

  ## defining resistance overtime in the tm
  tm[ ,"base" ,"r"] <- r.vec*inputs[parameter=="well_sick", Value]
  ## nn - note this means we are assuming no change in infection rates
  # just resistnace
  # could change infection rates also using same approach in terms of vector multiplication
  
  
## doing the same for susceptible overtime:  
  s.vec <- 1-r.vec
  
  tm[ ,"base" ,"s"] <- s.vec*inputs[parameter=="well_sick", Value]
  
## so from net births - there is no modelled mortality
  # so we are essentially assuming all births and background deaths
  # happen in the beginning of the year 
# nn -Gwen ok with this?
# need to then remember reporting incremental death/qaly impact only
# (which is fine but just need to remember this in write up)
  
  tm[ ,"2disease" ,"r_seq"] <- rep(inputs[parameter=="seq_res", Value], n.t) 
  #chance of developing sequelae following resistant infection
  
  tm[ ,"2disease","s_seq"] <- rep(inputs[parameter=="seq_sus", Value], n.t) 
  #chance of developing sequelae following susceptible infection
  
## nn - so we need to highlight stay in seq for the rest of their lives? do you not want incidence over prevalence?  
# might want to make clear in the manual and could just have one "dumping" state change afterlife state name to "outofmodel" or something more generic
# could we talk through again in a meeting how the costs and qalys are calculated
# for all of the different health states 
# maybe with a walk through of one specific example with numbers?
  
  tm[ , "2disease","mort_r"] <- rep(inputs[parameter=="mort_res", Value], n.t)
  #fatality from resistant infection
  
  tm[ ,"2disease" ,"mort_s"] <- rep(inputs[parameter=="mort_sus", Value], n.t)
  #fatality from susceptible infection
  
  tm[ , "2disease","rec_r"] <- rep(max(0,(1-(inputs[parameter=="mort_res", Value]+
                                               inputs[parameter=="seq_res", Value]))), n.t)
  ## nn - would need to update this to a vector calculation if morality rates and r_seq were
  ## to change over time 
  #chance of recovering from a resistant infection
  
  tm[ , "2disease","rec_s"] <-  rep(max(0,(1-(inputs[parameter=="mort_sus", Value]+
                                                inputs[parameter=="seq_sus", Value]))), n.t)
  ## nn - would need to update this to a vector calculation if morality rates and s_seq were
  ## to change over time 
  #chance of recovering from a susceptible infection
  
  ## nn - i see why would set max to 0, but if it is <0 need
  # to figure out why and if mort_r and r_seq >1 then that means that there's more transitions than should be theoretically possible 
  ## leading to wrongful population inflation
  # does it make sense that all people with resistant infection either die or get sequaelae? not sure it does
  ## r_seq seems high - but i guess the QOL detriment isn't much so maybe not that much of an impact?
  # maybe worth talking through, Cassini et al estimated 9.4 - 20.3% for DRI BSI so worth incorporating into PSA ?
  
  ## nn - way around it could be to add code that if mort_r + r_seq > 1, destribute the difference across 1
  # e.g. 0.575 becomes 0.575/(0.575/1.06262) + 0.48762 becomes 0.48762/1.06262 where 1.06262=0.575+ 0.48762
  #   # please try adding in, Gwen check if happy with this suggestion ?

  #predicted net births in a given year - previously you use absoulte numbers
  ## nn - i think we should really try and make this a rate rather than a fixed value
  # ideally birth and death rates
  # as just having a fixed value means we are assuming the number of births and deaths is exogenous
  # to the number of people in the population - Gwen thoughts?
  # I have used popgrowth value in the below - note we also assume this is fixed overtime

  
  tm[ ,"base","birth"] <- rep(inputs[parameter=="pop_growth", Value],n.t)
  
  ## first run
  tm[1 , "base", "well"] <- population[1] 
  tm[1,"2disease","well"] <- tm[1,"base","well"] - 
                                        (tm[1,1,"r"]*tm[1,1,"well"]) -
                                        (tm[1,1,"s"]*tm[1,1,"well"]) 
  
  tm[1,"2disease","res"] <- (tm[1,1,"r"]*tm[1,1,"well"]) 
  tm[1,"2disease","sus"] <- (tm[1,1,"s"]*tm[1,1,"well"]) 
  ## nn-also note we're assuming people get one or the other (similar to other models but worth flagging)
  tm[1,"recover.dead","well"] <-tm[1,2,"well"]+
                            (tm[1,2,"res"]*tm[1,2,"rec_r"])+
                              (tm[1,2,"sus"]*tm[1,2,"rec_s"])
  
  tm[1,"recover.dead","seq"] <-(tm[1,2,"res"]*tm[1,2,"r_seq"])+
                                   (tm[1,2,"sus"]*tm[1,2,"s_seq"])
  
  tm[1,"recover.dead","dead"] <- (tm[1,2,"res"]*tm[1,2,"mort_r"])+
                                    (tm[1,2,"sus"]*tm[1,2,"mort_s"])
    
 ## for totals :
  all <- colSums(tm[1,,]) 
  ## nn although would want to not have the colsum for well just final value
  ## replace
  all[1] <- tm[1,"recover.dead","well"]
  
## to be changed into function for i:nt for the rest..
  
  ## can link across the array
  tm[2,"base","well"] <- tm[1,"recover.dead","well"]+
                              (tm[1,"recover.dead","well"]*tm[2,"base","birth"])
  tm[2,"base","afterlife"] <- tm[1,"recover.dead","dead"] 
  
  ### then copy and replace same thing as above like...
  tm[2,"2disease","res"] <- (tm[2,1,"r"]*tm[2,1,"well"]) 
  ##etc...

## create a list and store all the "all" values in it as the outcome of your loop 
# ## e.g. all.list[2] <-  all <- colSums(tm[2,,]) 
#   all.list[2,1] <- tm[2,"recover.dead","well"]

## rbindlist() to turn that into a data.frame with cycles as rows and all your total values 
## then you can use as "m_param" was being used before in terms of multiplying with the reward matrices
  