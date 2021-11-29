# General Model -----------------------------------------------------------

Model <- function(inputs, scenario_income, scenario_prod, scenario_transmission,
                  scenario_farm_effect){
  
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
  
  n.t <- inputs[parameter=="n.t",Value] + 1
  
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
  
  state_names <- c("well", "res","sus","dead", "was_well", "seq") ## the compartments
  transition_names  <- c("birth","r","s","mort_r", "mort_s","mort_w", "rec_r","rec_s", "dead_aft", "r_seq", "s_seq")  ## the transition probabilities
  parameter_names <- c(state_names, transition_names)
  
  state_i <- c(inputs[parameter=="pop",Value], rep(0,length=length(state_names)-1))
  #initial state vector
  
  m_param <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names))
  colnames(m_param) <- parameter_names
  rownames(m_param) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  m_param[ , "r_seq"] <- rep(inputs[parameter=="seq_res", Value], n.t) 
  #chance of developing sequelae following resistant infection
  
  m_param[ , "s_seq"] <- rep(inputs[parameter=="seq_sus", Value], n.t) 
  #chance of developing sequelae following susceptible infection
  
  m_param[ , "r"] <- rep(inputs[parameter=="well_sick", Value]*inputs[parameter=="portion_res",Value], n.t)
  #chance of developing a resistant infection in a year
  
  m_param[ , "s"] <- rep(inputs[parameter=="well_sick", Value]*(1-inputs[parameter=="portion_res",Value]), n.t)
  #chance of developing a susceptible infection in a year
  
  m_param[ , "mort_r"] <- rep(inputs[parameter=="mort_res", Value], n.t)
  #fatality from resistant infection
  
  m_param[ , "mort_s"] <- rep(inputs[parameter=="mort_sus", Value], n.t)
  #fatality from susceptible infection
  
  m_param[ , "rec_r"] <- rep(max(0,(1-(m_param[1,"mort_r"]+m_param[1,"r_seq"]))), n.t)
  #chance of recovering from a resistant infection
  
  m_param[ , "rec_s"] <- rep(max(0,(1-(m_param[1,"mort_s"]+m_param[1,"s_seq"]))), n.t)
  #chance of recovering from a susceptible infection
  
  m_param[ , "mort_w"] <- rep(0, n.t) 
  #chance of dying without infection from 'well', set to zero because background mortality is included in net births
  
  m_param[ , "dead_aft"] <- rep(1, n.t) 
  #all those who die go to the afterlife
  
  m_param[ , "birth"] <- popchange[1:n.t]
  #predicted net births in a given year
  
  
  m_param[1, 1:length(state_names)] <- state_i 
  #adding initial cycle 0 values
  
  #have growing AMR prevalence
  #note that we will later ensure that the total disease incidence does not change,
  #only the portion of infections from resistant bacteria
  amr_growth <- inputs[parameter=="amr_grow", Value]
  
  #set the maximum portion of resistant infections
  max_res <- inputs[parameter == "max_r", Value]
  
  for (i in 2:(n.t)){
    m_param[i, "r"] <- m_param[i-1, "r"]*amr_growth
  }
  
  for(i in 1:n.t){
    if(m_param[i, "r"] > max_res *(m_param[1,"r"]+m_param[1,"s"])){ 
      m_param[i, "r"] <- max_res *(m_param[1,"r"]+m_param[1,"s"]) 
    }
    m_param[i, "s"] <- m_param[1,"r"]+m_param[1,"s"] - m_param[i, "r"] 
  }
  #made it so that, while the incidence of resistant infections increases,
  #the total number of infections doesn't increase. However, if we wanted to 
  #have a changing number of infections over time, we could do that, but would
  #need to replace "m_param[1,"r"]+m_param[1,"s"]" with a parameter called 
  #something like "chance_sick" which can change each period, and have that 
  #grow before allowing the AMR incidence to grow
  
  #make sure that the transition probabilities don't exceed 1
  for(i in 1:n.t){
    m_param[i, "mort_r"] <- m_param[i, "mort_r"] / (m_param[i, "mort_r"] + m_param[i, "rec_r"] + m_param[i, "r_seq"])
    m_param[i, "rec_r"] <- m_param[i, "rec_r"] / (m_param[i, "mort_r"] + m_param[i, "rec_r"] + m_param[i, "r_seq"])
    m_param[i, "r_seq"] <- m_param[i, "r_seq"] / (m_param[i, "mort_r"] + m_param[i, "rec_r"] + m_param[i, "r_seq"])
    
    m_param[i, "mort_s"] <- m_param[i, "mort_s"] / (m_param[i, "mort_s"] + m_param[i, "rec_s"] + m_param[i, "s_seq"])
    m_param[i, "rec_s"] <- m_param[i, "rec_s"] / (m_param[i, "mort_s"] + m_param[i, "rec_s"] + m_param[i, "s_seq"])
    m_param[i, "s_seq"] <- m_param[i, "s_seq"] / (m_param[i, "mort_s"] + m_param[i, "rec_s"] + m_param[i, "s_seq"])
  }
  
  ##set the initial state
  
  #born
  m_param[1, "well"] <- m_param[1, "well"] + m_param[1, "birth"]
  
  #transition out of well
  m_param[1, "was_well"] <- m_param[1, "well"]
  m_param[1, "well"] <- m_param[1, "was_well"] * (1 - m_param[1, "r"] - m_param[1, "s"] - m_param[1, "mort_w"])
  
  m_param[1, "res"] <- m_param[1, "was_well"] * m_param[1, "r"]
  m_param[1, "sus"] <- m_param[1, "was_well"] * m_param[1, "s"]
  m_param[1, "dead"] <- m_param[1, "was_well"] * m_param[1, "mort_w"]
  
  #transition out of sick
  m_param[1, "well"] <- m_param[1, "well"] + 
    (m_param[1, "rec_r"] * m_param[1, "res"]) + 
    (m_param[1, "rec_s"] * m_param[1, "sus"])
  
  m_param[1, "dead"] <- m_param[1, "dead"] +
    (m_param[1, "mort_s"] * m_param[1, "sus"]) + 
    (m_param[1, "mort_r"] * m_param[1, "res"])
  
  m_param[1, "seq"] <- m_param[1, "seq"] +
    (m_param[1, "s_seq"] * m_param[1, "sus"]) + 
    (m_param[1, "r_seq"] * m_param[1, "res"])
  
  ##difference equation
  
  f_human_epi <- function(m_param, n.t){
    
    n.t.val <- n.t
    
    for(i in 2:n.t.val){
      
      #carry over from last period and be born
      m_param[i, "well"] <- m_param[i-1, "well"] + m_param[i, "birth"]
      
      m_param[i, "was_well"] <- m_param[i, "well"]
      
      #transition out of well
      m_param[i, "well"] <- m_param[i, "was_well"] * (1 - m_param[i, "r"] - m_param[i, "s"] - m_param[i, "mort_w"])
      
      m_param[i, "res"] <- m_param[i, "was_well"] * m_param[i, "r"]
      m_param[i, "sus"] <- m_param[i, "was_well"] * m_param[i, "s"]
      m_param[i, "dead"] <- m_param[i, "was_well"] * m_param[i, "mort_w"]
      
      #transition out of sick
      m_param[i, "well"] <- m_param[i, "well"] + 
        (m_param[i, "rec_r"] * m_param[i, "res"]) + 
        (m_param[i, "rec_s"] * m_param[i, "sus"])
      
      m_param[i, "dead"] <- m_param[i, "dead"] +
        (m_param[i, "mort_s"] * m_param[i, "sus"]) + 
        (m_param[i, "mort_r"] * m_param[i, "res"])
      
      m_param[i, "seq"] <- m_param[i, "seq"] +
        (m_param[i, "s_seq"] * m_param[i, "sus"]) + 
        (m_param[i, "r_seq"] * m_param[i, "res"])
      
    }
    
    return(m_param)
  }
  
  m_param <- f_human_epi(m_param,n.t) 
  
  # Healthcare Costs --------------------------------------------------------
  
  dr <- inputs[parameter == "dr", Value]
  
  m_cost <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names))
  colnames(m_cost) <- parameter_names
  rownames(m_cost) <- paste("cycle", 0:(n.t-1), sep  =  "")  
  
  #cost of hospital stay for res and sus infection
  
  c_r <- inputs[parameter == "los_sus", Value] * inputs[parameter == "bed_day_cost", Value] * 365.25
  c_s <- inputs[parameter == "los_res", Value] * inputs[parameter == "bed_day_cost", Value] * 365.25
  
  cost_i <- c(0,c_r,c_s,0,0,0) #only infections incur a healthcare cost here
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_cost[2, 1:length(state_names)] <- cost_i
  
  for (j in 1:length(state_names)) {
    for (i in 3:(n.t)){
      m_cost[i,j] <- f_di(m_cost[i-1,j],dr)
    }  
  }
  
  
  # Healthcare Rewards ------------------------------------------------------
  
  m_rwd <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names))
  colnames(m_rwd) <- parameter_names
  rownames(m_rwd) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #calculate the present value of expected remaining life years for a) a perfectly
  #healthy person and b) someone with sequelae. The negative difference is the 'reward' 
  #of being in the sequelae state 
  
  pv_fut_life <- c(rep(0,n.t-1))
  
  for (i in 1:n.t-1){
    pv_fut_life[i] <- inputs[parameter=="background_qol", Value] * (1-dr)^(i-1)
  }
  pv_life <- sum(pv_fut_life)
  
  pv_fut_life_seq <- c(rep(0,n.t-1))
  for (i in 1:n.t-1){
    pv_fut_life_seq[i] <- inputs[parameter=="qol_seq", Value] * (1-dr)^(i-1)
  }
  pv_life_seq <- sum(pv_fut_life_seq)
  
  #the 'reward' for death is not sweet release, but in fact the negative of the present value of remaining life 
  #years, and the 'reward' for being infected is the loss of welfare while hospitalised
  #therefore in a scenario with more deaths, infections and sequelae will have a negative
  #'reward' of larger absolute value
  
  r_s <- inputs[parameter == "los_sus", Value] *
    (inputs[parameter == "qol_sick", Value] - inputs[parameter == "background_qol", Value])
  
  r_r <- inputs[parameter == "los_res", Value] * 
    (inputs[parameter == "qol_sick", Value] - inputs[parameter == "background_qol", Value])
  
  r_d <- -1 * pv_life #discounted QoL loss from death
  r_seq <- pv_life_seq - pv_life # fixed this because we were previously assigning a benefit to sequelae (the subtraction was the wrong way around lol)
  
  rwd_i <- c(0,r_r,r_s,r_d,0,r_seq) 
  
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd[2, 1:length(state_names)] <- rwd_i
  
  ### accounting for discounting
  for (j in 1:length(state_names)) {
    for (i in 3:(n.t)){
      m_rwd[i,j] <- f_di(m_rwd[i-1,j],dr)
    }  
  }
  
  
  # Productivity Costs -------------------------------------------------------
  
  #for HCA and FCA, we only care about the losses in productivity, so we set the 
  #reward for 'well' to be zero. Going into the 'dead' state incurs a productivity
  #loss equal to the discounted value of forgone future earnings, going into the
  #'res' or 'sus states incurs a loss equal to the earnings that would have
  #been made during the time in hospital. After 1 period, all people in 'dead' go
  #to 'afterlife', which has a reward of zero. 
  #Using the HCA, the forgone future earnings are those of expected remaining economically
  #active years. For FCA, it is the forgone earnings during the time that it takes
  #to find a replacement worker
  
  #all productivity rewards are set to zero, and we only see a difference between
  #the reward matrices of different scenarios
  m_cost_prod <- matrix(rep(0),nrow = n.t, ncol = length(parameter_names))
  colnames(m_cost_prod) <- parameter_names
  rownames(m_cost_prod) <- paste("cycle", 0:(n.t-1), sep = "")
  
  cost_i_prod <- rep(0,(length(state_names)))
  
  ## start at cycle 1 so you do not multiply initial state vector
  m_cost_prod[2,1:length(state_names)] <- cost_i_prod
  
  for (j in 1:length(state_names)) {
    for (i in 3:(n.t)){
      m_cost_prod[i,j] <- f_di(m_cost_prod[i-1,j],dr)
    }
  }
  
  
  # Productivity Rewards ----------------------------------------------------
  
  #the 'reward' for being infected is equal to the negative forgone productivity
  #while hospitalised
  
  m_rwd_prod <- matrix(rep(0), nrow = n.t, ncol = length(parameter_names))
  colnames(m_rwd_prod) <- parameter_names
  rownames(m_rwd_prod) <- paste("cycle", 0:(n.t-1), sep = "")
  
  r_r_prod <- -1 * inputs[parameter == "los_res", Value] * 
    ((inputs[parameter == "prod_pc", Value]*inputs[parameter == "lfpr", Value])* 
       inputs[parameter == "unpaid_prod_pc", Value])
  
  r_s_prod <- -1 * inputs[parameter == "los_sus", Value] *
    ((inputs[parameter == "prod_pc", Value]*inputs[parameter == "lfpr", Value])*
       inputs[parameter == "unpaid_prod_pc", Value])
  
  r_w_prod <- 0
  
  r_aft_prod <- 0
  
  r_seq_prod <- 0 #importantly assumes that people with sequelae are equally productive
  
  #the reward for dead is the present discounted value of future work (either for the
  #remainder of economically active life, or for the 6 months needed to find a replacement)
  
  remaining_work_years <- inputs[parameter == "remaining_work_years", Value]
  
  yearly_prod <- inputs[parameter == "prod_pc", Value]*inputs[parameter == "lfpr", Value]*
    inputs[parameter == "unpaid_prod_pc", Value]
  
  pv_fut_prod <- c(rep(0,remaining_work_years))
  
  dr_pgrowth <- dr - inputs[parameter=="prod_growth", Value] 
  ##discount rate net of productivity growth (in theory it can be negative)
  
  for (i in 1:remaining_work_years){ 
    pv_fut_prod[i] <- yearly_prod * (1-dr_pgrowth)^(i-1)
  }
  
  pv_life_prod <- sum(pv_fut_prod)
  
  if(scenario_prod == "HCA"){
    r_d_prod <- -1 * pv_life_prod
  } else if(scenario_prod == "FCA"){
    r_d_prod <- -0.5 * yearly_prod
  } else{
    paste("ERROR: PLEASE CHOOSE AN APPROACH TO ESTIMATING PRODUCTIVITY OUTCOMES")
  }
  
  rwd_i_prod <- c(r_w_prod, r_r_prod, r_s_prod, r_d_prod, r_aft_prod, r_seq_prod)
  
  ## start at cycle 1 so you do not multiply initial state vector
  m_rwd_prod[2,1:length(state_names)] <- rwd_i_prod 
  
  ### discount, but also account for labour productivity growth
  
  for (j in 1:length(state_names)) {
    for (i in 3:(n.t)){
      m_rwd_prod[i,j] <- f_di(m_rwd_prod[i-1,j],dr_pgrowth)
    }
  }
  
  # Animal Epi Model --------------------------------------------------------
  
  state_names_a <- c("well", "res","sus","fallen","sold") ## the compartments
  transition_names_a  <- c("birth","r","s","mort_r", "mort_s","mort_w", "rec_r","rec_s","w_sold")  ## the rates
  parameter_names_a <- c(state_names_a, transition_names_a)
  
  f_animal_epi <- function(m_param_a_base, n.t, scenario_animal){
    
    n.t.val <- n.t
    
    if(scenario_animal == "chicken_small"){
      n_animals_farm <- inputs[parameter == "n_chickens_farm_small", Value]
      annual_cycles <- inputs[parameter == "pcycles_chicken_small", Value]
    } else if (scenario_animal == "chicken_ind"){
      n_animals_farm <- inputs[parameter == "n_chickens_farm_ind", Value]
      annual_cycles <- inputs[parameter == "pcycles_chicken_ind", Value]
    } else if (scenario_animal == "pig_small"){
      n_animals_farm <- inputs[parameter == "n_pigs_farm_small", Value]
      annual_cycles <- inputs[parameter == "pcycles_pig_small", Value]
    } else if (scenario_animal == "pig_ind"){
      n_animals_farm <- inputs[parameter == "n_pigs_farm_ind", Value]
      annual_cycles <- inputs[parameter == "pcycles_pig_ind", Value]  
    }    
    
    m_param_a_temp <- m_param_a_base[1:4,]
    rownames(m_param_a_temp) <- NULL ##removing rownames
    
    i <- 2 ## getting sick
    m_param_a_temp[i,"well"] <- m_param_a_temp[i-1,"well"] -(m_param_a_temp[i-1,"r"]*m_param_a_temp[i-1,"well"]) -
      (m_param_a_temp[i-1,"s"]*m_param_a_temp[i-1,"well"])
    m_param_a_temp[i,"res"] <- m_param_a_temp[i-1,"res"] + (m_param_a_temp[i-1,"r"]*m_param_a_temp[i-1,"well"]) 
    m_param_a_temp[i,"sus"] <- m_param_a_temp[i-1,"sus"] + (m_param_a_temp[i-1,"s"]*m_param_a_temp[i-1,"well"])
    
    i <- 3 ## dying and recovering
    m_param_a_temp[i,"fallen"] <- (m_param_a_temp[i-1,"mort_w"]*m_param_a_temp[i-1,"well"]) +
      (m_param_a_temp[i-1,"mort_r"]*m_param_a_temp[i-1,"res"]) + 
      (m_param_a_temp[i-1,"mort_s"]*m_param_a_temp[i-1,"sus"])
    m_param_a_temp[i, "well"] <- m_param_a_temp[i-1,"well"] - (m_param_a_temp[i-1,"well"]*m_param_a_temp[i-1,"mort_w"]) +
      (m_param_a_temp[i-1,"rec_r"]*m_param_a_temp[i-1,"res"])+ 
      (m_param_a_temp[i-1,"rec_s"]*m_param_a_temp[i-1,"sus"])
    
    i <- 4 ##sold
    m_param_a_temp[i,"sold"] <- (m_param_a_temp[i-1,"w_sold"]*m_param_a_temp[i-1,"well"])
    
    ## final states
    m_a_sum <- m_param_a_temp[4,]
    m_a_sum["res"] <- m_param_a_temp[2,"res"]
    m_a_sum["sus"] <- m_param_a_temp[2,"sus"]
    m_a_sum["well"] <- n_animals_farm - m_a_sum["res"] - m_a_sum["sus"] ## reset the number in 'well'
    
    m_a_sum[1:5] <- annual_cycles * m_a_sum[1:5] #multiply by the number of annual cycles
    
    m_param_a <- matrix(rep(m_a_sum), nrow=n.t.val, ncol =length(parameter_names_a))
    m_param_a <- t(replicate(n.t.val,m_a_sum))
    colnames(m_param_a) <- parameter_names_a
    rownames(m_param_a) <- paste("cycle", 0:(n.t-1), sep  =  "")
    
    m_param_a
    
    return(m_param_a)
    
  }
  
  # Chicken Epi Module - Smallholder -------------------------------------------------------
  
  scenario_animal <- "chicken_small"
  
  state_i_c_s <- c(inputs[parameter == "n_chickens_farm_small", Value], rep(0,length=length(state_names_a)-1))
  
  m_param_c_s_base <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_param_c_s_base) <- parameter_names_a
  rownames(m_param_c_s_base) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #here, we have a set mortality rate for all states - 
  #it only matters if animals are in 'res' or 'sus' for the purpose of calculating 
  #the cost of therapeutic treatment
  
  m_param_c_s_base[ , "r"] <- rep(inputs[parameter=="c_res_small", Value], n.t)
  m_param_c_s_base[ , "s"] <- rep(inputs[parameter=="c_sus_small", Value], n.t)
  m_param_c_s_base[ , "mort_s"] <- rep(inputs[parameter=="c_mort_small", Value], n.t) 
  m_param_c_s_base[ , "mort_r"] <- rep(inputs[parameter=="c_mort_small", Value], n.t)
  m_param_c_s_base[ , "rec_r"] <- rep(1-(m_param_c_s_base[1,"mort_r"]), n.t)
  m_param_c_s_base[ , "rec_s"] <- rep(1-(m_param_c_s_base[1,"mort_s"]), n.t)
  m_param_c_s_base[ , "birth"] <- rep(1, n.t)
  m_param_c_s_base[ , "mort_w"] <- rep(inputs[parameter=="c_mort_small", Value], n.t)
  m_param_c_s_base[ , "w_sold"] <- rep(1, n.t) 
  
  #make it so that the total incidence of infections stays the same, and only the
  #portion of them that are resistant changes
  for(i in 1:n.t){
    if(m_param_c_s_base[i, "r"] > max_res * (m_param_c_s_base[1, "r"] + m_param_c_s_base[1, "s"])){
      m_param_c_s_base[i, "r"] <- max_res * (m_param_c_s_base[1, "r"] + m_param_c_s_base[1, "s"])
    }
    m_param_c_s_base[i, "s"] <- m_param_c_s_base[1, "r"] + m_param_c_s_base[1, "s"] - m_param_c_s_base[i, "r"]
  }
  
  m_param_c_s_base[1, 1:length(state_names_a)] <- state_i_c_s
  
  m_param_c_s <- f_animal_epi(m_param_c_s_base,n.t, scenario_animal)
  ### ignore totals of transition probs etc. as they are over counted etc.
  ## just want to focus on health state totals
  
  
  # Chicken Epi Module - Industrial -----------------------------------------
  
  scenario_animal <- "chicken_ind"
  
  state_i_c_i <- c(inputs[parameter == "n_chickens_farm_ind", Value], rep(0,length=length(state_names_a)-1))
  
  m_param_c_i_base <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_param_c_i_base) <- parameter_names_a
  rownames(m_param_c_i_base) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #here, we have a set mortality rate for all states - 
  #it only matters if animals are in 'res' or 'sus' for the purpose of calculating 
  #the cost of therapeutic treatment
  
  m_param_c_i_base[ , "r"] <- rep(inputs[parameter=="c_res_ind", Value], n.t)
  m_param_c_i_base[ , "s"] <- rep(inputs[parameter=="c_sus_ind", Value], n.t)
  m_param_c_i_base[ , "mort_s"] <- rep(inputs[parameter=="c_mort_ind", Value], n.t) 
  m_param_c_i_base[ , "mort_r"] <- rep(inputs[parameter=="c_mort_ind", Value], n.t)
  m_param_c_i_base[ , "rec_r"] <- rep(1-(m_param_c_i_base[1,"mort_r"]), n.t)
  m_param_c_i_base[ , "rec_s"] <- rep(1-(m_param_c_i_base[1,"mort_s"]), n.t)
  m_param_c_i_base[ , "birth"] <- rep(1, n.t)
  m_param_c_i_base[ , "mort_w"] <- rep(inputs[parameter=="c_mort_ind", Value], n.t)
  m_param_c_i_base[ , "w_sold"] <- rep(1, n.t) 
  
  #make it so that the total incidence of infections stays the same, and only the
  #portion of them that are resistant changes
  for(i in 1:n.t){
    if(m_param_c_i_base[i, "r"] > max_res * (m_param_c_i_base[1, "r"] + m_param_c_i_base[1, "s"])){
      m_param_c_i_base[i, "r"] <- max_res * (m_param_c_i_base[1, "r"] + m_param_c_i_base[1, "s"])
    }
    m_param_c_i_base[i, "s"] <- m_param_c_i_base[1, "r"] + m_param_c_i_base[1, "s"] - m_param_c_i_base[i, "r"]
  }
  
  m_param_c_i_base[1, 1:length(state_names_a)] <- state_i_c_i
  
  m_param_c_i <- f_animal_epi(m_param_c_i_base,n.t, scenario_animal)
  ### ignore totals of transition probs etc. as they are over counted etc.
  ## just want to focus on health state totals
  
  
  # Pig Epi Module - Smallholder --------------------------------------------
  
  scenario_animal <- "pig_small"
  
  state_i_p_s <- c(inputs[parameter == "n_pigs_farm_small", Value], rep(0,length=length(state_names_a)-1))
  
  m_param_p_s_base <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_param_p_s_base) <- parameter_names_a
  rownames(m_param_p_s_base) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #here, we have a set mortality rate for all states - 
  #it only matters if animals are in 'res' or 'sus' for the purpose of calculating 
  #the cost of therapeutic treatment
  
  m_param_p_s_base[ , "r"] <- rep(inputs[parameter=="p_res_small", Value], n.t)
  m_param_p_s_base[ , "s"] <- rep(inputs[parameter=="p_sus_small", Value], n.t)
  m_param_p_s_base[ , "mort_s"] <- rep(inputs[parameter=="p_mort_small", Value], n.t) 
  m_param_p_s_base[ , "mort_r"] <- rep(inputs[parameter=="p_mort_small", Value], n.t)
  m_param_p_s_base[ , "rec_r"] <- rep(1-(m_param_p_s_base[1,"mort_r"]), n.t)
  m_param_p_s_base[ , "rec_s"] <- rep(1-(m_param_p_s_base[1,"mort_s"]), n.t)
  m_param_p_s_base[ , "birth"] <- rep(1, n.t)
  m_param_p_s_base[ , "mort_w"] <- rep(inputs[parameter=="p_mort_small", Value], n.t)
  m_param_p_s_base[ , "w_sold"] <- rep(1, n.t) 
  
  #make it so that the total incidence of infections stays the same, and only the
  #portion of them that are resistant changes
  for(i in 1:n.t){
    if(m_param_p_s_base[i, "r"] > max_res * (m_param_p_s_base[1, "r"] + m_param_p_s_base[1, "s"])){
      m_param_p_s_base[i, "r"] <- max_res * (m_param_p_s_base[1, "r"] + m_param_p_s_base[1, "s"])
    }
    m_param_p_s_base[i, "s"] <- m_param_p_s_base[1, "r"] + m_param_p_s_base[1, "s"] - m_param_p_s_base[i, "r"]
  }
  
  m_param_p_s_base[1, 1:length(state_names_a)] <- state_i_p_s
  
  m_param_p_s <- f_animal_epi(m_param_p_s_base,n.t, scenario_animal)
  ### ignore totals of transition probs etc. as they are over counted etc.
  ## just want to focus on health state totals
  
  
  # Pig Epi Module - Industrial ---------------------------------------------
  
  scenario_animal <- "pig_ind"
  
  state_i_p_i <- c(inputs[parameter == "n_pigs_farm_ind", Value], rep(0,length=length(state_names_a)-1))
  
  m_param_p_i_base <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_param_p_i_base) <- parameter_names_a
  rownames(m_param_p_i_base) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #here, we have a set mortality rate for all states - 
  #it only matters if animals are in 'res' or 'sus' for the purpose of calculating 
  #the cost of therapeutic treatment
  
  m_param_p_i_base[ , "r"] <- rep(inputs[parameter=="p_res_ind", Value], n.t)
  m_param_p_i_base[ , "s"] <- rep(inputs[parameter=="p_sus_ind", Value], n.t)
  m_param_p_i_base[ , "mort_s"] <- rep(inputs[parameter=="p_mort_ind", Value], n.t) 
  m_param_p_i_base[ , "mort_r"] <- rep(inputs[parameter=="p_mort_ind", Value], n.t)
  m_param_p_i_base[ , "rec_r"] <- rep(1-(m_param_p_i_base[1,"mort_r"]), n.t)
  m_param_p_i_base[ , "rec_s"] <- rep(1-(m_param_p_i_base[1,"mort_s"]), n.t)
  m_param_p_i_base[ , "birth"] <- rep(1, n.t)
  m_param_p_i_base[ , "mort_w"] <- rep(inputs[parameter=="p_mort_ind", Value], n.t)
  m_param_p_i_base[ , "w_sold"] <- rep(1, n.t) 
  
  #make it so that the total incidence of infections stays the same, and only the
  #portion of them that are resistant changes
  for(i in 1:n.t){
    if(m_param_p_i_base[i, "r"] > max_res * (m_param_p_i_base[1, "r"] + m_param_p_i_base[1, "s"])){
      m_param_p_i_base[i, "r"] <- max_res * (m_param_p_i_base[1, "r"] + m_param_p_i_base[1, "s"])
    }
    m_param_p_i_base[i, "s"] <- m_param_p_i_base[1, "r"] + m_param_p_i_base[1, "s"] - m_param_p_i_base[i, "r"]
  }
  
  m_param_p_i_base[1, 1:length(state_names_a)] <- state_i_p_i
  
  m_param_p_i <- f_animal_epi(m_param_p_i_base,n.t, scenario_animal)
  ### ignore totals of transition probs etc. as they are over counted etc.
  ## just want to focus on health state totals  
  
  
  #'All of the farm costs are zero here, as any changes to profits per animal
  #'from the intervention will be reflected in changes to the 'farm rewards' 
  
  # Chicken Farm Costs - Smallholder ----------------------------------------
  
  m_cost_c_s <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_cost_c_s) <- parameter_names_a
  rownames(m_cost_c_s) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  # Chicken Farm Costs - Industrial -----------------------------------------
  
  m_cost_c_i <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_cost_c_i) <- parameter_names_a
  rownames(m_cost_c_i) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  # Pig Farm Costs - Smallholder --------------------------------------------
  
  m_cost_p_s <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_cost_p_s) <- parameter_names_a
  rownames(m_cost_p_s) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  # Pig Farm Costs - Industrial ---------------------------------------------
  
  m_cost_p_i <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_cost_p_i) <- parameter_names_a
  rownames(m_cost_p_i) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  
  # Chicken Farm Rewards - Smallholder --------------------------------------
  
  m_rwd_c_s <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_rwd_c_s) <- parameter_names_a
  rownames(m_rwd_c_s) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #only get a reward for selling animals
  r_sold_c_s <- inputs[parameter == "chicken_weight", Value] * inputs[parameter == "chicken_price", Value]
  rwd_i_c_s <- c(0,0,0,0,r_sold_c_s)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_c_s[2, 1:length(state_names_a)] <- rwd_i_c_s
  
  #discount
  for (j in 1:length(state_names_a)) {
    for (i in 3:(n.t)){
      m_rwd_c_s[i,j] <- f_di(m_rwd_c_s[i-1,j],dr)
    }  
  } 
  
  # Chicken Farm Rewards - Industrial ---------------------------------------
  
  m_rwd_c_i <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_rwd_c_i) <- parameter_names_a
  rownames(m_rwd_c_i) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #only get a reward for selling animals
  r_sold_c_i <- inputs[parameter == "chicken_weight", Value] * inputs[parameter == "chicken_price", Value]
  rwd_i_c_i <- c(0,0,0,0,r_sold_c_i)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_c_i[2, 1:length(state_names_a)] <- rwd_i_c_i
  
  #discount
  for (j in 1:length(state_names_a)) {
    for (i in 3:(n.t)){
      m_rwd_c_i[i,j] <- f_di(m_rwd_c_i[i-1,j],dr)
    }  
  } 
  
  # Pig Farm Rewards - Smallholder --------------------------------------
  
  m_rwd_p_s <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_rwd_p_s) <- parameter_names_a
  rownames(m_rwd_p_s) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #only get a reward for selling animals
  r_sold_p_s <- inputs[parameter == "pig_weight", Value] * inputs[parameter == "pig_price", Value]
  rwd_i_p_s <- c(0,0,0,0,r_sold_p_s)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_p_s[2, 1:length(state_names_a)] <- rwd_i_p_s
  
  #discount
  for (j in 1:length(state_names_a)) {
    for (i in 3:(n.t)){
      m_rwd_p_s[i,j] <- f_di(m_rwd_p_s[i-1,j],dr)
    }  
  } 
  
  # Pig Farm Rewards - Industrial ---------------------------------------
  
  m_rwd_p_i <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_rwd_p_i) <- parameter_names_a
  rownames(m_rwd_p_i) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #only get a reward for selling animals
  r_sold_p_i <- inputs[parameter == "pig_weight", Value] * inputs[parameter == "pig_price", Value]
  rwd_i_p_i <- c(0,0,0,0,r_sold_p_i)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_p_i[2, 1:length(state_names_a)] <- rwd_i_p_i
  
  #discount
  for (j in 1:length(state_names_a)) {
    for (i in 3:(n.t)){
      m_rwd_p_i[i,j] <- f_di(m_rwd_p_i[i-1,j],dr)
    }  
  }  
  
  
  # The Intervention --------------------------------------------------------
  
  pig_income_effect     <- as.numeric()
  chicken_income_effect <- as.numeric()
  
  if(scenario_farm_effect == "min"){
    pig_income_effect     <- inputs[parameter == "pig_income_effect", Min]
    chicken_income_effect <- inputs[parameter == "chicken_income_effect", Min]
  } else if (scenario_farm_effect == "lo"){
    pig_income_effect     <- inputs[parameter == "pig_income_effect", Lo]
    chicken_income_effect <- inputs[parameter == "chicken_income_effect", Lo]
  } else if (scenario_farm_effect == "med"){
    pig_income_effect     <- inputs[parameter == "pig_income_effect", Med]
    chicken_income_effect <- inputs[parameter == "chicken_income_effect", Med]
  } else if (scenario_farm_effect == "hi"){
    pig_income_effect     <- inputs[parameter == "pig_income_effect", Hi]
    chicken_income_effect <- inputs[parameter == "chicken_income_effect", Hi]
  } else if (scenario_farm_effect == "max"){
    pig_income_effect     <- inputs[parameter == "pig_income_effect", Max]
    chicken_income_effect <- inputs[parameter == "chicken_income_effect", Max]
  }  
  
  ##Intervention - Humans
  
  #reduction in incidence of drug resistant infections in humans
  m_param2 <- m_param ## human parameter matrix for the intervention scenario
  
  #reduce the chance of getting a resistant infection in humans, depending on the link parameter used
  
  for(i in 1:n.t){ ## fixed because previously the intervention was erasing the background growth in human AMR 
    if(scenario_transmission == "low"){
      m_param2[i , "r"] <- m_param2[i , "r"]*(1+inputs[parameter=="res_change", Lo])
    } else if (scenario_transmission == "med"){
      m_param2[i , "r"] <- m_param2[i , "r"]*(1+inputs[parameter=="res_change", Med])
    } else if (scenario_transmission == "hi"){
      m_param2[i , "r"] <- m_param2[i , "r"]*(1+inputs[parameter=="res_change", Hi])
    } else if (scenario_transmission == "max"){
      m_param2[i , "r"] <- m_param2[i , "r"]*(1+inputs[parameter=="res_change", Max])
    } 
  }
  
  #make sure that the total number of infections remains constant
  for(i in 1:n.t){
    if(m_param2[i, "r"] > max_res * (m_param2[1,"r"]+m_param2[1,"s"])){ 
      m_param2[i, "r"] <- max_res * (m_param2[1,"r"]+m_param2[1,"s"]) 
    }
    m_param2[i, "s"] <- m_param2[1,"r"]+m_param2[1,"s"] - m_param2[i, "r"]
  }
  
  ## clear state values
  m_param2[ , 1:length(state_names)] <- 0
  m_param2[1, 1:length(state_names)] <- state_i
  
  ##set the initial state
  
  #born
  m_param2[1, "well"] <- m_param2[1, "well"] + m_param2[1, "birth"]
  
  #transition out of well
  m_param2[1, "was_well"] <- m_param2[1, "well"]
  m_param2[1, "well"] <- m_param2[1, "was_well"] * (1 - m_param2[1, "r"] - m_param2[1, "s"] - m_param2[1, "mort_w"])
  
  m_param2[1, "res"] <- m_param2[1, "was_well"] * m_param2[1, "r"]
  m_param2[1, "sus"] <- m_param2[1, "was_well"] * m_param2[1, "s"]
  m_param2[1, "dead"] <- m_param2[1, "was_well"] * m_param2[1, "mort_w"]
  
  #transition out of sick
  m_param2[1, "well"] <- m_param2[1, "well"] + 
    (m_param2[1, "rec_r"] * m_param2[1, "res"]) + 
    (m_param2[1, "rec_s"] * m_param2[1, "sus"])
  
  m_param2[1, "dead"] <- m_param2[1, "dead"] +
    (m_param2[1, "mort_s"] * m_param2[1, "sus"]) + 
    (m_param2[1, "mort_r"] * m_param2[1, "res"])
  
  m_param2[1, "seq"] <- m_param2[1, "seq"] +
    (m_param2[1, "s_seq"] * m_param2[1, "sus"]) + 
    (m_param2[1, "r_seq"] * m_param2[1, "res"])
  
  #apply the human epi function to the intervention case
  m_param2 <- f_human_epi(m_param2, n.t) 
  
  ##Intervention - Chickens - Industrial
  
  #create a parameter spreadsheet for the intervention case
  m_param_c_i2 <- m_param_c_i 
  
  #change in mortality
  
  m_param_c_i2[ , "mort_w"] <- m_param_c_i2[ , "mort_w"] * (1+inputs[parameter == "chicken_mort_effect", Value])
  
  m_param_c_i2[ , "mort_s"] <- m_param_c_i2[ , "mort_s"] * (1+inputs[parameter == "chicken_mort_effect", Value])
  
  m_param_c_i2[ , "mort_r"] <- m_param_c_i2[ , "mort_r"] * (1+inputs[parameter == "chicken_mort_effect", Value])
  
  #make sure the total number of infections stays constant
  for(i in 1:n.t){
    m_param_c_i2[i, "s"] <- m_param_c_i2[1, "r"] + m_param_c_i2[1, "s"] - m_param_c_i2[i, "r"]
  }
  
  m_param_c_i2[ , 1:length(state_names_a)] <- 0
  m_param_c_i2[1, 1:length(state_names_a)] <- state_i_c_i
  
  #apply the animal epi function
  scenario_animal <- "chicken_ind"
  m_param_c_i2 <- f_animal_epi(m_param_c_i2, n.t, scenario_animal)
  
  #rewards
  m_rwd_c_i2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_rwd_c_i2) <- parameter_names_a
  rownames(m_rwd_c_i2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  r_sold_c_i2 <- r_sold_c_i * (1+chicken_income_effect)
  rwd_i_c_i2 <- c(0,0,0,0,r_sold_c_i2)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_c_i2[2, 1:length(state_names_a)] <- rwd_i_c_i2
  
  #discount
  for (j in 1:length(state_names_a)) {
    for (i in 3:(n.t)){
      m_rwd_c_i2[i,j] <- f_di(m_rwd_c_i2[i-1,j],dr)
    }  
  }
  
  #costs
  m_cost_c_i2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_cost_c_i2) <- parameter_names_a
  rownames(m_cost_c_i2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  ##Intervention - Chickens - Smallholder
  
  #create a parameter spreadsheet for the intervention case
  m_param_c_s2 <- m_param_c_s 
  
  #change in mortality
  
  m_param_c_s2[ , "mort_w"] <- m_param_c_s2[ , "mort_w"] * (1+inputs[parameter == "chicken_mort_effect", Value])
  
  m_param_c_s2[ , "mort_s"] <- m_param_c_s2[ , "mort_s"] * (1+inputs[parameter == "chicken_mort_effect", Value])
  
  m_param_c_s2[ , "mort_r"] <- m_param_c_s2[ , "mort_r"] * (1+inputs[parameter == "chicken_mort_effect", Value])
  
  #make sure the total number of infections stays constant
  for(i in 1:n.t){
    m_param_c_s2[i, "s"] <- m_param_c_s2[1, "r"] + m_param_c_s2[1, "s"] - m_param_c_s2[i, "r"]
  }
  
  m_param_c_s2[ , 1:length(state_names_a)] <- 0
  m_param_c_s2[1, 1:length(state_names_a)] <- state_i_c_s
  
  #apply the animal epi function
  scenario_animal <- "chicken_small"
  m_param_c_s2 <- f_animal_epi(m_param_c_s2, n.t, scenario_animal)
  
  #rewards
  m_rwd_c_s2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_rwd_c_s2) <- parameter_names_a
  rownames(m_rwd_c_s2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  r_sold_c_s2 <- r_sold_c_s * (1+chicken_income_effect)
  rwd_i_c_s2 <- c(0,0,0,0,r_sold_c_s2)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_c_s2[2, 1:length(state_names_a)] <- rwd_i_c_s2
  
  #discount
  for (j in 1:length(state_names_a)) {
    for (i in 3:(n.t)){
      m_rwd_c_s2[i,j] <- f_di(m_rwd_c_s2[i-1,j],dr)
    }  
  }
  
  #costs
  m_cost_c_s2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_cost_c_s2) <- parameter_names_a
  rownames(m_cost_c_s2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  ##Intervention - Pigs - Industrial
  
  #create a parameter spreadsheet for the intervention case
  m_param_p_i2 <- m_param_p_i 
  
  #change in mortality
  
  m_param_p_i2[ , "mort_w"] <- m_param_p_i2[ , "mort_w"] * (1+inputs[parameter == "pig_mort_effect", Value])
  
  m_param_p_i2[ , "mort_s"] <- m_param_p_i2[ , "mort_s"] * (1+inputs[parameter == "pig_mort_effect", Value])
  
  m_param_p_i2[ , "mort_r"] <- m_param_p_i2[ , "mort_r"] * (1+inputs[parameter == "pig_mort_effect", Value])
  
  #make sure the total number of infections stays constant
  for(i in 1:n.t){
    m_param_p_i2[i, "s"] <- m_param_p_i2[1, "r"] + m_param_p_i2[1, "s"] - m_param_p_i2[i, "r"]
  }
  
  m_param_p_i2[ , 1:length(state_names_a)] <- 0
  m_param_p_i2[1, 1:length(state_names_a)] <- state_i_p_i
  
  #apply the animal epi function
  scenario_animal <- "pig_ind"
  m_param_p_i2 <- f_animal_epi(m_param_p_i2, n.t, scenario_animal)  
  
  #rewards
  m_rwd_p_i2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_rwd_p_i2) <- parameter_names_a
  rownames(m_rwd_p_i2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  r_sold_p_i2 <- r_sold_p_i * (1+pig_income_effect)
  rwd_i_p_i2 <- c(0,0,0,0,r_sold_p_i2)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_p_i2[2, 1:length(state_names_a)] <- rwd_i_p_i2
  
  #discount
  for (j in 1:length(state_names_a)) {
    for (i in 3:(n.t)){
      m_rwd_p_i2[i,j] <- f_di(m_rwd_p_i2[i-1,j],dr)
    }  
  }
  
  #costs
  m_cost_p_i2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_cost_p_i2) <- parameter_names_a
  rownames(m_cost_p_i2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  ##Intervention - Pigs - Smallholder
  
  #create a parameter spreadsheet for the intervention case
  m_param_p_s2 <- m_param_p_s 
  
  #change in mortality
  
  m_param_p_s2[ , "mort_w"] <- m_param_p_s2[ , "mort_w"] * (1+inputs[parameter == "pig_mort_effect", Value])
  
  m_param_p_s2[ , "mort_s"] <- m_param_p_s2[ , "mort_s"] * (1+inputs[parameter == "pig_mort_effect", Value])
  
  m_param_p_s2[ , "mort_r"] <- m_param_p_s2[ , "mort_r"] * (1+inputs[parameter == "pig_mort_effect", Value])
  
  #make sure the total number of infections stays constant
  for(i in 1:n.t){
    m_param_p_s2[i, "s"] <- m_param_p_s2[1, "r"] + m_param_p_s2[1, "s"] - m_param_p_s2[i, "r"]
  }
  
  m_param_p_s2[ , 1:length(state_names_a)] <- 0
  m_param_p_s2[1, 1:length(state_names_a)] <- state_i_p_s
  
  #apply the animal epi function
  scenario_animal <- "pig_small"
  m_param_p_s2 <- f_animal_epi(m_param_p_s2, n.t, scenario_animal)
  
  #rewards
  m_rwd_p_s2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_rwd_p_s2) <- parameter_names_a
  rownames(m_rwd_p_s2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  r_sold_p_s2 <- r_sold_p_s * (1+pig_income_effect)
  rwd_i_p_s2 <- c(0,0,0,0,r_sold_p_s2)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_p_s2[2, 1:length(state_names_a)] <- rwd_i_p_s2
  
  #discount
  for (j in 1:length(state_names_a)) {
    for (i in 3:(n.t)){
      m_rwd_p_s2[i,j] <- f_di(m_rwd_p_s2[i-1,j],dr)
    }  
  }
  
  #costs
  m_cost_p_s2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_cost_p_s2) <- parameter_names_a
  rownames(m_cost_p_s2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  
  # Results and Outputs -----------------------------------------------------
  
  #Get number of each type of farm nationally
  n_farms_chicken_small <- inputs[parameter == "n_chickens", Value] * 
    (1 - inputs[parameter == "portion_animals_ind", Value]) / 
    inputs[parameter == "n_chickens_farm_small", Value]
  
  n_farms_chicken_ind <- inputs[parameter == "n_chickens", Value] * 
    (inputs[parameter == "portion_animals_ind", Value]) / 
    inputs[parameter == "n_chickens_farm_ind", Value]
  
  n_farms_pig_small <- inputs[parameter == "n_pigs", Value] * 
    (1 - inputs[parameter == "portion_animals_ind", Value]) / 
    inputs[parameter == "n_pigs_farm_small", Value]
  
  n_farms_pig_ind <- inputs[parameter == "n_pigs", Value] * 
    (inputs[parameter == "portion_animals_ind", Value]) / 
    inputs[parameter == "n_pigs_farm_ind", Value]
  
  #results matrix for healthcare
  results_base_h <- f_expvalue(m_param,m_cost,m_rwd)
  results_interv_h <- f_expvalue(m_param2,m_cost,m_rwd)
  
  total_results_HC<- matrix(rep(0), nrow=2, ncol=2)
  colnames(total_results_HC) <- c("Costs ($)", "QALYs")
  rownames(total_results_HC) <- c("Base Case", "Intervention")
  
  total_results_HC[1,] <- results_base_h[1,]
  total_results_HC[2,] <- results_interv_h[1,]
  
  wtp <- inputs[parameter == "wtp", Value]
  
  incr_cost_health <- (results_interv_h[1,1] - results_base_h[1,1])
  QALYs_saved <-  (results_interv_h[1,2]-results_base_h[1,2])
  NMB_health <- (QALYs_saved*wtp)-(incr_cost_health)
  
  #results matrix for productivity
  results_base_prod <- f_expvalue(m_param,m_cost_prod,m_rwd_prod)
  results_interv_prod <- f_expvalue(m_param2,m_cost_prod,m_rwd_prod)
  
  total_results_prod <- matrix(rep(0), nrow=2, ncol=2)
  colnames(total_results_prod) <- c("Productivity", "QALYs")
  rownames(total_results_prod) <- c("Base Case", "Intervention")
  
  total_results_prod[1,2] <- results_base_h[,2]
  total_results_prod[2,2] <- results_interv_h[,2]
  total_results_prod[1,1] <- results_base_prod[,2]   #will be negative
  total_results_prod[2,1] <- results_interv_prod[,2] #will be negative but hopefully closer to zero
  
  incr_cost_prod <- total_results_prod[1,1] - total_results_prod[2,1] #hopefully negative
  incr_benefit_prod <- total_results_prod[2,2] - total_results_prod[1,2] #hopefully positive
  NMB_prod <- total_results_prod[2,1] - total_results_prod[1,1] #hopefully positive
  
  #results matrix for industrial chicken farms
  results_base_c_i <- f_expvalue(m_param_c_i,m_cost_c_i,m_rwd_c_i)
  results_interv_c_i <- f_expvalue(m_param_c_i2,m_cost_c_i2,m_rwd_c_i2)
  
  total_results_c_i<- matrix(rep(0), nrow=2, ncol=2)
  colnames(total_results_c_i) <- c("Costs ($)", "Benefits ($)")
  rownames(total_results_c_i) <- c("Base Case", "Intervention")
  
  incr_cost_c_i <- (results_interv_c_i[1,1] - results_base_c_i[1,1])
  incr_benefit_c_i <-  (results_interv_c_i[1,2]-results_base_c_i[1,2])
  
  total_results_c_i[1,] <- results_base_c_i[1,] 
  total_results_c_i[2,] <- results_interv_c_i[1,] 
  
  NMB_c_i <- (incr_benefit_c_i-incr_cost_c_i) * n_farms_chicken_ind
  
  #results matrix for smallholder chicken farms
  results_base_c_s <- f_expvalue(m_param_c_s,m_cost_c_s,m_rwd_c_s)
  results_interv_c_s <- f_expvalue(m_param_c_s2,m_cost_c_s2,m_rwd_c_s2)
  
  total_results_c_s<- matrix(rep(0), nrow=2, ncol=2)
  colnames(total_results_c_s) <- c("Costs ($)", "Benefits ($)")
  rownames(total_results_c_s) <- c("Base Case", "Intervention")
  
  incr_cost_c_s <- (results_interv_c_s[1,1] - results_base_c_s[1,1])
  incr_benefit_c_s <-  (results_interv_c_s[1,2]-results_base_c_s[1,2])
  
  total_results_c_s[1,] <- results_base_c_s[1,] 
  total_results_c_s[2,] <- results_interv_c_s[1,] 
  
  NMB_c_s <- (incr_benefit_c_s-incr_cost_c_s) * n_farms_chicken_small
  
  #results matrix for industrial pig farms
  results_base_p_i <- f_expvalue(m_param_p_i,m_cost_p_i,m_rwd_p_i)
  results_interv_p_i <- f_expvalue(m_param_p_i2,m_cost_p_i2,m_rwd_p_i2)
  
  total_results_p_i<- matrix(rep(0), nrow=2, ncol=2)
  colnames(total_results_p_i) <- c("Costs ($)", "Benefits ($)")
  rownames(total_results_p_i) <- c("Base Case", "Intervention")
  
  incr_cost_p_i <- (results_interv_p_i[1,1] - results_base_p_i[1,1])
  incr_benefit_p_i <-  (results_interv_p_i[1,2]-results_base_p_i[1,2])
  
  total_results_p_i[1,] <- results_base_p_i[1,] 
  total_results_p_i[2,] <- results_interv_p_i[1,] 
  
  NMB_p_i <- (incr_benefit_p_i-incr_cost_p_i) * n_farms_pig_ind
  
  #results matrix for smallholder pig farms
  results_base_p_s <- f_expvalue(m_param_p_s,m_cost_p_s,m_rwd_p_s)
  results_interv_p_s <- f_expvalue(m_param_p_s2,m_cost_p_s2,m_rwd_p_s2)
  
  total_results_p_s<- matrix(rep(0), nrow=2, ncol=2)
  colnames(total_results_p_s) <- c("Costs ($)", "Benefits ($)")
  rownames(total_results_p_s) <- c("Base Case", "Intervention")
  
  incr_cost_p_s <- (results_interv_p_s[1,1] - results_base_p_s[1,1])
  incr_benefit_p_s <-  (results_interv_p_s[1,2]-results_base_p_s[1,2])
  
  total_results_p_s[1,] <- results_base_p_s[1,] 
  total_results_p_s[2,] <- results_interv_p_s[1,] 
  
  NMB_p_s <- (incr_benefit_p_s-incr_cost_p_s) * n_farms_pig_small
  
  max_cost_discounted <- NMB_c_i + NMB_c_s + NMB_p_i + NMB_p_s + NMB_health + NMB_prod
  
  #convert maximum total cost to maximum annual cost
  discount_vector <- rep(0,n.t)
  discount_vector[1] <- 1
  for(i in 2:length(discount_vector)){
    discount_vector[i] = discount_vector[i-1]*(1-dr)
  }
  discount_sum <- sum(discount_vector)
  
  max_cost_annual <- max_cost_discounted / discount_sum
  
  money_saved_health <- -1 * incr_cost_health
  valuation_QALYs <- QALYs_saved*wtp
  
  #Final outputs
  outputs <- data.table("Maximum Acceptable Cost (Annual)"=max_cost_annual,
                        "Value of Productivity Gained"=NMB_prod,
                        "Cost Saved for Healthcare"=money_saved_health,
                        "Value of DALYs Averted"=valuation_QALYs,
                        "Increased Profit - Smallholder Pig Farms"=NMB_p_s,
                        "Increased Profit - Industrial Pig Farms"=NMB_p_i,
                        "Increased Profit - Smallholder Chicken Farms"=NMB_c_s,
                        "Increased Profit - Industrial Chicken Farms"=NMB_c_i)
  outputs
  
  return(outputs)
  
}


# Case Study --------------------------------------------------------------

Model_Case_Study <- function(inputs, scenario_income, scenario_prod, scenario_transmission,
                  scenario_farm_effect){

  if(scenario_income == "Viet Nam"){
    inputs[,"Value"] <- inputs[,"Viet Nam"]
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
  
  tstop <- n.t + 3
  
  pop <- ts(pop$Population, start = 1960, frequency = 1)#
  ARIMApop <- auto.arima(pop, stepwise = F, approximation = F)
  plot(forecast(ARIMApop, tstop))
  forecast(ARIMApop, tstop)
  predict <- forecast(ARIMApop, tstop)
  predict$mean
  futurepop <- predict$mean
  
  popchange <- numeric()
  for (i in 1:tstop) {
    popchange[i] <- futurepop[i+1] - futurepop[i]
  } #atm popchange[1] shows NET births in 2020
  
  popchange <- popchange[2:tstop] #now popchange[1] is NET popchange in 2021


# Dependency ratio --------------------------------------------------------

  dependency <- ts(dependency$Dependency.Ratio, start = 1960, frequency = 1)
  arimadependency <- auto.arima(dependency, stepwise = F, approximation = F)
  plot(forecast(arimadependency, tstop))
  predictdependency <- forecast(arimadependency, tstop)
  predictdependency$mean
  futuredepend <- predictdependency$mean
  
  dependency <- numeric()
  for(i in 1:tstop) {
    dependency[i] <- futuredepend[i + 1]
  } #now dependency[1] is the dependency in 2021, not 2020
  
  rm(i)
  
  #now get portion working age
  dependency <- dependency * 0.01 ## *0.01 added to make it a portion rather than a %%
  
  portion_working_age <- 1/(1+dependency)
  
  plot(portion_working_age)
  
  emp_rate <- inputs[parameter == "emp_rate", Value]
  lfpr <- inputs[parameter == "lfpr", Value][1]
  
  portion_working <- portion_working_age * emp_rate * lfpr #assuming unemployment rate equal to mean of 2016-2020 and LFPR equal to mean of 2016-2019
  
  plot(portion_working)
  
  # Human Epi Model ---------------------------------------------------------
  
  #building parameter matrix
  
  state_names <- c("well", "res","sus","dead", "was_well", "seq") ## the compartments
  transition_names  <- c("birth","r","s","mort_r", "mort_s","mort_w", "rec_r","rec_s", "dead_aft", "r_seq", "s_seq")  ## the transition probabilities
  parameter_names <- c(state_names, transition_names)
  
  state_i <- c(inputs[parameter=="pop",Value], rep(0,length=length(state_names)-1))
  #initial state vector
  
  m_param <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names))
  colnames(m_param) <- parameter_names
  rownames(m_param) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  m_param[ , "r_seq"] <- rep(inputs[parameter=="seq_res", Value], n.t) 
  #chance of developing sequelae following resistant infection
  
  m_param[ , "s_seq"] <- rep(inputs[parameter=="seq_sus", Value], n.t) 
  #chance of developing sequelae following susceptible infection
  
  m_param[ , "r"] <- rep(inputs[parameter=="well_sick", Value]*inputs[parameter=="portion_res",Value], n.t)
  #chance of developing a resistant infection in a year
  
  m_param[ , "s"] <- rep(inputs[parameter=="well_sick", Value]*(1-inputs[parameter=="portion_res",Value]), n.t)
  #chance of developing a susceptible infection in a year
  
  m_param[ , "mort_r"] <- rep(inputs[parameter=="mort_res", Value], n.t)
  #fatality from resistant infection
  
  m_param[ , "mort_s"] <- rep(inputs[parameter=="mort_sus", Value], n.t)
  #fatality from susceptible infection
  
  m_param[ , "rec_r"] <- rep(max(0,(1-(m_param[1,"mort_r"]+m_param[1,"r_seq"]))), n.t)
  #chance of recovering from a resistant infection
  
  m_param[ , "rec_s"] <- rep(max(0,(1-(m_param[1,"mort_s"]+m_param[1,"s_seq"]))), n.t)
  #chance of recovering from a susceptible infection
  
  m_param[ , "mort_w"] <- rep(0, n.t) 
  #chance of dying without infection from 'well', set to zero because background mortality is included in net births
  
  m_param[ , "dead_aft"] <- rep(1, n.t) 
  #all those who die go to the afterlife
  
  m_param[ , "birth"] <- popchange[1:n.t]
  #predicted net births in a given year
  
  
  m_param[1, 1:length(state_names)] <- state_i 
  #adding initial cycle 0 values
  
  #have growing AMR prevalence
  #note that we will later ensure that the total disease incidence does not change,
  #only the portion of infections from resistant bacteria
  amr_growth <- inputs[parameter=="amr_grow", Value]
  
  #set the maximum portion of resistant infections
  max_res <- inputs[parameter == "max_r", Value]
  
  for (i in 2:(n.t)){
    m_param[i, "r"] <- m_param[i-1, "r"]*amr_growth
  }
  
  for(i in 1:n.t){
    if(m_param[i, "r"] > max_res *(m_param[1,"r"]+m_param[1,"s"])){ 
      m_param[i, "r"] <- max_res *(m_param[1,"r"]+m_param[1,"s"]) 
    }
    m_param[i, "s"] <- m_param[1,"r"]+m_param[1,"s"] - m_param[i, "r"] 
  }
  #made it so that, while the incidence of resistant infections increases,
  #the total number of infections doesn't increase. However, if we wanted to 
  #have a changing number of infections over time, we could do that, but would
  #need to replace "m_param[1,"r"]+m_param[1,"s"]" with a parameter called 
  #something like "chance_sick" which can change each period, and have that 
  #grow before allowing the AMR incidence to grow
  
  #make sure that the transition probabilities don't exceed 1
  for(i in 1:n.t){
    m_param[i, "mort_r"] <- m_param[i, "mort_r"] / (m_param[i, "mort_r"] + m_param[i, "rec_r"] + m_param[i, "r_seq"])
    m_param[i, "rec_r"] <- m_param[i, "rec_r"] / (m_param[i, "mort_r"] + m_param[i, "rec_r"] + m_param[i, "r_seq"])
    m_param[i, "r_seq"] <- m_param[i, "r_seq"] / (m_param[i, "mort_r"] + m_param[i, "rec_r"] + m_param[i, "r_seq"])
    
    m_param[i, "mort_s"] <- m_param[i, "mort_s"] / (m_param[i, "mort_s"] + m_param[i, "rec_s"] + m_param[i, "s_seq"])
    m_param[i, "rec_s"] <- m_param[i, "rec_s"] / (m_param[i, "mort_s"] + m_param[i, "rec_s"] + m_param[i, "s_seq"])
    m_param[i, "s_seq"] <- m_param[i, "s_seq"] / (m_param[i, "mort_s"] + m_param[i, "rec_s"] + m_param[i, "s_seq"])
  }
  
  ##set the initial state
  
  #born
  m_param[1, "well"] <- m_param[1, "well"] + m_param[1, "birth"]
  
  #transition out of well
  m_param[1, "was_well"] <- m_param[1, "well"]
  m_param[1, "well"] <- m_param[1, "was_well"] * (1 - m_param[1, "r"] - m_param[1, "s"] - m_param[1, "mort_w"])
  
  m_param[1, "res"] <- m_param[1, "was_well"] * m_param[1, "r"]
  m_param[1, "sus"] <- m_param[1, "was_well"] * m_param[1, "s"]
  m_param[1, "dead"] <- m_param[1, "was_well"] * m_param[1, "mort_w"]
  
  #transition out of sick
  m_param[1, "well"] <- m_param[1, "well"] + 
    (m_param[1, "rec_r"] * m_param[1, "res"]) + 
    (m_param[1, "rec_s"] * m_param[1, "sus"])
  
  m_param[1, "dead"] <- m_param[1, "dead"] +
    (m_param[1, "mort_s"] * m_param[1, "sus"]) + 
    (m_param[1, "mort_r"] * m_param[1, "res"])
  
  m_param[1, "seq"] <- m_param[1, "seq"] +
    (m_param[1, "s_seq"] * m_param[1, "sus"]) + 
    (m_param[1, "r_seq"] * m_param[1, "res"])
  
  ##difference equation
  
  f_human_epi <- function(m_param, n.t){
    
    n.t.val <- n.t
    
    for(i in 2:n.t.val){
      
      #carry over from last period and be born
      m_param[i, "well"] <- m_param[i-1, "well"] + m_param[i, "birth"]
      
      m_param[i, "was_well"] <- m_param[i, "well"]
      
      #transition out of well
      m_param[i, "well"] <- m_param[i, "was_well"] * (1 - m_param[i, "r"] - m_param[i, "s"] - m_param[i, "mort_w"])
      
      m_param[i, "res"] <- m_param[i, "was_well"] * m_param[i, "r"]
      m_param[i, "sus"] <- m_param[i, "was_well"] * m_param[i, "s"]
      m_param[i, "dead"] <- m_param[i, "was_well"] * m_param[i, "mort_w"]
      
      #transition out of sick
      m_param[i, "well"] <- m_param[i, "well"] + 
        (m_param[i, "rec_r"] * m_param[i, "res"]) + 
        (m_param[i, "rec_s"] * m_param[i, "sus"])
      
      m_param[i, "dead"] <- m_param[i, "dead"] +
        (m_param[i, "mort_s"] * m_param[i, "sus"]) + 
        (m_param[i, "mort_r"] * m_param[i, "res"])
      
      m_param[i, "seq"] <- m_param[i, "seq"] +
        (m_param[i, "s_seq"] * m_param[i, "sus"]) + 
        (m_param[i, "r_seq"] * m_param[i, "res"])
      
    }
    
    return(m_param)
  }
  
  m_param <- f_human_epi(m_param,n.t) 
  
  # Healthcare Costs --------------------------------------------------------
  
  dr <- inputs[parameter == "dr", Value]
  
  m_cost <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names))
  colnames(m_cost) <- parameter_names
  rownames(m_cost) <- paste("cycle", 0:(n.t-1), sep  =  "")  
  
  #cost of hospital stay for res and sus infection
  
  c_r <- inputs[parameter == "los_sus", Value] * inputs[parameter == "bed_day_cost", Value] * 365.25
  c_s <- inputs[parameter == "los_res", Value] * inputs[parameter == "bed_day_cost", Value] * 365.25
  
  cost_i <- c(0,c_r,c_s,0,0,0) #only infections incur a healthcare cost here
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_cost[2, 1:length(state_names)] <- cost_i
  
  for (j in 1:length(state_names)) {
    for (i in 3:(n.t)){
      m_cost[i,j] <- f_di(m_cost[i-1,j],dr)
    }  
  }
  
  
  # Healthcare Rewards ------------------------------------------------------
  
  m_rwd <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names))
  colnames(m_rwd) <- parameter_names
  rownames(m_rwd) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #calculate the present value of expected remaining life years for a) a perfectly
  #healthy person and b) someone with sequelae. The negative difference is the 'reward' 
  #of being in the sequelae state 
  
  pv_fut_life <- c(rep(0,n.t-1))
  
  for (i in 1:n.t-1){
    pv_fut_life[i] <- inputs[parameter=="background_qol", Value] * (1-dr)^(i-1)
  }
  pv_life <- sum(pv_fut_life)
  
  pv_fut_life_seq <- c(rep(0,n.t-1))
  for (i in 1:n.t-1){
    pv_fut_life_seq[i] <- inputs[parameter=="qol_seq", Value] * (1-dr)^(i-1)
  }
  pv_life_seq <- sum(pv_fut_life_seq)
  
  #the 'reward' for death is not sweet release, but in fact the negative of the present value of remaining life 
  #years, and the 'reward' for being infected is the loss of welfare while hospitalised
  #therefore in a scenario with more deaths, infections and sequelae will have a negative
  #'reward' of larger absolute value
  
  r_s <- inputs[parameter == "los_sus", Value] *
    (inputs[parameter == "qol_sick", Value] - inputs[parameter == "background_qol", Value])
  
  r_r <- inputs[parameter == "los_res", Value] * 
    (inputs[parameter == "qol_sick", Value] - inputs[parameter == "background_qol", Value])
  
  r_d <- -1 * pv_life #discounted QoL loss from death
  r_seq <- pv_life_seq - pv_life # fixed this because we were previously assigning a benefit to sequelae (the subtraction was the wrong way around lol)
  
  rwd_i <- c(0,r_r,r_s,r_d,0,r_seq) 
  
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd[2, 1:length(state_names)] <- rwd_i
  
  ### accounting for discounting
  for (j in 1:length(state_names)) {
    for (i in 3:(n.t)){
      m_rwd[i,j] <- f_di(m_rwd[i-1,j],dr)
    }  
  }
  
  
  # Productivity Costs -------------------------------------------------------
  
  #for HCA and FCA, we only care about the losses in productivity, so we set the 
  #reward for 'well' to be zero. Going into the 'dead' state incurs a productivity
  #loss equal to the discounted value of forgone future earnings, going into the
  #'res' or 'sus states incurs a loss equal to the earnings that would have
  #been made during the time in hospital. After 1 period, all people in 'dead' go
  #to 'afterlife', which has a reward of zero. 
  #Using the HCA, the forgone future earnings are those of expected remaining economically
  #active years. For FCA, it is the forgone earnings during the time that it takes
  #to find a replacement worker
  
  #all productivity rewards are set to zero, and we only see a difference between
  #the reward matrices of different scenarios
  m_cost_prod <- matrix(rep(0),nrow = n.t, ncol = length(parameter_names))
  colnames(m_cost_prod) <- parameter_names
  rownames(m_cost_prod) <- paste("cycle", 0:(n.t-1), sep = "")
  
  cost_i_prod <- rep(0,(length(state_names)))
  
  ## start at cycle 1 so you do not multiply initial state vector
  m_cost_prod[2,1:length(state_names)] <- cost_i_prod
  
  for (j in 1:length(state_names)) {
    for (i in 3:(n.t)){
      m_cost_prod[i,j] <- f_di(m_cost_prod[i-1,j],dr)
    }
  }
  
  
  # Productivity Rewards ----------------------------------------------------
  
  #the 'reward' for being infected is equal to the negative forgone productivity
  #while hospitalised
  
  m_rwd_prod <- matrix(rep(0), nrow = n.t, ncol = length(parameter_names))
  colnames(m_rwd_prod) <- parameter_names
  rownames(m_rwd_prod) <- paste("cycle", 0:(n.t-1), sep = "")
  
  r_r_prod <- -1 * inputs[parameter == "los_res", Value] * 
    (inputs[parameter == "prod_pc", Value]*#inputs[parameter == "lfpr", Value])* 
       inputs[parameter == "unpaid_prod_pc", Value])
  
  r_s_prod <- -1 * inputs[parameter == "los_sus", Value] *
    (inputs[parameter == "prod_pc", Value]*#inputs[parameter == "lfpr", Value])*
       inputs[parameter == "unpaid_prod_pc", Value])
  
  r_w_prod <- 0
  
  r_aft_prod <- 0
  
  r_seq_prod <- 0 #importantly assumes that people with sequelae are equally productive
  
  #the reward for dead is the present discounted value of future work (either for the
  #remainder of economically active life, or for the 6 months needed to find a replacement)
  
  remaining_work_years <- inputs[parameter == "remaining_work_years", Value]
  
  yearly_prod <- (inputs[parameter == "prod_pc", Value]*inputs[parameter == "lfpr", Value]*
    inputs[parameter == "unpaid_prod_pc", Value])[1]
  
  pv_fut_prod <- c(rep(0,remaining_work_years))
  
  dr_pgrowth <- dr - inputs[parameter=="prod_growth", Value] 
  ##discount rate net of productivity growth (in theory it can be negative)
  
  for (i in 1:remaining_work_years){ 
    pv_fut_prod[i] <- yearly_prod * (1-dr_pgrowth)^(i-1)
  }
  
  pv_life_prod <- sum(pv_fut_prod)
  
  if(scenario_prod == "HCA"){
    r_d_prod <- -1 * pv_life_prod
  } else if(scenario_prod == "FCA"){
    r_d_prod <- -0.5 * yearly_prod
  } else{
    paste("ERROR: PLEASE CHOOSE AN APPROACH TO ESTIMATING PRODUCTIVITY OUTCOMES")
  }
  
  rwd_i_prod <- c(r_w_prod, r_r_prod, r_s_prod, r_d_prod, r_aft_prod, r_seq_prod)
  
  ## start at cycle 1 so you do not multiply initial state vector
  m_rwd_prod[2,1:length(state_names)] <- rwd_i_prod 
  
  ### discount, but also account for labour productivity growth
  
  for (j in 1:length(state_names)) {
    for (i in 3:(n.t)){
      m_rwd_prod[i,j] <- f_di(m_rwd_prod[i-1,j],dr_pgrowth)
    }
  }
  
  #now we multiply the productivity rewards across all states and time steps
  #(apart from initial, which we already did) by the portion of people working
  for (j in 1:length(state_names)) {
    for (i in 2:(n.t)) {
      m_rwd_prod[i,j] <- m_rwd_prod[i,j]*portion_working[i]
    }
  } 
  
  # Animal Epi Model --------------------------------------------------------
  
  state_names_a <- c("well", "res","sus","fallen","sold") ## the compartments
  transition_names_a  <- c("birth","r","s","mort_r", "mort_s","mort_w", "rec_r","rec_s","w_sold")  ## the rates
  parameter_names_a <- c(state_names_a, transition_names_a)
  
  f_animal_epi <- function(m_param_a_base, n.t, scenario_animal){
    
    n.t.val <- n.t
    
    if(scenario_animal == "chicken_small"){
      n_animals_farm <- inputs[parameter == "n_chickens_farm_small", Value]
      annual_cycles <- inputs[parameter == "pcycles_chicken_small", Value]
    } else if (scenario_animal == "chicken_ind"){
      n_animals_farm <- inputs[parameter == "n_chickens_farm_ind", Value]
      annual_cycles <- inputs[parameter == "pcycles_chicken_ind", Value]
    } else if (scenario_animal == "pig_small"){
      n_animals_farm <- inputs[parameter == "n_pigs_farm_small", Value]
      annual_cycles <- inputs[parameter == "pcycles_pig_small", Value]
    } else if (scenario_animal == "pig_ind"){
      n_animals_farm <- inputs[parameter == "n_pigs_farm_ind", Value]
      annual_cycles <- inputs[parameter == "pcycles_pig_ind", Value]  
    }    
    
    m_param_a_temp <- m_param_a_base[1:4,]
    rownames(m_param_a_temp) <- NULL ##removing rownames
    
    i <- 2 ## getting sick
    m_param_a_temp[i,"well"] <- m_param_a_temp[i-1,"well"] -(m_param_a_temp[i-1,"r"]*m_param_a_temp[i-1,"well"]) -
      (m_param_a_temp[i-1,"s"]*m_param_a_temp[i-1,"well"])
    m_param_a_temp[i,"res"] <- m_param_a_temp[i-1,"res"] + (m_param_a_temp[i-1,"r"]*m_param_a_temp[i-1,"well"]) 
    m_param_a_temp[i,"sus"] <- m_param_a_temp[i-1,"sus"] + (m_param_a_temp[i-1,"s"]*m_param_a_temp[i-1,"well"])
    
    i <- 3 ## dying and recovering
    m_param_a_temp[i,"fallen"] <- (m_param_a_temp[i-1,"mort_w"]*m_param_a_temp[i-1,"well"]) +
      (m_param_a_temp[i-1,"mort_r"]*m_param_a_temp[i-1,"res"]) + 
      (m_param_a_temp[i-1,"mort_s"]*m_param_a_temp[i-1,"sus"])
    m_param_a_temp[i, "well"] <- m_param_a_temp[i-1,"well"] - (m_param_a_temp[i-1,"well"]*m_param_a_temp[i-1,"mort_w"]) +
      (m_param_a_temp[i-1,"rec_r"]*m_param_a_temp[i-1,"res"])+ 
      (m_param_a_temp[i-1,"rec_s"]*m_param_a_temp[i-1,"sus"])
    
    i <- 4 ##sold
    m_param_a_temp[i,"sold"] <- (m_param_a_temp[i-1,"w_sold"]*m_param_a_temp[i-1,"well"])
    
    ## final states
    m_a_sum <- m_param_a_temp[4,]
    m_a_sum["res"] <- m_param_a_temp[2,"res"]
    m_a_sum["sus"] <- m_param_a_temp[2,"sus"]
    m_a_sum["well"] <- n_animals_farm - m_a_sum["res"] - m_a_sum["sus"] ## reset the number in 'well'
    
    m_a_sum[1:5] <- annual_cycles * m_a_sum[1:5] #multiply by the number of annual cycles
    
    m_param_a <- matrix(rep(m_a_sum), nrow=n.t.val, ncol =length(parameter_names_a))
    m_param_a <- t(replicate(n.t.val,m_a_sum))
    colnames(m_param_a) <- parameter_names_a
    rownames(m_param_a) <- paste("cycle", 0:(n.t-1), sep  =  "")
    
    m_param_a
    
    return(m_param_a)
    
  }
  
  # Chicken Epi Module - Smallholder -------------------------------------------------------
  
  scenario_animal <- "chicken_small"
  
  state_i_c_s <- c(inputs[parameter == "n_chickens_farm_small", Value], rep(0,length=length(state_names_a)-1))
  
  m_param_c_s_base <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_param_c_s_base) <- parameter_names_a
  rownames(m_param_c_s_base) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #here, we have a set mortality rate for all states - 
  #it only matters if animals are in 'res' or 'sus' for the purpose of calculating 
  #the cost of therapeutic treatment
  
  m_param_c_s_base[ , "r"] <- rep(inputs[parameter=="c_res_small", Value], n.t)
  m_param_c_s_base[ , "s"] <- rep(inputs[parameter=="c_sus_small", Value], n.t)
  m_param_c_s_base[ , "mort_s"] <- rep(inputs[parameter=="c_mort_small", Value], n.t) 
  m_param_c_s_base[ , "mort_r"] <- rep(inputs[parameter=="c_mort_small", Value], n.t)
  m_param_c_s_base[ , "rec_r"] <- rep(1-(m_param_c_s_base[1,"mort_r"]), n.t)
  m_param_c_s_base[ , "rec_s"] <- rep(1-(m_param_c_s_base[1,"mort_s"]), n.t)
  m_param_c_s_base[ , "birth"] <- rep(1, n.t)
  m_param_c_s_base[ , "mort_w"] <- rep(inputs[parameter=="c_mort_small", Value], n.t)
  m_param_c_s_base[ , "w_sold"] <- rep(1, n.t) 
  
  #make it so that the total incidence of infections stays the same, and only the
  #portion of them that are resistant changes
  for(i in 1:n.t){
    if(m_param_c_s_base[i, "r"] > max_res * (m_param_c_s_base[1, "r"] + m_param_c_s_base[1, "s"])){
      m_param_c_s_base[i, "r"] <- max_res * (m_param_c_s_base[1, "r"] + m_param_c_s_base[1, "s"])
    }
    m_param_c_s_base[i, "s"] <- m_param_c_s_base[1, "r"] + m_param_c_s_base[1, "s"] - m_param_c_s_base[i, "r"]
  }
  
  m_param_c_s_base[1, 1:length(state_names_a)] <- state_i_c_s
  
  m_param_c_s <- f_animal_epi(m_param_c_s_base,n.t, scenario_animal)
  ### ignore totals of transition probs etc. as they are over counted etc.
  ## just want to focus on health state totals
  
  
  # Chicken Epi Module - Industrial -----------------------------------------
  
  scenario_animal <- "chicken_ind"
  
  state_i_c_i <- c(inputs[parameter == "n_chickens_farm_ind", Value], rep(0,length=length(state_names_a)-1))
  
  m_param_c_i_base <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_param_c_i_base) <- parameter_names_a
  rownames(m_param_c_i_base) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #here, we have a set mortality rate for all states - 
  #it only matters if animals are in 'res' or 'sus' for the purpose of calculating 
  #the cost of therapeutic treatment
  
  m_param_c_i_base[ , "r"] <- rep(inputs[parameter=="c_res_ind", Value], n.t)
  m_param_c_i_base[ , "s"] <- rep(inputs[parameter=="c_sus_ind", Value], n.t)
  m_param_c_i_base[ , "mort_s"] <- rep(inputs[parameter=="c_mort_ind", Value], n.t) 
  m_param_c_i_base[ , "mort_r"] <- rep(inputs[parameter=="c_mort_ind", Value], n.t)
  m_param_c_i_base[ , "rec_r"] <- rep(1-(m_param_c_i_base[1,"mort_r"]), n.t)
  m_param_c_i_base[ , "rec_s"] <- rep(1-(m_param_c_i_base[1,"mort_s"]), n.t)
  m_param_c_i_base[ , "birth"] <- rep(1, n.t)
  m_param_c_i_base[ , "mort_w"] <- rep(inputs[parameter=="c_mort_ind", Value], n.t)
  m_param_c_i_base[ , "w_sold"] <- rep(1, n.t) 
  
  #make it so that the total incidence of infections stays the same, and only the
  #portion of them that are resistant changes
  for(i in 1:n.t){
    if(m_param_c_i_base[i, "r"] > max_res * (m_param_c_i_base[1, "r"] + m_param_c_i_base[1, "s"])){
      m_param_c_i_base[i, "r"] <- max_res * (m_param_c_i_base[1, "r"] + m_param_c_i_base[1, "s"])
    }
    m_param_c_i_base[i, "s"] <- m_param_c_i_base[1, "r"] + m_param_c_i_base[1, "s"] - m_param_c_i_base[i, "r"]
  }
  
  m_param_c_i_base[1, 1:length(state_names_a)] <- state_i_c_i
  
  m_param_c_i <- f_animal_epi(m_param_c_i_base,n.t, scenario_animal)
  ### ignore totals of transition probs etc. as they are over counted etc.
  ## just want to focus on health state totals
  
  
  # Pig Epi Module - Smallholder --------------------------------------------
  
  scenario_animal <- "pig_small"
  
  state_i_p_s <- c(inputs[parameter == "n_pigs_farm_small", Value], rep(0,length=length(state_names_a)-1))
  
  m_param_p_s_base <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_param_p_s_base) <- parameter_names_a
  rownames(m_param_p_s_base) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #here, we have a set mortality rate for all states - 
  #it only matters if animals are in 'res' or 'sus' for the purpose of calculating 
  #the cost of therapeutic treatment
  
  m_param_p_s_base[ , "r"] <- rep(inputs[parameter=="p_res_small", Value], n.t)
  m_param_p_s_base[ , "s"] <- rep(inputs[parameter=="p_sus_small", Value], n.t)
  m_param_p_s_base[ , "mort_s"] <- rep(inputs[parameter=="p_mort_small", Value], n.t) 
  m_param_p_s_base[ , "mort_r"] <- rep(inputs[parameter=="p_mort_small", Value], n.t)
  m_param_p_s_base[ , "rec_r"] <- rep(1-(m_param_p_s_base[1,"mort_r"]), n.t)
  m_param_p_s_base[ , "rec_s"] <- rep(1-(m_param_p_s_base[1,"mort_s"]), n.t)
  m_param_p_s_base[ , "birth"] <- rep(1, n.t)
  m_param_p_s_base[ , "mort_w"] <- rep(inputs[parameter=="p_mort_small", Value], n.t)
  m_param_p_s_base[ , "w_sold"] <- rep(1, n.t) 
  
  #make it so that the total incidence of infections stays the same, and only the
  #portion of them that are resistant changes
  for(i in 1:n.t){
    if(m_param_p_s_base[i, "r"] > max_res * (m_param_p_s_base[1, "r"] + m_param_p_s_base[1, "s"])){
      m_param_p_s_base[i, "r"] <- max_res * (m_param_p_s_base[1, "r"] + m_param_p_s_base[1, "s"])
    }
    m_param_p_s_base[i, "s"] <- m_param_p_s_base[1, "r"] + m_param_p_s_base[1, "s"] - m_param_p_s_base[i, "r"]
  }
  
  m_param_p_s_base[1, 1:length(state_names_a)] <- state_i_p_s
  
  m_param_p_s <- f_animal_epi(m_param_p_s_base,n.t, scenario_animal)
  ### ignore totals of transition probs etc. as they are over counted etc.
  ## just want to focus on health state totals
  
  
  # Pig Epi Module - Industrial ---------------------------------------------
  
  scenario_animal <- "pig_ind"
  
  state_i_p_i <- c(inputs[parameter == "n_pigs_farm_ind", Value], rep(0,length=length(state_names_a)-1))
  
  m_param_p_i_base <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_param_p_i_base) <- parameter_names_a
  rownames(m_param_p_i_base) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #here, we have a set mortality rate for all states - 
  #it only matters if animals are in 'res' or 'sus' for the purpose of calculating 
  #the cost of therapeutic treatment
  
  m_param_p_i_base[ , "r"] <- rep(inputs[parameter=="p_res_ind", Value], n.t)
  m_param_p_i_base[ , "s"] <- rep(inputs[parameter=="p_sus_ind", Value], n.t)
  m_param_p_i_base[ , "mort_s"] <- rep(inputs[parameter=="p_mort_ind", Value], n.t) 
  m_param_p_i_base[ , "mort_r"] <- rep(inputs[parameter=="p_mort_ind", Value], n.t)
  m_param_p_i_base[ , "rec_r"] <- rep(1-(m_param_p_i_base[1,"mort_r"]), n.t)
  m_param_p_i_base[ , "rec_s"] <- rep(1-(m_param_p_i_base[1,"mort_s"]), n.t)
  m_param_p_i_base[ , "birth"] <- rep(1, n.t)
  m_param_p_i_base[ , "mort_w"] <- rep(inputs[parameter=="p_mort_ind", Value], n.t)
  m_param_p_i_base[ , "w_sold"] <- rep(1, n.t) 
  
  #make it so that the total incidence of infections stays the same, and only the
  #portion of them that are resistant changes
  for(i in 1:n.t){
    if(m_param_p_i_base[i, "r"] > max_res * (m_param_p_i_base[1, "r"] + m_param_p_i_base[1, "s"])){
      m_param_p_i_base[i, "r"] <- max_res * (m_param_p_i_base[1, "r"] + m_param_p_i_base[1, "s"])
    }
    m_param_p_i_base[i, "s"] <- m_param_p_i_base[1, "r"] + m_param_p_i_base[1, "s"] - m_param_p_i_base[i, "r"]
  }
  
  m_param_p_i_base[1, 1:length(state_names_a)] <- state_i_p_i
  
  m_param_p_i <- f_animal_epi(m_param_p_i_base,n.t, scenario_animal)
  ### ignore totals of transition probs etc. as they are over counted etc.
  ## just want to focus on health state totals  
  
  
  #'All of the farm costs are zero here, as any changes to profits per animal
  #'from the intervention will be reflected in changes to the 'farm rewards' 
  
  # Chicken Farm Costs - Smallholder ----------------------------------------
  
  m_cost_c_s <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_cost_c_s) <- parameter_names_a
  rownames(m_cost_c_s) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  # Chicken Farm Costs - Industrial -----------------------------------------
  
  m_cost_c_i <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_cost_c_i) <- parameter_names_a
  rownames(m_cost_c_i) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  # Pig Farm Costs - Smallholder --------------------------------------------
  
  m_cost_p_s <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_cost_p_s) <- parameter_names_a
  rownames(m_cost_p_s) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  # Pig Farm Costs - Industrial ---------------------------------------------
  
  m_cost_p_i <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_cost_p_i) <- parameter_names_a
  rownames(m_cost_p_i) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  
  # Chicken Farm Rewards - Smallholder --------------------------------------
  
  m_rwd_c_s <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_rwd_c_s) <- parameter_names_a
  rownames(m_rwd_c_s) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #only get a reward for selling animals
  r_sold_c_s <- inputs[parameter == "chicken_weight", Value] * inputs[parameter == "chicken_price", Value]
  rwd_i_c_s <- c(0,0,0,0,r_sold_c_s)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_c_s[2, 1:length(state_names_a)] <- rwd_i_c_s
  
  #discount
  for (j in 1:length(state_names_a)) {
    for (i in 3:(n.t)){
      m_rwd_c_s[i,j] <- f_di(m_rwd_c_s[i-1,j],dr)
    }  
  } 
  
  # Chicken Farm Rewards - Industrial ---------------------------------------
  
  m_rwd_c_i <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_rwd_c_i) <- parameter_names_a
  rownames(m_rwd_c_i) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #only get a reward for selling animals
  r_sold_c_i <- inputs[parameter == "chicken_weight", Value] * inputs[parameter == "chicken_price", Value]
  rwd_i_c_i <- c(0,0,0,0,r_sold_c_i)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_c_i[2, 1:length(state_names_a)] <- rwd_i_c_i
  
  #discount
  for (j in 1:length(state_names_a)) {
    for (i in 3:(n.t)){
      m_rwd_c_i[i,j] <- f_di(m_rwd_c_i[i-1,j],dr)
    }  
  } 
  
  # Pig Farm Rewards - Smallholder --------------------------------------
  
  m_rwd_p_s <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_rwd_p_s) <- parameter_names_a
  rownames(m_rwd_p_s) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #only get a reward for selling animals
  r_sold_p_s <- inputs[parameter == "pig_weight", Value] * inputs[parameter == "pig_price", Value]
  rwd_i_p_s <- c(0,0,0,0,r_sold_p_s)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_p_s[2, 1:length(state_names_a)] <- rwd_i_p_s
  
  #discount
  for (j in 1:length(state_names_a)) {
    for (i in 3:(n.t)){
      m_rwd_p_s[i,j] <- f_di(m_rwd_p_s[i-1,j],dr)
    }  
  } 
  
  # Pig Farm Rewards - Industrial ---------------------------------------
  
  m_rwd_p_i <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_rwd_p_i) <- parameter_names_a
  rownames(m_rwd_p_i) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #only get a reward for selling animals
  r_sold_p_i <- inputs[parameter == "pig_weight", Value] * inputs[parameter == "pig_price", Value]
  rwd_i_p_i <- c(0,0,0,0,r_sold_p_i)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_p_i[2, 1:length(state_names_a)] <- rwd_i_p_i
  
  #discount
  for (j in 1:length(state_names_a)) {
    for (i in 3:(n.t)){
      m_rwd_p_i[i,j] <- f_di(m_rwd_p_i[i-1,j],dr)
    }  
  }  
  
  
  # The Intervention --------------------------------------------------------
  
  pig_income_effect     <- as.numeric()
  chicken_income_effect <- as.numeric()
  
  if(scenario_farm_effect == "min"){
    pig_income_effect     <- inputs[parameter == "pig_income_effect", Min]
    chicken_income_effect <- inputs[parameter == "chicken_income_effect", Min]
  } else if (scenario_farm_effect == "lo"){
    pig_income_effect     <- inputs[parameter == "pig_income_effect", Lo]
    chicken_income_effect <- inputs[parameter == "chicken_income_effect", Lo]
  } else if (scenario_farm_effect == "med"){
    pig_income_effect     <- inputs[parameter == "pig_income_effect", Med]
    chicken_income_effect <- inputs[parameter == "chicken_income_effect", Med]
  } else if (scenario_farm_effect == "hi"){
    pig_income_effect     <- inputs[parameter == "pig_income_effect", Hi]
    chicken_income_effect <- inputs[parameter == "chicken_income_effect", Hi]
  } else if (scenario_farm_effect == "max"){
    pig_income_effect     <- inputs[parameter == "pig_income_effect", Max]
    chicken_income_effect <- inputs[parameter == "chicken_income_effect", Max]
  }  
  
  ##Intervention - Humans
  
  #reduction in incidence of drug resistant infections in humans
  m_param2 <- m_param ## human parameter matrix for the intervention scenario
  
  #reduce the chance of getting a resistant infection in humans, depending on the link parameter used
  
  for(i in 1:n.t){ ## fixed because previously the intervention was erasing the background growth in human AMR 
    if(scenario_transmission == "low"){
      m_param2[i , "r"] <- m_param2[i , "r"]*(1+inputs[parameter=="res_change", Lo])
    } else if (scenario_transmission == "med"){
      m_param2[i , "r"] <- m_param2[i , "r"]*(1+inputs[parameter=="res_change", Med])
    } else if (scenario_transmission == "hi"){
      m_param2[i , "r"] <- m_param2[i , "r"]*(1+inputs[parameter=="res_change", Hi])
    } else if (scenario_transmission == "max"){
      m_param2[i , "r"] <- m_param2[i , "r"]*(1+inputs[parameter=="res_change", Max])
    } 
  }
  
  #make sure that the total number of infections remains constant
  for(i in 1:n.t){
    if(m_param2[i, "r"] > max_res * (m_param2[1,"r"]+m_param2[1,"s"])){ 
      m_param2[i, "r"] <- max_res * (m_param2[1,"r"]+m_param2[1,"s"]) 
    }
    m_param2[i, "s"] <- m_param2[1,"r"]+m_param2[1,"s"] - m_param2[i, "r"]
  }
  
  ## clear state values
  m_param2[ , 1:length(state_names)] <- 0
  m_param2[1, 1:length(state_names)] <- state_i
  
  ##set the initial state
  
  #born
  m_param2[1, "well"] <- m_param2[1, "well"] + m_param2[1, "birth"]
  
  #transition out of well
  m_param2[1, "was_well"] <- m_param2[1, "well"]
  m_param2[1, "well"] <- m_param2[1, "was_well"] * (1 - m_param2[1, "r"] - m_param2[1, "s"] - m_param2[1, "mort_w"])
  
  m_param2[1, "res"] <- m_param2[1, "was_well"] * m_param2[1, "r"]
  m_param2[1, "sus"] <- m_param2[1, "was_well"] * m_param2[1, "s"]
  m_param2[1, "dead"] <- m_param2[1, "was_well"] * m_param2[1, "mort_w"]
  
  #transition out of sick
  m_param2[1, "well"] <- m_param2[1, "well"] + 
    (m_param2[1, "rec_r"] * m_param2[1, "res"]) + 
    (m_param2[1, "rec_s"] * m_param2[1, "sus"])
  
  m_param2[1, "dead"] <- m_param2[1, "dead"] +
    (m_param2[1, "mort_s"] * m_param2[1, "sus"]) + 
    (m_param2[1, "mort_r"] * m_param2[1, "res"])
  
  m_param2[1, "seq"] <- m_param2[1, "seq"] +
    (m_param2[1, "s_seq"] * m_param2[1, "sus"]) + 
    (m_param2[1, "r_seq"] * m_param2[1, "res"])
  
  #apply the human epi function to the intervention case
  m_param2 <- f_human_epi(m_param2, n.t) 
  
  ##Intervention - Chickens - Industrial
  
  #create a parameter spreadsheet for the intervention case
  m_param_c_i2 <- m_param_c_i 
  
  #change in mortality
  
  m_param_c_i2[ , "mort_w"] <- m_param_c_i2[ , "mort_w"] * (1+inputs[parameter == "chicken_mort_effect", Value])
  
  m_param_c_i2[ , "mort_s"] <- m_param_c_i2[ , "mort_s"] * (1+inputs[parameter == "chicken_mort_effect", Value])
  
  m_param_c_i2[ , "mort_r"] <- m_param_c_i2[ , "mort_r"] * (1+inputs[parameter == "chicken_mort_effect", Value])
  
  #make sure the total number of infections stays constant
  for(i in 1:n.t){
    m_param_c_i2[i, "s"] <- m_param_c_i2[1, "r"] + m_param_c_i2[1, "s"] - m_param_c_i2[i, "r"]
  }
  
  m_param_c_i2[ , 1:length(state_names_a)] <- 0
  m_param_c_i2[1, 1:length(state_names_a)] <- state_i_c_i
  
  #apply the animal epi function
  scenario_animal <- "chicken_ind"
  m_param_c_i2 <- f_animal_epi(m_param_c_i2, n.t, scenario_animal)
  
  #rewards
  m_rwd_c_i2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_rwd_c_i2) <- parameter_names_a
  rownames(m_rwd_c_i2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  r_sold_c_i2 <- r_sold_c_i * (1+chicken_income_effect)
  rwd_i_c_i2 <- c(0,0,0,0,r_sold_c_i2)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_c_i2[2, 1:length(state_names_a)] <- rwd_i_c_i2
  
  #discount
  for (j in 1:length(state_names_a)) {
    for (i in 3:(n.t)){
      m_rwd_c_i2[i,j] <- f_di(m_rwd_c_i2[i-1,j],dr)
    }  
  }
  
  #costs
  m_cost_c_i2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_cost_c_i2) <- parameter_names_a
  rownames(m_cost_c_i2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  ##Intervention - Chickens - Smallholder
  
  #create a parameter spreadsheet for the intervention case
  m_param_c_s2 <- m_param_c_s 
  
  #change in mortality
  
  m_param_c_s2[ , "mort_w"] <- m_param_c_s2[ , "mort_w"] * (1+inputs[parameter == "chicken_mort_effect", Value])
  
  m_param_c_s2[ , "mort_s"] <- m_param_c_s2[ , "mort_s"] * (1+inputs[parameter == "chicken_mort_effect", Value])
  
  m_param_c_s2[ , "mort_r"] <- m_param_c_s2[ , "mort_r"] * (1+inputs[parameter == "chicken_mort_effect", Value])
  
  #make sure the total number of infections stays constant
  for(i in 1:n.t){
    m_param_c_s2[i, "s"] <- m_param_c_s2[1, "r"] + m_param_c_s2[1, "s"] - m_param_c_s2[i, "r"]
  }
  
  m_param_c_s2[ , 1:length(state_names_a)] <- 0
  m_param_c_s2[1, 1:length(state_names_a)] <- state_i_c_s
  
  #apply the animal epi function
  scenario_animal <- "chicken_small"
  m_param_c_s2 <- f_animal_epi(m_param_c_s2, n.t, scenario_animal)
  
  #rewards
  m_rwd_c_s2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_rwd_c_s2) <- parameter_names_a
  rownames(m_rwd_c_s2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  r_sold_c_s2 <- r_sold_c_s * (1+chicken_income_effect)
  rwd_i_c_s2 <- c(0,0,0,0,r_sold_c_s2)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_c_s2[2, 1:length(state_names_a)] <- rwd_i_c_s2
  
  #discount
  for (j in 1:length(state_names_a)) {
    for (i in 3:(n.t)){
      m_rwd_c_s2[i,j] <- f_di(m_rwd_c_s2[i-1,j],dr)
    }  
  }
  
  #costs
  m_cost_c_s2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_cost_c_s2) <- parameter_names_a
  rownames(m_cost_c_s2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  ##Intervention - Pigs - Industrial
  
  #create a parameter spreadsheet for the intervention case
  m_param_p_i2 <- m_param_p_i 
  
  #change in mortality
  
  m_param_p_i2[ , "mort_w"] <- m_param_p_i2[ , "mort_w"] * (1+inputs[parameter == "pig_mort_effect", Value])
  
  m_param_p_i2[ , "mort_s"] <- m_param_p_i2[ , "mort_s"] * (1+inputs[parameter == "pig_mort_effect", Value])
  
  m_param_p_i2[ , "mort_r"] <- m_param_p_i2[ , "mort_r"] * (1+inputs[parameter == "pig_mort_effect", Value])
  
  #make sure the total number of infections stays constant
  for(i in 1:n.t){
    m_param_p_i2[i, "s"] <- m_param_p_i2[1, "r"] + m_param_p_i2[1, "s"] - m_param_p_i2[i, "r"]
  }
  
  m_param_p_i2[ , 1:length(state_names_a)] <- 0
  m_param_p_i2[1, 1:length(state_names_a)] <- state_i_p_i
  
  #apply the animal epi function
  scenario_animal <- "pig_ind"
  m_param_p_i2 <- f_animal_epi(m_param_p_i2, n.t, scenario_animal)  
  
  #rewards
  m_rwd_p_i2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_rwd_p_i2) <- parameter_names_a
  rownames(m_rwd_p_i2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  r_sold_p_i2 <- r_sold_p_i * (1+pig_income_effect)
  rwd_i_p_i2 <- c(0,0,0,0,r_sold_p_i2)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_p_i2[2, 1:length(state_names_a)] <- rwd_i_p_i2
  
  #discount
  for (j in 1:length(state_names_a)) {
    for (i in 3:(n.t)){
      m_rwd_p_i2[i,j] <- f_di(m_rwd_p_i2[i-1,j],dr)
    }  
  }
  
  #costs
  m_cost_p_i2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_cost_p_i2) <- parameter_names_a
  rownames(m_cost_p_i2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  ##Intervention - Pigs - Smallholder
  
  #create a parameter spreadsheet for the intervention case
  m_param_p_s2 <- m_param_p_s 
  
  #change in mortality
  
  m_param_p_s2[ , "mort_w"] <- m_param_p_s2[ , "mort_w"] * (1+inputs[parameter == "pig_mort_effect", Value])
  
  m_param_p_s2[ , "mort_s"] <- m_param_p_s2[ , "mort_s"] * (1+inputs[parameter == "pig_mort_effect", Value])
  
  m_param_p_s2[ , "mort_r"] <- m_param_p_s2[ , "mort_r"] * (1+inputs[parameter == "pig_mort_effect", Value])
  
  #make sure the total number of infections stays constant
  for(i in 1:n.t){
    m_param_p_s2[i, "s"] <- m_param_p_s2[1, "r"] + m_param_p_s2[1, "s"] - m_param_p_s2[i, "r"]
  }
  
  m_param_p_s2[ , 1:length(state_names_a)] <- 0
  m_param_p_s2[1, 1:length(state_names_a)] <- state_i_p_s
  
  #apply the animal epi function
  scenario_animal <- "pig_small"
  m_param_p_s2 <- f_animal_epi(m_param_p_s2, n.t, scenario_animal)
  
  #rewards
  m_rwd_p_s2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_rwd_p_s2) <- parameter_names_a
  rownames(m_rwd_p_s2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  r_sold_p_s2 <- (r_sold_p_s * (1+pig_income_effect)) + 
    inputs[parameter == "pig_money_saved", Value]
  rwd_i_p_s2 <- c(0,0,0,0,r_sold_p_s2)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_p_s2[2, 1:length(state_names_a)] <- rwd_i_p_s2
  
  #discount
  for (j in 1:length(state_names_a)) {
    for (i in 3:(n.t)){
      m_rwd_p_s2[i,j] <- f_di(m_rwd_p_s2[i-1,j],dr)
    }  
  }
  
  #costs
  m_cost_p_s2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_cost_p_s2) <- parameter_names_a
  rownames(m_cost_p_s2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  
  # Results and Outputs -----------------------------------------------------
  
  #Get number of each type of farm nationally
  n_farms_chicken_small <- inputs[parameter == "n_chickens", Value] * 
    (1 - inputs[parameter == "portion_animals_ind", Value]) / 
    inputs[parameter == "n_chickens_farm_small", Value]
  
  if(!is.na(inputs[parameter == "n_chickens", Value] * 
            (inputs[parameter == "portion_animals_ind", Value]) / 
            inputs[parameter == "n_chickens_farm_ind", Value])){
    n_farms_chicken_ind <- inputs[parameter == "n_chickens", Value] * 
      (inputs[parameter == "portion_animals_ind", Value]) / 
      inputs[parameter == "n_chickens_farm_ind", Value]
  } else n_farms_chicken_ind <- 0
  
  n_farms_pig_small <- inputs[parameter == "n_pigs", Value] * 
    (1 - inputs[parameter == "portion_animals_ind", Value]) / 
    inputs[parameter == "n_pigs_farm_small", Value]
  
  if(!is.na(inputs[parameter == "n_pigs", Value] * 
            (inputs[parameter == "portion_animals_ind", Value]) / 
            inputs[parameter == "n_pigs_farm_ind", Value])) {
    n_farms_pig_ind <- inputs[parameter == "n_pigs", Value] * 
      (inputs[parameter == "portion_animals_ind", Value]) / 
      inputs[parameter == "n_pigs_farm_ind", Value]
  } else n_farms_pig_ind <- 0
  
  #results matrix for healthcare
  results_base_h <- f_expvalue(m_param,m_cost,m_rwd)
  results_interv_h <- f_expvalue(m_param2,m_cost,m_rwd)
  
  total_results_HC<- matrix(rep(0), nrow=2, ncol=2)
  colnames(total_results_HC) <- c("Costs ($)", "QALYs")
  rownames(total_results_HC) <- c("Base Case", "Intervention")
  
  total_results_HC[1,] <- results_base_h[1,]
  total_results_HC[2,] <- results_interv_h[1,]
  
  wtp <- inputs[parameter == "wtp", Value]
  
  incr_cost_health <- (results_interv_h[1,1] - results_base_h[1,1])
  QALYs_saved <-  (results_interv_h[1,2]-results_base_h[1,2])
  NMB_health <- (QALYs_saved*wtp)-(incr_cost_health)
  
  #results matrix for productivity
  results_base_prod <- f_expvalue(m_param,m_cost_prod,m_rwd_prod)
  results_interv_prod <- f_expvalue(m_param2,m_cost_prod,m_rwd_prod)
  
  total_results_prod <- matrix(rep(0), nrow=2, ncol=2)
  colnames(total_results_prod) <- c("Productivity", "QALYs")
  rownames(total_results_prod) <- c("Base Case", "Intervention")
  
  total_results_prod[1,2] <- results_base_h[,2]
  total_results_prod[2,2] <- results_interv_h[,2]
  total_results_prod[1,1] <- results_base_prod[,2]   #will be negative
  total_results_prod[2,1] <- results_interv_prod[,2] #will be negative but hopefully closer to zero
  
  incr_cost_prod <- total_results_prod[1,1] - total_results_prod[2,1] #hopefully negative
  incr_benefit_prod <- total_results_prod[2,2] - total_results_prod[1,2] #hopefully positive
  NMB_prod <- total_results_prod[2,1] - total_results_prod[1,1] #hopefully positive
  
  #results matrix for industrial chicken farms
  results_base_c_i <- f_expvalue(m_param_c_i,m_cost_c_i,m_rwd_c_i)
  results_interv_c_i <- f_expvalue(m_param_c_i2,m_cost_c_i2,m_rwd_c_i2)
  
  total_results_c_i<- matrix(rep(0), nrow=2, ncol=2)
  colnames(total_results_c_i) <- c("Costs ($)", "Benefits ($)")
  rownames(total_results_c_i) <- c("Base Case", "Intervention")
  
  incr_cost_c_i <- (results_interv_c_i[1,1] - results_base_c_i[1,1])
  incr_benefit_c_i <-  (results_interv_c_i[1,2]-results_base_c_i[1,2])
  
  total_results_c_i[1,] <- results_base_c_i[1,] 
  total_results_c_i[2,] <- results_interv_c_i[1,] 
  
  NMB_c_i <- (incr_benefit_c_i-incr_cost_c_i) * n_farms_chicken_ind
  
  #results matrix for smallholder chicken farms
  results_base_c_s <- f_expvalue(m_param_c_s,m_cost_c_s,m_rwd_c_s)
  results_interv_c_s <- f_expvalue(m_param_c_s2,m_cost_c_s2,m_rwd_c_s2)
  
  total_results_c_s<- matrix(rep(0), nrow=2, ncol=2)
  colnames(total_results_c_s) <- c("Costs ($)", "Benefits ($)")
  rownames(total_results_c_s) <- c("Base Case", "Intervention")
  
  incr_cost_c_s <- (results_interv_c_s[1,1] - results_base_c_s[1,1])
  incr_benefit_c_s <-  (results_interv_c_s[1,2]-results_base_c_s[1,2])
  
  total_results_c_s[1,] <- results_base_c_s[1,] 
  total_results_c_s[2,] <- results_interv_c_s[1,] 
  
  NMB_c_s <- (incr_benefit_c_s-incr_cost_c_s) * n_farms_chicken_small
  
  #results matrix for industrial pig farms
  results_base_p_i <- f_expvalue(m_param_p_i,m_cost_p_i,m_rwd_p_i)
  results_interv_p_i <- f_expvalue(m_param_p_i2,m_cost_p_i2,m_rwd_p_i2)
  
  total_results_p_i<- matrix(rep(0), nrow=2, ncol=2)
  colnames(total_results_p_i) <- c("Costs ($)", "Benefits ($)")
  rownames(total_results_p_i) <- c("Base Case", "Intervention")
  
  incr_cost_p_i <- (results_interv_p_i[1,1] - results_base_p_i[1,1])
  incr_benefit_p_i <-  (results_interv_p_i[1,2]-results_base_p_i[1,2])
  
  total_results_p_i[1,] <- results_base_p_i[1,] 
  total_results_p_i[2,] <- results_interv_p_i[1,] 
  
  NMB_p_i <- (incr_benefit_p_i-incr_cost_p_i) * n_farms_pig_ind
  
  #results matrix for smallholder pig farms
  results_base_p_s <- f_expvalue(m_param_p_s,m_cost_p_s,m_rwd_p_s)
  results_interv_p_s <- f_expvalue(m_param_p_s2,m_cost_p_s2,m_rwd_p_s2)
  
  total_results_p_s<- matrix(rep(0), nrow=2, ncol=2)
  colnames(total_results_p_s) <- c("Costs ($)", "Benefits ($)")
  rownames(total_results_p_s) <- c("Base Case", "Intervention")
  
  incr_cost_p_s <- (results_interv_p_s[1,1] - results_base_p_s[1,1])
  incr_benefit_p_s <-  (results_interv_p_s[1,2]-results_base_p_s[1,2])
  
  total_results_p_s[1,] <- results_base_p_s[1,] 
  total_results_p_s[2,] <- results_interv_p_s[1,] 
  
  NMB_p_s <- (incr_benefit_p_s-incr_cost_p_s) * n_farms_pig_small
  
  GMB_discounted <- NMB_c_i + NMB_c_s + NMB_p_i + NMB_p_s + NMB_health + NMB_prod
  
  money_saved_health <- -1 * incr_cost_health
  
  valuation_QALYs <- QALYs_saved*wtp
  
  #calculate intervention cost
  
  seminar_cost <- inputs[parameter == "seminar_cost", Value]
  visit_cost <- inputs[parameter == "visit_cost", Value]
  farmers_per_seminar <- inputs[parameter == "farmers_per_seminar", Value]
  hourly_compensation <- inputs[parameter == "hourly_wage", Value]
  seminar_length <- inputs[parameter == "seminar_length", Value]
  visits_per_year <- inputs[parameter == "visits_per_year", Value]
  transport_cost <- inputs[parameter == "transport_cost", Value]
  
  group_size <- inputs[parameter == "group_size", Value]
  additional_time_per_farm <- inputs[parameter == "additional_time_per_farm", Value]
  visit_length_village <- inputs[parameter == "visit_length_village", Value]
  
  cost_per_farm <- seminar_cost/farmers_per_seminar + 
    seminar_length*hourly_compensation + transport_cost +
    (visit_cost + ((group_size - 1)*additional_time_per_farm*hourly_compensation))*(visits_per_year/group_size) +
    visit_length_village*hourly_compensation*visits_per_year
  
  cost_per_farm <- cost_per_farm / inputs[parameter == "intervention_followup_period", Value]
  
  int_cost_vector <- rep(0, n.t)
  
  int_cost_vector[1] <- cost_per_farm
  
  for(i in 2:n.t){
    int_cost_vector[i] <- f_di(int_cost_vector[i-1], dr_pgrowth)
  }
  
  intervention_cost_discounted <- sum(int_cost_vector)
  
  int_cost_pig <- intervention_cost_discounted * (n_farms_pig_ind + n_farms_pig_small)
  
  int_cost_chicken <- intervention_cost_discounted * (n_farms_chicken_ind + n_farms_chicken_small)
  
  int_cost_total <- int_cost_pig + int_cost_chicken + inputs[parameter == "admin_cost", Value]
  
  NMB <- GMB_discounted - int_cost_total

  #Final outputs
  outputs <- data.table("Net Monetary Benefit"=NMB,
                        "Value of Productivity Gained"=NMB_prod,
                        "Cost Saved for Healthcare"=money_saved_health,
                        "Value of DALYs Averted"=valuation_QALYs,
                        "Increased Profit - Pig Farms"=NMB_p_s,
                        "Increased Profit - Chicken Farms"=NMB_c_s,
                        "Implementation Cost"=int_cost_total)
  outputs
  
  return(outputs)

}