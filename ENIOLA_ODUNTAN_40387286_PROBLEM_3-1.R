#===========================================
# This script solves the LP model for the 
# devising of production schedule  
#  

#-------------------------------------------
# Import the required packages. 

library(dplyr)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)
#===========================================
# The LP Model:

cLP = MIPModel() %>%
  #-------------------------------------------
# Add the Decision Variables:

add_variable(P1aj) %>%
  add_variable(P2aj) %>%
  add_variable(P1af) %>%
  add_variable(P2af) %>%
  add_variable(P1am) %>%
  add_variable(P2am) %>%
  add_variable(P1bj) %>%
  add_variable(P2bj) %>%
  add_variable(P1bf) %>%
  add_variable(P2bf) %>%
  add_variable(P1bm) %>%
  add_variable(P2bm) %>%
  
  #------------------------------------------- 
# The Objective Function:
set_objective(14*(P1aj+P2aj+P1af+P2af+P1am+P2am)+
                18*(P1bj+P2bj+P1bf+P2bf+P1bm+P2bm)-
                6.2*(P1aj+P2aj+P1af+P2af+P1am+P2am)-
                7.8*(P1bj+P2bj+P1bf+P2bf+P1bm+P2bm)-
                0.46*(P1aj+P2aj+P1af+P2af+P1am+P2am)-
                0.46*(P1bj+P2bj+P1bf+P2bf+P1bm+P2bm)-
                10*(0.3*P1aj+0.32*P2aj+0.3*P1af+0.32*P2af+0.3*P1am+0.32*P2am)-
                10*(0.24*P1bj+0.28*P2bj+0.24*P1bf+0.28*P2bf+0.24*P1bm+0.28*P2bm)-
                0.2*(P1aj+P2aj-8000)-0.2*(P1bj+P2bj-2000)-
                0.2*(P1aj+P2aj+P1af+P2af-24000)-
                0.2*(P1bj+P2bj+P1bf+P2bf-12000), "max") %>%
  #------------------------------------------
# The Constraints:
add_constraint(P1aj + P2aj >= 8000) %>% #constraint for Type A January
  add_constraint(P1bj + P2bj >=2000) %>% #constraint for Type B January
  add_constraint(P1aj + P2aj + P1af  + P2af >=24000) %>% #constraint for Type A February
  add_constraint(P1bj + P2bj + P1bf + P2bf >= 12000) %>% #constraint for Type B February
  add_constraint(P1aj + P2aj + P1af + P2af + P1am + P2am == 30000) %>% #constraint for Type A March
  add_constraint(P1bj + P2bj + P1bf + P2bf + P1bm + P2bm == 22000) %>% #constraint for Type B March
  
  ## add constraints the time per kilometer for each cable type
  
  add_constraint(0.3*P1aj + 0.24*P1bj <=  1400) %>% #constraint for Plant 1 in January
  add_constraint(0.32*P2aj + 0.28*P2bj <=  3000) %>% #constraint for Plant 2 in January
  add_constraint(0.3*P1af + 0.24*P1bf <=  600) %>% #constraint for Plant 1 in February
  add_constraint(0.32*P2af + 0.28*P2bf <=  800) %>% #constraint for Plant 2 in February
  add_constraint(0.3*P1am + 0.24*P1bm <=  2000) %>% #constraint for Plant 1 in March
  add_constraint(0.32*P2am + 0.28*P2bm <=  600) %>% #constraint for Plant 2 in March
  set_bounds(P1aj, lb =0) %>%
  set_bounds(P2aj, lb=0) %>%
  set_bounds(P1bj, lb=0) %>%
  set_bounds(P2bj, lb=0) %>%
  set_bounds(P1af, lb=0) %>%
  set_bounds(P2af, lb=0) %>%
  set_bounds(P1bf, lb=0) %>%
  set_bounds(P2bf, lb=0) %>%
  set_bounds(P1am, lb=0) %>%
  set_bounds(P2am, lb=0) %>%
  set_bounds(P1bm, lb=0) %>%
  set_bounds(P2bm, lb=0) %>%
  #-------------------------------------------
#--------Solve the Model---------------------------
solve_model(with_ROI(solver = "glpk", verbose = TRUE))

#extract the Solution:
P1aj = get_solution(cLP, P1aj)
P2aj = get_solution(cLP, P2aj)
P1bj = get_solution(cLP, P1bj)
P2bj = get_solution(cLP, P2bj)
P1af = get_solution(cLP, P1af)
P2af = get_solution(cLP, P2af)
P1bf = get_solution(cLP, P1bf)
P2bf = get_solution(cLP, P2bf)
P1am = get_solution(cLP, P1am)
P2am = get_solution(cLP, P2am)
P1bm = get_solution(cLP, P1bm)
P2bm = get_solution(cLP, P1bm)



profitmade = objective_value(cLP)
profitmade
#-------------------------------------------

production_schedule = c(P1aj, P2aj, P1bj, P2bj, P1af, P2af, P1bf, P2bf, P1am, P2am, P1bm, P2bm)
production_schedule

