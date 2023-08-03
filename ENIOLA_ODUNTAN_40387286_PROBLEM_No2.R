#Calling required libraries
install.packages("dplyr")
library(dplyr)

install.packages("ROI")
library(ROI)

install.packages("ROI.plugin.glpk")
library(ROI.plugin.glpk)

install.packages("ompr")
library(ompr)

install.packages("ompr.roi")
library(ompr.roi)

#-----Structure the MILP Model and add decision variables----------

chipsetLP = MIPModel() %>%
  add_variable(xF1F2, type = "integer") %>%
  add_variable(xF1F3, type = "integer") %>%
  add_variable(xF1DC1, type = "integer") %>%
  add_variable(xF1DC2, type = "integer") %>%
  add_variable(xF1P1, type = "integer") %>%
  add_variable(xF1P2, type = "integer") %>%
  add_variable(xF2F1, type = "integer") %>%
  add_variable(xF2F3, type = "integer") %>%
  add_variable(xF2DC1, type = "integer") %>%
  add_variable(xF2DC2, type = "integer") %>%
  add_variable(xF2P1, type = "integer") %>%
  add_variable(xF2P2, type = "integer") %>%
  add_variable(xF3F1, type = "integer") %>%
  add_variable(xF3F2, type = "integer") %>%
  add_variable(xF3DC1, type = "integer") %>%
  add_variable(xF3DC2, type = "integer") %>%
  add_variable(xF3P1, type = "integer") %>%
  add_variable(xF3P2, type = "integer") %>%
  add_variable(xDC1DC2, type = "integer") %>%
  add_variable(xDC1P1, type = "integer") %>%
  add_variable(xDC1P2, type = "integer") %>%
  add_variable(xDC2DC1, type = "integer") %>%
  add_variable(xDC2P1, type = "integer") %>%
  add_variable(xDC2P2, type = "integer") %>%
  add_variable(xP1P2, type = "integer") %>%
  add_variable(xP2P1, type = "integer") %>%
  
#---Set objective based on the cost of each shipment link---------------
  
  set_objective(100*xF1F2 + 60*xF1F3 + 100*xF1DC1 + 100*xF1DC2 + 400*xF1P1 + 400*xF1P2 
                + 180*xF2F1 + 180*xF2F3 + 20*xF2DC1 + 20*xF2DC2 + 160*xF2P1 + 300*xF2P2 
                + 8*xF3F1 + 160*xF3F2 + 20*xF3DC1 + 10*xF3DC2 + 200*xF3P1 + 240*xF3P2 
                + 24*xDC1DC2 + 40*xDC1P1 + 240*xDC1P2+ 16*xDC2DC1 + 40*xDC2P1 + 240*xDC2P2 
                + 20*xP1P2 + 140*xP2P1, "min") %>%
  
##---Add constraints to the objective-----------
add_constraint(xF1F2 + xF1F3 + xF1DC1 + xF1DC2 + xF1P1 +xF1P2 - xF2F1 - xF3F1  <= 400000)%>%
  add_constraint(xF2F1 + xF2F3 + xF2DC1 + xF2DC2 + xF2P1 + xF2P2 - xF1F2 - xF3F2 <= 600000)%>%
  add_constraint(xF3F1 + xF3F2 + xF3DC1 + xF3DC2  + xF3P1 + xF3P2 - xF1F3 - xF2F3 <= 200000)%>%
  add_constraint(-xF1DC1 - xF2DC1 -xF3DC1 + xDC1DC2 + xDC1P1  + xDC1P2  - xDC2DC1  == 0)%>%
  add_constraint(-xF1DC2 - xF2DC2 -xF3DC2 + xDC2DC1  + xDC2P1  + xDC2P2  - xDC1DC2  == 0)%>%
  add_constraint(xP1P2 - xDC1P1  - xF1P1  - xF2P1  - xF3P1  - xDC2P1  - xP2P1  == -400000)%>%
  add_constraint(xP2P1 - xDC1P2  - xF1P2  - xF2P2  - xF3P2  - xDC2P2  - xP1P2  == -180000)%>%
  
#--- Add additional constraint for maximum 400k capacity------
  
  add_constraint(xF1F2<=400000)%>%
  add_constraint(xF1F3<=400000)%>%
  add_constraint(xF1DC1<=400000)%>%
  add_constraint(xF1DC2<=400000)%>%
  add_constraint(xF1P1<=400000)%>%
  add_constraint(xF1P2<=400000)%>%
  add_constraint(xF2F1<=400000)%>%
  add_constraint(xF2F3<=400000)%>%
  add_constraint(xF2DC1<=400000)%>%
  add_constraint(xF2DC2<=400000)%>%
  add_constraint(xF2P1<=400000)%>%
  add_constraint(xF2P2<=400000)%>%
  add_constraint(xF3F1<=400000)%>%
  add_constraint(xF3F2<=400000)%>%
  add_constraint(xF3DC1<=400000)%>%
  add_constraint(xF3DC2<=400000)%>%
  add_constraint(xF3P1<=400000)%>%
  add_constraint(xF3P2<=400000)%>%
  add_constraint(xDC1DC2<=400000)%>%
  add_constraint(xDC1P1<=400000)%>%
  add_constraint(xDC1P2<=400000)%>%
  add_constraint(xDC2DC1<=400000)%>%
  add_constraint(xDC2P1<=400000)%>%
  add_constraint(xDC2P2<=400000)%>%
  add_constraint(xP1P2<=400000)%>%
  add_constraint(xP2P1<=400000)%>%
  
#---Set lower bounds--------
  set_bounds(xF1F2, lb = 0) %>%
  set_bounds(xF1F3, lb = 0) %>%
  set_bounds(xF1DC1, lb = 0) %>%
  set_bounds(xF1DC2, lb = 0) %>%
  set_bounds(xF1P1, lb = 0) %>%
  set_bounds(xF1P2, lb = 0) %>%
  set_bounds(xF2F1, lb = 0) %>%
  set_bounds(xF2F3, lb = 0) %>%
  set_bounds(xF2DC1, lb = 0) %>%
  set_bounds(xF2DC2, lb = 0) %>%
  set_bounds(xF2P1, lb = 0) %>%
  set_bounds(xF2P2, lb = 0) %>%
  set_bounds(xF3F1, lb = 0) %>%
  set_bounds(xF3F2, lb = 0) %>%
  set_bounds(xF3DC1, lb = 0) %>%
  set_bounds(xF3DC2, lb = 0) %>%
  set_bounds(xF3P1, lb = 0) %>%
  set_bounds(xF3P2, lb = 0) %>%
  set_bounds(xDC1DC2, lb = 0) %>%
  set_bounds(xDC1P1, lb = 0) %>%
  set_bounds(xDC1P2, lb = 0) %>%
  set_bounds(xDC2DC1, lb = 0) %>%
  set_bounds(xDC2P1, lb = 0) %>%
  set_bounds(xDC2P2, lb = 0) %>%
  set_bounds(xP1P2, lb = 0) %>%
  set_bounds(xP2P1, lb = 0) %>%
  
  #----Solve the equation of the MILP----  
  solve_model(with_ROI(solver = "glpk", verbose = TRUE))
sF1F2 = get_solution(chipsetLP, xF1F2)
sF1F3 = get_solution(chipsetLP, xF1F3)
sF1DC1 = get_solution(chipsetLP, xF1DC1)
sF1DC2 = get_solution(chipsetLP, xF1DC2)
sF1P1 = get_solution(chipsetLP, xF1P1)
sF1P2 = get_solution(chipsetLP, xF1P2)
sF2F1 = get_solution(chipsetLP, xF2F1)
sF2F3 = get_solution(chipsetLP, xF2F3)
sF2DC1 = get_solution(chipsetLP, xF2DC1)
sF2DC2 = get_solution(chipsetLP, xF2DC2)
sF2P1 = get_solution(chipsetLP, xF2P1)
sF2P2 = get_solution(chipsetLP, xF2P2)
sF3F1 = get_solution(chipsetLP, xF3F1)
sF3F2 = get_solution(chipsetLP, xF3F2)
sF3DC1 = get_solution(chipsetLP, xF3DC1)
sF3DC2 = get_solution(chipsetLP, xF3DC2)
sF3P1 = get_solution(chipsetLP, xF3P1)
sF3P2 = get_solution(chipsetLP, xF3P2)
sDC1DC2 = get_solution(chipsetLP, xDC1DC2)
sDC1P1 = get_solution(chipsetLP, xDC1P1)
sDC1P2 = get_solution(chipsetLP, xDC1P2)
sDC2DC1 = get_solution(chipsetLP, xDC2DC1)
sDC2P1 = get_solution(chipsetLP, xDC2P1)
sDC2P2 = get_solution(chipsetLP, xDC2P2)
sP1P2 = get_solution(chipsetLP, xP1P2)
sP2P1 = get_solution(chipsetLP, xP2P1)

#Print the optimal solution
cost = objective_value(chipsetLP)

cost

#Print the optimal linkage
linkTraffic = c(sF1F2, sF1F3, sF1DC1, sF1DC2, sF1P1, sF1P2, sF2F1, sF2F3, sF2DC1, 
                sF2DC2, sF2P1, sF2P2, sF3F1, sF3F2, sF3DC1, sF3DC2, sF3P1, sF3P2, 
                sDC1DC2, sDC1P1, sDC1P2, sDC2DC1, sDC2P1, sDC2P2, sP1P2, sP2P1)
linkTraffic
