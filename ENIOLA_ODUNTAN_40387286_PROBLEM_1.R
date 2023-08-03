#--------------install needed packages--------------------------
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

#-----------create the model and add decision variables--------------------
nursesLP = MIPModel() %>%
  add_variable(x1, type = "integer") %>%
  add_variable(x2, type = "integer") %>%
  add_variable(x3, type = "integer") %>%
  add_variable(x4, type = "integer") %>%
  add_variable(x5, type = "integer") %>%
  add_variable(x6, type = "integer") %>%
  add_variable(x7, type = "integer") %>%
  add_variable(y1, type = "integer") %>%
  add_variable(y2, type = "integer") %>%
  add_variable(y3, type = "integer") %>%
  add_variable(y4, type = "integer") %>%
  add_variable(y5, type = "integer") %>%
  add_variable(y6, type = "integer") %>%
  add_variable(y7, type = "integer") %>%

 #-------------------set objective----------------------------- 
set_objective(1250 * x1 + 1315 * x2 + 1440 * x3 + 1440 * x4 + 1440 * x5 + 1440 * x6 +
                  1375 * x7 + 450 * y1 + 450 * y2 + 450 * y3 + 485 * y4 + 560 * y5 +
                  560 * y6 + 525 * y7, "min") %>%
  
 #------ add the constraints ( )---------------
  add_constraint(x1 + x4 + x5 + x6 + x7 - 3*y1 - 3*y6 - 3*y7 >= 0) %>%
  add_constraint(x1 + x2 + x5 + x6 + x7 - 3*y1 - 3*y2 - 3*y7 >= 0) %>%
  add_constraint(x1 + x2 + x3 + x6 + x7 - 3*y1 - 3*y2 - 3*y3 >= 0) %>%
  add_constraint(x1 + x2 + x3 + x4 + x7 - 3*y2 - 3*y3 - 3*y4 >= 0) %>%
  add_constraint(x1 + x2 + x3 + x4 + x5 - 3*y3 - 3*y4 - 3*y5 >= 0) %>%
  add_constraint(x2 + x3 + x4 + x5 + x6 - 3*y4 - 3*y5 - 3*y6 >= 0) %>%
  add_constraint(x3 + x4 + x5 + x6 + x7 - 3*y5 - 3*y6 - 3*y7 >= 0) %>%
  add_constraint(x1 + x4 + x5 + x6 + x7 + y1 + y6 + y7 >= 17) %>%
  add_constraint(x1 + x2 + x5 + x6 + x7 + y1 + y2 + y7 >= 13) %>%
  add_constraint(x1 + x2 + x3 + x6 + x7 + y1 + y2 + y3 >= 15) %>%
  add_constraint(x1 + x2 + x3 + x4 + x7 + y2 + y3 + y4 >= 19) %>%
  add_constraint(x1 + x2 + x3 + x4 + x5 + y3 + y4 + y5 >= 14) %>%
  add_constraint(x2 + x3 + x4 + x5 + x6 + y4 + y5 + y6 >= 16) %>%
  add_constraint(x3 + x4 + x5 + x6 + x7 + y5 + y6 + y7 >= 11) %>%
  
  #-----set boundary--------
  set_bounds(x1, lb = 0) %>% 
  set_bounds(x2, lb = 0) %>% 
  set_bounds(x3, lb = 0) %>% 
  set_bounds(x4, lb = 0) %>% 
  set_bounds(x5, lb = 0) %>% 
  set_bounds(x6, lb = 0) %>% 
  set_bounds(x7, lb = 0) %>% 
  set_bounds(y1, lb = 0) %>% 
  set_bounds(y2, lb = 0) %>% 
  set_bounds(y3, lb = 0) %>% 
  set_bounds(y4, lb = 0) %>% 
  set_bounds(y5, lb = 0) %>% 
  set_bounds(y6, lb = 0) %>% 
  set_bounds(y7, lb = 0) %>%
  
  
  # Solving equation of the MILP  
  solve_model(with_ROI(solver = "glpk", verbose = TRUE))
ft1 = get_solution(nursesLP, x1)
ft2 = get_solution(nursesLP, x2)
ft3 = get_solution(nursesLP, x3)
ft4 = get_solution(nursesLP, x4)
ft5 = get_solution(nursesLP, x5)
ft6 = get_solution(nursesLP, x6)
ft7 = get_solution(nursesLP, x7)


pt1 = get_solution(nursesLP, y1)
pt2 = get_solution(nursesLP, y2)
pt3 = get_solution(nursesLP, y3)
pt4 = get_solution(nursesLP, y4)
pt5 = get_solution(nursesLP, y5)
pt6 = get_solution(nursesLP, y6)
pt7 = get_solution(nursesLP, y7)

cost = objective_value(nursesLP)
cost


Nurses = c(ft1, ft2, ft3, ft4, ft5, ft6, ft7, pt1, pt2,pt3, pt4, pt5, pt6, pt7)
Nurses
