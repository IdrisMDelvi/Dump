library(lpSolveAPI) # Install Library

#Q1 - Cheese Factory (Not asked in assignment. Done for the same of completion & to verify graphical result)

cheeseFactory = make.lp(0,2) # 2 variables, 0 prior constrains
lp.control(cheeseFactory, sense="min") # Since our target is the production costs, we want to minimize it.
set.objfn(cheeseFactory, c(5,8)) # objective function - cost/kg of cheese A & B

add.constraint(cheeseFactory, c(30/60,80/60), "<=", 60) # Sheep milk constraint (/60 to reduce to per kg)
add.constraint(cheeseFactory, c(60/60,40/60), ">=", 45) # Cow milk constraint (/60 to reduce to per kg)
add.constraint(cheeseFactory, c(40/60,70/60), ">=", 50) # Goat milk constraint (/60 to reduce to per kg)
add.constraint(cheeseFactory, c(1,1), ">=", 60) # min requirement is 60kgs
set.bounds(cheeseFactory, lower=c(0,0))

constraintNames = c("Sheep Milk", "Cow Milk", "Goat Milk", "Weekly Req")
variableNames = c("CheeseA", "CheeseB")
dimnames(cheeseFactory) = list(constraintNames, variableNames)

solve(cheeseFactory)

get.objective(cheeseFactory)
get.variables(cheeseFactory)
get.constraints(cheeseFactory)

#Q2 - Cereal Production

cerealProduction = make.lp(0,12) #12 variables, 0 prior constrains
lp.control(cerealProduction, sense="max") # Since our target are the profits, we want to maximize it.
set.objfn(cerealProduction, c(2416,2464,2492,2486,1932.2,1973.2,1993.2,1977.2,2947,2985,2989,2937)) # objective function - Wt/tom of each ingredient/cereal

add.constraint(cerealProduction, c(1,0,0,0,1,0,0,0,1,0,0,0), "<=", 10) # max oats available (in tons)
add.constraint(cerealProduction, c(0,1,0,0,0,1,0,0,0,1,0,0), "<=", 5) # max apricots available (in tons)
add.constraint(cerealProduction, c(0,0,1,0,0,0,1,0,0,0,1,0), "<=", 2) # max coconuts available (in tons)
add.constraint(cerealProduction, c(0,0,0,1,0,0,0,1,0,0,0,1), "<=", 2) # max hazelnuts available (in tons)
add.constraint(cerealProduction, c(1,0,0,0,0,0,0,0,0,0,0,0), ">=", 0.8) # min boxes (in ton) of cereal A * ratio of oats in A
add.constraint(cerealProduction, c(0,1,0,0,0,0,0,0,0,0,0,0), ">=", 0.1) # min boxes (in ton) of cereal A * ratio of apricots in A
add.constraint(cerealProduction, c(0,0,1,0,0,0,0,0,0,0,0,0), ">=", 0.05) # min boxes (in ton) of cereal A * ratio of coconuts in A
add.constraint(cerealProduction, c(0,0,0,1,0,0,0,0,0,0,0,0), ">=", 0.05) # min boxes (in ton) of cereal A * ratio of hazelnuts in A
add.constraint(cerealProduction, c(0,0,0,0,1,0,0,0,0,0,0,0), ">=", 0.455) # min boxes (in ton) of cereal B * ratio of oats in B
add.constraint(cerealProduction, c(0,0,0,0,0,1,0,0,0,0,0,0), ">=", 0.14) # min boxes (in ton) of cereal B * ratio of apricots in B
add.constraint(cerealProduction, c(0,0,0,0,0,0,1,0,0,0,0,0), ">=", 0.035) # min boxes (in ton) of cereal B * ratio of coconuts in B
add.constraint(cerealProduction, c(0,0,0,0,0,0,0,1,0,0,0,0), ">=", 0.07) # min boxes (in ton) of cereal B * ratio of hazelnuts in B
add.constraint(cerealProduction, c(0,0,0,0,0,0,0,0,1,0,0,0), ">=", 0.375) # min boxes (in ton) of cereal C * ratio of oats in C
add.constraint(cerealProduction, c(0,0,0,0,0,0,0,0,0,1,0,0), ">=", 0.075) # min boxes (in ton) of cereal C * ratio of apricots in C
add.constraint(cerealProduction, c(0,0,0,0,0,0,0,0,0,0,1,0), ">=", 0.075) # min boxes (in ton) of cereal C * ratio of coconuts in C
add.constraint(cerealProduction, c(0,0,0,0,0,0,0,0,0,0,0,1), ">=", 0.225) # min boxes (in ton) of cereal C * ratio of hazelnuts in C
#add.constraint(cerealProduction, c(1,1,1,1,0,0,0,0,0,0,0,0), ">=", 1) # min boxes (in ton) of cereal A
#add.constraint(cerealProduction, c(0,0,0,0,1,1,1,1,0,0,0,0), ">=", 0.7) # min boxes (in ton) of cereal B
#add.constraint(cerealProduction, c(0,0,0,0,0,0,0,0,1,1,1,1), ">=", 0.75) # min boxes (in ton) of cereal B
set.bounds(cerealProduction, lower=c(0,0,0,0,0,0,0,0,0,0,0,0))


constraintNames = c('max oats','max apricots','max coconut','max hazelnut','min ao','min aa','min ac','min ah','min bo','min ba','min bc','min bh','min co','min ca','min cc','min ch')
variableNames = c('Xao','Xaa','Xac','Xah','Xbo','Xba','Xbc','Xbh','Xco','Xca','Xcc','Xch')
dimnames(cerealProduction) = list(constraintNames, variableNames)

solve(cerealProduction)

get.objective(cerealProduction)
get.variables(cerealProduction)
get.constraints(cerealProduction)

#Q3 - bidField

bidField = make.lp(0,6) # 6 variables, 0 prior constrains
lp.control(bidField, sense="min") # We want to minimize Company Reds Gains
set.objfn(bidField, c(0,0,0,0,0,1))

add.constraint(bidField, c(-30,20,10,0,-5,1), ">=", 0) # strategy 1
add.constraint(bidField, c(-20,-20,10,0,-5,1), ">=", 0) # strategy 2
add.constraint(bidField, c(-10,-10,-10,0,-5,1), ">=", 0) # strategy 3
add.constraint(bidField, c(0,0,0,0,-5,1), ">=", 0) # strategy 4
add.constraint(bidField, c(-5,-5,-5,-5,0,1), ">=", 0) # strategy 5
add.constraint(bidField, c(1,1,1,1,1,0), "=", 1) # sum of probabilities is 1
set.bounds(bidField, lower=c(0,0,0,0,0,-Inf)) # non-negative probabilities


constraintNames = c('Row1','Row2','Row3','Row4','Row5','Row6')
variableNames = c('b1','b2','b3','b4','b5','v')
dimnames(bidField) = list(constraintNames, variableNames)

solve(bidField)

get.objective(bidField)
get.variables(bidField)
get.constraints(bidField)

bidField # Visualize the payoff matrix
