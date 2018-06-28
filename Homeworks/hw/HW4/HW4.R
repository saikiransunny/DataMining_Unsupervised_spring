rm(list = ls())

library(Rgraphviz)
library(gRbase)
library(gRain)
library(ggm)
library(gRim)
library(bnlearn)

setwd("D:/Spring 2018/STA/Homeworks/hw/HW4/sputta_submission")

load("cad1.RData")
names(cad1)
#1
#a 
######
cad = list(~sex, ~smoker|sex, ~suffheartf, ~inherit|smoker, ~hyperchol|suffheartf:smoker, ~cad|inherit:hyperchol)
cad_dag = dagList(cad)

plot(cad_dag)

### checking if nodes have a link
dSep(as(cad_dag, "matrix"),"sex", "suffheartf",cond = NULL)
dSep(as(cad_dag, "matrix"),"smoker", "suffheartf",cond = NULL)
dSep(as(cad_dag, "matrix"),"inherit", "suffheartf",cond = NULL)
dSep(as(cad_dag, "matrix"),"smoker","cad",c("inherit","hyperchol"))
dSep(as(cad_dag, "matrix"),"sex", "hyperchol", c("smoker", "suffheartf"))
dSep(as(cad_dag, "matrix"), "sex", "inherit", c("smoker"))



all = xtabs(~Sex, data = cad1[,c("CAD","Hyperchol","Sex","Smoker","SuffHeartF","Inherit")])
smoker_sex = xtabs(~Smoker+Sex, data = cad1[,c("CAD","Hyperchol","Sex","Smoker","SuffHeartF","Inherit")])
inherit_smoker = xtabs(~Inherit+Smoker , data = cad1[,c("CAD","Hyperchol","Sex","Smoker","SuffHeartF","Inherit")])
cad_inherit_hyperchol = xtabs(~CAD+Inherit+Hyperchol, data = cad1[,c("CAD","Hyperchol","Sex","Smoker","SuffHeartF","Inherit")])
hyperchol_smoker_suffheartf = xtabs(~Hyperchol+Smoker+SuffHeartF, data = cad1[,c("CAD","Hyperchol","Sex","Smoker","SuffHeartF","Inherit")])
suffheartf_obj = xtabs(~SuffHeartF, data = cad1[,c("CAD","Hyperchol","Sex","Smoker","SuffHeartF","Inherit")])
######





#b
#####
plist = compileCPT(list(all, smoker_sex, inherit_smoker, cad_inherit_hyperchol, hyperchol_smoker_suffheartf, suffheartf_obj))
grain1 = grain(plist)
summary(grain1)


### Compile the network
grain1_compile = compile(grain1)
summary(grain1_compile)

### Propagate the the network
grain1_compile = propagate(grain1_compile)
summary(grain1_compile)

### before absorbing evidence and after absorbing the evidence.
grain1_compile_ev = setFinding(grain1_compile, nodes = c("Sex", "Hyperchol"), states = c("Female", "yes"))

querygrain(grain1_compile, nodes = c("SuffHeartF", "CAD"), type = "joint")
querygrain(grain1_compile_ev, nodes = c("SuffHeartF", "CAD"), type = "joint")


querygrain(grain1_compile, nodes = c("SuffHeartF", "CAD"), type = "conditional")
querygrain(grain1_compile_ev, nodes = c("SuffHeartF", "CAD"), type = "conditional")


querygrain(grain1_compile, nodes = c("SuffHeartF", "CAD"), type = "marginal")
querygrain(grain1_compile_ev, nodes = c("SuffHeartF", "CAD"), type = "marginal")
#####




#c
# Simulating 5 new obs
######
sim_find5 = simulate(grain1_compile_ev, nsim = 5)
sim_find5
predict(grain1_compile, response = c("Smoker","CAD"),newdata = sim_find5, predictors = c("Sex","SuffHeartF","Hyperchol","Inherit"),type = "class")
######



#d
#####
#Simulating 500 Data points
sim_find500 = simulate(grain1_compile_ev, nsim = 500)
yhat = predict(grain1_compile, response = c("Smoker","CAD"),newdata = sim_find500, predictors = c("Sex","SuffHeartF","Hyperchol","Inherit"),type = "class")
rm(cad, cad_dag, grain1, grain1_compile, grain1_compile_ev, plist, sim_find5, cad_inherit_hyperchol, hyperchol_smoker_suffheartf, inherit_smoker, all, suffheartf_obj, smoker_sex)
save(file="HW4env_RData")

### misclassification rate
#for smoker
tab1 = table(sim_find500$Smoker, yhat$pred$Smoker)
(1-sum(diag(tab1))/500)*100

#for CAD
tab2 = table(sim_find500$CAD, yhat$pred$CAD)
(1-sum(diag(tab2))/500)*100
######








#3
rm(list = ls(all = T))
g <- list(~A,~B,~C|A,~D|A:B,~E|B,~F|C:A:E,~G|D:E,~H|F:G)
dag <- dagList(g)
plot(dag)

####### Inquire about d-seperation ########

dSep(as(dag,"matrix"),"C","G",cond = NULL)
dSep(as(dag,"matrix"),"C","E",cond = NULL)
dSep(as(dag,"matrix"),"C","E",c("G"))
dSep(as(dag,"matrix"),"A","G",c("D","E"))
dSep(as(dag,"matrix"),"A","G",c("D"))