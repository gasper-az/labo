rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("rpart")
require("parallel")


setwd("C:\\uba\\dmeyf\\") 

file <- "./exp/HT3210/HT322.txt"

dataset.log <- fread(file)

setorderv(dataset.log, cols = c("ganancia", "iteracion"), order = -1)
dataset.log[1:10,]