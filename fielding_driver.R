
###########################################################################
###########################################################################
###                                                                     ###
###                         FIELDING EXECUTABLE                         ###
###                RUN TO BEGIN BUILDING FIELDING MODELS                ###
###                                                                     ###
###########################################################################
###########################################################################

library(data.table)
library(dplyr)
library(magrittr)
library(keras)
library(ggplot2)
library(pracma)
library(randomForest)
library(GeomMLBStadiums)
library(Lahman)
library(caret)
library(varhandle)
library(purrr)
library(stringdist)
library(scales)



source('clean_fielding_data.R')
source('find_wall.R')

#source('fielding_probability_model.R)
