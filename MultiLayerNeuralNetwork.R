knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(tidymodels)
library(neuralnet)
library(knitr)




#####################################################################################################################################
#              Load Dataset                                                                                                         #
#####################################################################################################################################


electricity_load_data <- read_excel("C:/Users/bhast/Downloads/UoW_load.xlsx") %>%
  janitor::clean_names() %>%
  mutate( date_in_ymd=ymd(dates)) %>%
  select( -1) %>%
  select( date_in_ymd,everything())
summary(electricity_load_data)
any( is.na (electricity_load_data))
num_rows <- nrow(electricity_load_data)


############################# Print the number of rows #############################################################################

print(num_rows)
electricity_load_data



####################################################################################################################################
#              creating input vectors Up to t-7 using AR approach                                                                  #
#                                                                                                                                  #
####################################################################################################################################


electricity_load_dataFul_AR =electricity_load_data %>%
  mutate(previous1DaySet_A=lag(electricity_load_data$x11_00,1),
         previous1DaySet_B=lag(electricity_load_data$x11_00,1),
         previous2DaySet_B=lag(electricity_load_data$x11_00,2),
         previous1DaySet_C=lag(electricity_load_data$x11_00,1),
         previous2DaySet_C=lag(electricity_load_data$x11_00,2),
         previous3DaySet_C=lag(electricity_load_data$x11_00,3),
         previous1DaySet_D=lag(electricity_load_data$x11_00,1),
         previous2DaySet_D=lag(electricity_load_data$x11_00,2),
         previous3DaySet_D=lag(electricity_load_data$x11_00,3),
         previous4DaySet_D=lag(electricity_load_data$x11_00,4),
         previous1DaySet_E=lag(electricity_load_data$x11_00,1),
         previous2DaySet_E=lag(electricity_load_data$x11_00,2),
         previous3DaySet_E=lag(electricity_load_data$x11_00,3),
         previous4DaySet_E=lag(electricity_load_data$x11_00,4),
         previous7DaySet_E=rollmean(x11_00,7, fill = NA)) %>%
  drop_na()

electricity_load_dataFul_AR
summary(electricity_load_dataFul_AR)



###################################################################################################################################
#              Plotting input vector set which used AR approach                                                                   #   
#                                                                                                                                 #
###################################################################################################################################
 


############################### Plot set A input variables of AR ##################################################################

electricity_load_dataFul_AR %>%
  pivot_longer(cols =5, names_to= "kind", values_to="rate") %>%
  ggplot(aes(date_in_ymd,rate,color= kind))+geom_line() +
  facet_wrap(~kind
  )+theme(axis.text.x=element_text(angle=45,vjust=0.5,hjust=1))+
  labs(x ="",title="Set A of input variables")+theme(
    legend.position="none")



############################## Plot set B input variables of AR  ##################################################################

electricity_load_dataFul_AR %>%
  pivot_longer(cols =c(6,7), names_to= "kind", values_to="rate") %>%
  ggplot(aes(date_in_ymd,rate,color= kind))+geom_line() +
  facet_wrap(~kind
  )+theme(axis.text.x=element_text(angle=45,vjust=0.5,hjust=1))+
  labs(x ="",title="Set B of input variables")+theme(
    legend.position="none")



############################# Plot set C input variables of AR ####################################################################

electricity_load_dataFul_AR %>%
  pivot_longer(cols =8:10, names_to= "kind", values_to="rate") %>%
  ggplot(aes(date_in_ymd,rate,color= kind))+geom_line() +
  facet_wrap(~kind
  )+theme(axis.text.x=element_text(angle=45,vjust=0.5,hjust=1))+
  labs(x ="",title="Set C of input variables")+theme(
    legend.position="none")



############################# Plot set D input variables of AR ####################################################################

electricity_load_dataFul_AR %>%
  pivot_longer(cols =11:14, names_to= "kind", values_to="rate") %>%
  ggplot(aes(date_in_ymd,rate,color= kind))+geom_line() +
  facet_wrap(~kind
  )+theme(axis.text.x=element_text(angle=45,vjust=0.5,hjust=1))+
  labs(x ="",title="Set D of input variables")+theme(
    legend.position="none")



############################# Plot set E input variables of AR ####################################################################

electricity_load_dataFul_AR %>%
  pivot_longer(cols =15:19, names_to= "kind", values_to="rate") %>%
  ggplot(aes(date_in_ymd,rate,color= kind))+geom_line() +
  facet_wrap(~kind
  )+theme(axis.text.x=element_text(angle=45,vjust=0.5,hjust=1))+
  labs(x ="",title="Set E of input variables")+theme(
    legend.position="none")





##################################################################################################################################
#              Define the lagged input vectors Which used AR Approach                                                            #
#                                                                                                                                #
##################################################################################################################################


input_vec_A_AR <- electricity_load_dataFul_AR %>% select(previous1DaySet_A)

input_vec_B_AR <- electricity_load_dataFul_AR %>% select(previous1DaySet_B,
                                                         previous2DaySet_B)

input_vec_C_AR <- electricity_load_dataFul_AR %>% select(previous1DaySet_C,
                                                         previous2DaySet_C,
                                                         previous2DaySet_C)

input_vec_D_AR <- electricity_load_dataFul_AR %>% select(previous1DaySet_D,
                                                         previous2DaySet_D, 
                                                         previous3DaySet_D, 
                                                         previous4DaySet_D)

input_vec_E_AR <- electricity_load_dataFul_AR %>% select(previous1DaySet_E, 
                                                         previous2DaySet_E, 
                                                         previous3DaySet_E, 
                                                         previous4DaySet_E,
                                                         previous7DaySet_E)



##################################################################################################################################
#              Define the output vector  Which used AR Approach                                                                  #
##################################################################################################################################

output_vec_AR <- electricity_load_dataFul_AR$x11_00



##################################################################################################################################
#   Combine the input and output vectors to create the input/output matrix  Which used AR Approach                               #
##################################################################################################################################



#################################### io_matrix_A #################################################################################

io_matrix_A_AR <- cbind(input_vec_A_AR, output_vec_AR)
head(io_matrix_A_AR, 10)



#################################### io_matrix_B #################################################################################

io_matrix_B_AR <- cbind(input_vec_B_AR, output_vec_AR)
head(io_matrix_B_AR, 10)



#################################### io_matrix_C #################################################################################

io_matrix_C_AR <- cbind(input_vec_C_AR, output_vec_AR)
head(io_matrix_C_AR, 10)



########################## io_matrix_D ###########################################################################################
io_matrix_D_AR <- cbind(input_vec_D_AR, output_vec_AR)
head(io_matrix_D_AR, 10)



########################## io_matrix_E ###########################################################################################

io_matrix_E_AR <- cbind(input_vec_E_AR, output_vec_AR)
head(io_matrix_E_AR, 10)




##################################################################################################################################
#              creating input vectors Up to t-7 using NARX approach                                                              #
##################################################################################################################################


electricity_load_dataFul_NARX <- electricity_load_data %>% 
  mutate(previous1DaySet_A_x09_00 = lag(electricity_load_data$x09_00,1),
         previous1DaySet_A_x10_00 = lag(electricity_load_data$x10_00,1),
         previous1DaySet_A_x11_00 = lag(electricity_load_data$x11_00,1),
         previous1DaySet_B_x09_00 = lag(electricity_load_data$x09_00,1),
         previous1DaySet_B_x10_00 = lag(electricity_load_data$x10_00,1),
         previous1DaySet_B_x11_00 = lag(electricity_load_data$x11_00,1),
         previous2DaySet_B_x09_00 = lag(electricity_load_data$x09_00,2),
         previous2DaySet_B_x10_00 = lag(electricity_load_data$x10_00,2),
         previous2DaySet_B_x11_00 = lag(electricity_load_data$x11_00,2),
         previous1DaySet_C_x09_00 = lag(electricity_load_data$x09_00,1),
         previous1DaySet_C_x10_00 = lag(electricity_load_data$x10_00,1),
         previous1DaySet_C_x11_00 = lag(electricity_load_data$x11_00,1),
         previous2DaySet_C_x09_00 = lag(electricity_load_data$x09_00,2),
         previous2DaySet_C_x10_00 = lag(electricity_load_data$x10_00,2),
         previous2DaySet_C_x11_00 = lag(electricity_load_data$x11_00,2),
         previous3DaySet_C_x09_00 = lag(electricity_load_data$x09_00,3),
         previous3DaySet_C_x10_00 = lag(electricity_load_data$x10_00,3),
         previous3DaySet_C_x11_00 = lag(electricity_load_data$x11_00,3),
         previous1DaySet_D_x09_00 = lag(electricity_load_data$x09_00,1),
         previous1DaySet_D_x10_00 = lag(electricity_load_data$x10_00,1),
         previous1DaySet_D_x11_00 = lag(electricity_load_data$x11_00,1),
         previous2DaySet_D_x09_00 = lag(electricity_load_data$x09_00,2),
         previous2DaySet_D_x10_00 = lag(electricity_load_data$x10_00,2),
         previous2DaySet_D_x11_00 = lag(electricity_load_data$x11_00,2),
         previous3DaySet_D_x09_00 = lag(electricity_load_data$x09_00,3),
         previous3DaySet_D_x10_00 = lag(electricity_load_data$x10_00,3),
         previous3DaySet_D_x11_00 = lag(electricity_load_data$x11_00,3),
         previous4DaySet_D_x09_00 = lag(electricity_load_data$x09_00,4),
         previous4DaySet_D_x10_00 = lag(electricity_load_data$x10_00,4),
         previous4DaySet_D_x11_00 = lag(electricity_load_data$x11_00,4),
         previous1DaySet_E_x09_00 = lag(electricity_load_data$x09_00,1),
         previous1DaySet_E_x10_00 = lag(electricity_load_data$x10_00,1),
         previous1DaySet_E_x11_00 = lag(electricity_load_data$x11_00,1),
         previous2DaySet_E_x09_00 = lag(electricity_load_data$x09_00,2),
         previous2DaySet_E_x10_00 = lag(electricity_load_data$x10_00,2),
         previous2DaySet_E_x11_00 = lag(electricity_load_data$x11_00,2),
         previous3DaySet_E_x09_00 = lag(electricity_load_data$x09_00,3),
         previous3DaySet_E_x10_00 = lag(electricity_load_data$x10_00,3),
         previous3DaySet_E_x11_00 = lag(electricity_load_data$x11_00,3),
         previous4DaySet_E_x09_00 = lag(electricity_load_data$x09_00,4),
         previous4DaySet_E_x10_00 = lag(electricity_load_data$x10_00,4),
         previous4DaySet_E_x11_00 = lag(electricity_load_data$x11_00,4),
         previous7DaySet_E_x09_00 = rollmean(x09_00,7, fill = NA),
         previous7DaySet_E_x10_00 = rollmean(x10_00,7, fill = NA),
         previous7DaySet_E_x11_00 = rollmean(x11_00,7, fill = NA),
         ) %>%
  drop_na()

electricity_load_dataFul_NARX
summary(electricity_load_dataFul_NARX)



##################################################################################################################################
#                     Plotting input vector set Which is NARX                                                                    #
##################################################################################################################################



############################# Plot set A input variables of NARX #################################################################

electricity_load_dataFul_NARX %>%
  pivot_longer(cols = 5:7, names_to= "kind", values_to="rate") %>%
  ggplot(aes(date_in_ymd,rate,color= kind))+geom_line() +
  facet_wrap(~kind
  )+theme(axis.text.x=element_text(angle=45,vjust=0.5,hjust=1))+
  labs(x ="",title="Set A of input variables")+theme(
    legend.position="none")



############################# Plot set B input variables of NARX #################################################################

electricity_load_dataFul_NARX %>%
  pivot_longer(cols = 8:13, names_to= "kind", values_to="rate") %>%
  ggplot(aes(date_in_ymd,rate,color= kind))+geom_line() +
  facet_wrap(~kind
  )+theme(axis.text.x=element_text(angle=45,vjust=0.5,hjust=1))+
  labs(x ="",title="Set B of input variables")+theme(
    legend.position="none")



############################## Plot set C input variables of NARX ################################################################

electricity_load_dataFul_NARX %>%
  pivot_longer(cols = 14:22, names_to= "kind", values_to="rate") %>%
  ggplot(aes(date_in_ymd,rate,color= kind))+geom_line() +
  facet_wrap(~kind
  )+theme(axis.text.x=element_text(angle=45,vjust=0.5,hjust=1))+
  labs(x ="",title="Set C of input variables")+theme(
    legend.position="none")



############################## Plot set D input variables of NARX ################################################################

electricity_load_dataFul_NARX %>%
  pivot_longer(cols = 23:34, names_to= "kind", values_to="rate") %>%
  ggplot(aes(date_in_ymd,rate,color= kind))+geom_line() +
  facet_wrap(~kind
  )+theme(axis.text.x=element_text(angle=45,vjust=0.5,hjust=1))+
  labs(x ="",title="Set D of input variables")+theme(
    legend.position="none")



############################## Plot set E input variables of NARX ################################################################

electricity_load_dataFul_NARX %>%
  pivot_longer(cols = 35:49, names_to= "kind", values_to="rate") %>%
  ggplot(aes(date_in_ymd,rate,color= kind))+geom_line() +
  facet_wrap(~kind
  )+theme(axis.text.x=element_text(angle=45,vjust=0.5,hjust=1))+
  labs(x ="",title="Set E of input variables")+theme(
    legend.position="none")





##################################################################################################################################
#              Define the lagged input vectors Which used NARX Approach                                                          #
#                                                                                                                                #
##################################################################################################################################

input_vec_A_NARX <- electricity_load_dataFul_NARX %>% select(previous1DaySet_A_x09_00,
                                                             previous1DaySet_A_x10_00,
                                                             previous1DaySet_A_x11_00)

input_vec_B_NARX <- electricity_load_dataFul_NARX %>% select(previous1DaySet_B_x09_00,
                                                             previous1DaySet_B_x10_00,
                                                             previous1DaySet_B_x11_00,
                                                             previous2DaySet_B_x09_00,
                                                             previous2DaySet_B_x10_00,
                                                             previous2DaySet_B_x11_00)

input_vec_C_NARX <- electricity_load_dataFul_NARX %>% select(previous1DaySet_C_x09_00,
                                                             previous1DaySet_C_x10_00,
                                                             previous1DaySet_C_x11_00,
                                                             previous2DaySet_C_x09_00,
                                                             previous2DaySet_C_x10_00,
                                                             previous2DaySet_C_x11_00,
                                                             previous3DaySet_C_x09_00,
                                                             previous3DaySet_C_x10_00,
                                                             previous3DaySet_C_x11_00)

input_vec_D_NARX <- electricity_load_dataFul_NARX %>% select(previous1DaySet_D_x09_00,
                                                             previous1DaySet_D_x10_00,
                                                             previous1DaySet_D_x11_00,
                                                             previous2DaySet_D_x09_00,
                                                             previous2DaySet_D_x10_00,
                                                             previous2DaySet_D_x11_00,
                                                             previous3DaySet_D_x09_00,
                                                             previous3DaySet_D_x10_00,
                                                             previous3DaySet_D_x11_00,
                                                             previous4DaySet_D_x09_00,
                                                             previous4DaySet_D_x10_00,
                                                             previous4DaySet_D_x11_00)

input_vec_E_NARX <- electricity_load_dataFul_NARX %>% select(previous1DaySet_E_x09_00,
                                                             previous1DaySet_E_x10_00,
                                                             previous1DaySet_E_x11_00,
                                                             previous2DaySet_E_x09_00,
                                                             previous2DaySet_E_x10_00,
                                                             previous2DaySet_E_x11_00,
                                                             previous3DaySet_E_x09_00,
                                                             previous3DaySet_E_x10_00,
                                                             previous3DaySet_E_x11_00,
                                                             previous4DaySet_E_x09_00,
                                                             previous4DaySet_E_x10_00,
                                                             previous4DaySet_E_x11_00,
                                                             previous7DaySet_E_x09_00,
                                                             previous7DaySet_E_x10_00,
                                                             previous7DaySet_E_x11_00,)


##################################################################################################################################
#              Define the output vector  Which used AR Approach                                                                  #
##################################################################################################################################

output_vec_09_NARX <- electricity_load_dataFul_NARX$x09_00
output_vec_10_NARX <- electricity_load_dataFul_NARX$x10_00
output_vec_11_NARX <- electricity_load_dataFul_NARX$x11_00



##################################################################################################################################
#  Combine the input and output vectors to create the input/output matrix  Which used NARX Approach                              #
##################################################################################################################################


########################## io_matrix_A ###########################################################################################

io_matrix_A_NARX <- cbind(input_vec_A_NARX, output_vec_09_NARX,output_vec_10_NARX,output_vec_11_NARX)
head(io_matrix_A_NARX, 10)



########################## io_matrix_B ###########################################################################################
io_matrix_B_NARX <- cbind(input_vec_B_NARX, output_vec_09_NARX,output_vec_10_NARX,output_vec_11_NARX)
head(io_matrix_B_NARX, 10)



########################## io_matrix_C ###########################################################################################
io_matrix_C_NARX <- cbind(input_vec_C_NARX, output_vec_09_NARX,output_vec_10_NARX,output_vec_11_NARX)
head(io_matrix_C_NARX, 10)



########################## io_matrix_D ###########################################################################################
io_matrix_D_NARX <- cbind(input_vec_D_NARX, output_vec_09_NARX,output_vec_10_NARX,output_vec_11_NARX)
head(io_matrix_D_NARX, 10)



########################## io_matrix_E ###########################################################################################
io_matrix_E_NARX <- cbind(input_vec_E_NARX, output_vec_09_NARX,output_vec_10_NARX,output_vec_11_NARX)
head(io_matrix_E_NARX, 10)




##################################################################################################################################
#          We can create a function to normalize the data from 0 to 1 for AR Approach                                            #
##################################################################################################################################

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }


################################# All the variables are normalized ###############################################################

normalized_electricity_load_AR = electricity_load_dataFul_AR %>%
  mutate(across(2:14, ~normalize(.x)))


##################################  Look at the data that has been normalized ####################################################
summary(normalized_electricity_load_AR)

set.seed(123)

normalized_electricity_load_AR_train <- normalized_electricity_load_AR[1:430,]
normalized_electricity_load_AR_test <- normalized_electricity_load_AR[431:500,]

summary(normalized_electricity_load_AR_train)
summary(normalized_electricity_load_AR_test)



################################# We can create a function to unnormalize the data ###############################################
unnormalize <- function(x, min, max) {
  return( (max - min)*x + min ) }


################################## Get the min and max of the original training values ###########################################

electricity_load_AR_min_train <- min(electricity_load_dataFul_AR[1:430,4])
electricity_load_AR_max_train <- max(electricity_load_dataFul_AR[1:430,4])


################################## Get the min and max of the original testing values ############################################

electricity_load_AR_min_test <- min(electricity_load_dataFul_AR[431:500,4])
electricity_load_AR_max_test <- max(electricity_load_dataFul_AR[431:500,4])


################################## Check the range of the min and max of the training dataset ####################################

electricity_load_AR_min_test
electricity_load_AR_min_train
electricity_load_AR_max_test
electricity_load_AR_max_train

relevant_pred_stat <- function(true_value, predicted_value, model_kind) {
  rbind((tibble(truth = true_value, prediction = predicted_value) %>%
           metrics(truth,prediction) %>% mutate(type = model_kind))
        ,(tibble(truth = true_value, prediction = predicted_value) %>%
            mape(truth,prediction) %>% mutate(type = model_kind)))
}



#################################################################################################################################
#                    function setup that creates 1 layer model                                                                  #
#################################################################################################################################

model_one_hidden_layer = function(set_type,hidden,learning_rate,acc_fnn) {
  if(set_type == "A"){
    nn_model_true = neuralnet(x11_00~previous1DaySet_A,
                              data=normalized_electricity_load_AR_train, hidden=c(hidden), learningrate = learning_rate,
                              act.fct = acc_fnn, linear.output=FALSE)
  }else if(set_type == "B"){
    nn_model_true = neuralnet(x11_00~previous1DaySet_B + previous2DaySet_B,
                              data=normalized_electricity_load_AR_train, hidden=c(hidden), learningrate = learning_rate,
                              act.fct = acc_fnn, linear.output=FALSE)
  }else if(set_type == "C"){
    nn_model_true = neuralnet(x11_00~previous1DaySet_C + previous2DaySet_C + previous3DaySet_C,
                              data=normalized_electricity_load_AR_train, hidden=c(hidden), learningrate = learning_rate,
                              act.fct = acc_fnn, linear.output=FALSE)
  }else if(set_type == "D"){
    nn_model_true = neuralnet(x11_00~previous1DaySet_D + previous2DaySet_D + previous3DaySet_D + previous4DaySet_D,
                              data=normalized_electricity_load_AR_train, hidden=c(hidden), learningrate = learning_rate,
                              act.fct = acc_fnn, linear.output=FALSE)
  }else if(set_type == "E"){
    nn_model_true = neuralnet(x11_00~previous1DaySet_E + previous2DaySet_E  + previous3DaySet_E + previous4DaySet_E +previous7DaySet_E,
                              data=normalized_electricity_load_AR_train, hidden=c(hidden), learningrate = learning_rate,
                              act.fct = acc_fnn, linear.output=FALSE)
  }
  train_results = compute(nn_model_true,normalized_electricity_load_AR_test[,4:19])
  truthcol = electricity_load_dataFul_AR[431:500,4]$x11_00
  predcol = unnormalize(train_results$net.result,electricity_load_AR_min_train, electricity_load_AR_max_train)[,1]
  relevant_pred_stat(truthcol,predcol, "One Hidden Layers") %>%
    mutate(hidden_layers = hidden, input_set = paste(set_type)) %>%
    filter(.metric != "rsq")
}

model_one_hidden_layer


#################################################################################################################################
#          Function to display results table with best 10 results                                                               #
#################################################################################################################################

call_kable = function(results_hidden_layers){
  set_models_layers = results_hidden_layers %>%
    select(-estimator) %>%
    pivot_wider(names_from = metric, values_from = estimate) %>%
    arrange(rmse)
  kable(set_models_layers[1:10,])
}


#################################################################################################################################
#                   creation of different models with one hidden layer                                                          #
#                                                                                                                               #
#################################################################################################################################


#################################################################################################################################
#      creation of NN with one hidden layer with logistic activation function to set A                                          # 
#################################################################################################################################

results_one_hidden_layers_logistic_4_A = bind_rows(
  lapply(1:10, function(n) {
    model_one_hidden_layer("A",n,0.1,"logistic")
  })) %>%
  janitor::clean_names()


############## save the stat indices to a dataframe #############################################################################

call_kable(results_one_hidden_layers_logistic_4_A)



#################################################################################################################################
#      creation of NN with one hidden layer with logistic activation function to set B                                          # 
#################################################################################################################################

results_one_hidden_layers_logistic_4_B = bind_rows(
  lapply(1:10, function(n) {
    model_one_hidden_layer("B",n,0.1,"logistic")
  })) %>%
  janitor::clean_names()

############## save the stat indices to a dataframe #############################################################################

call_kable(results_one_hidden_layers_logistic_4_B)



#################################################################################################################################
#      creation of NN with one hidden layer with logistic activation function to set C                                          # 
#################################################################################################################################

results_one_hidden_layers_logistic_4_C = bind_rows(
  lapply(1:10, function(n) {
    model_one_hidden_layer("C",n,0.1,"logistic")
  })) %>%
  janitor::clean_names()

############## save the stat indices to a dataframe #############################################################################

call_kable(results_one_hidden_layers_logistic_4_C)



#################################################################################################################################
#      creation of NN with one hidden layer with logistic activation function to set D                                          # 
#################################################################################################################################

results_one_hidden_layers_logistic_4_D = bind_rows(
  lapply(1:10, function(n) {
    model_one_hidden_layer("D",n,0.1,"logistic")
  })) %>%
  janitor::clean_names()

############## save the stat indices to a dataframe #############################################################################

call_kable(results_one_hidden_layers_logistic_4_D)





#################################################################################################################################
#                    function setup that creates 2 layer model                                                                  #
#################################################################################################################################


two_hidden_layer_model = function(neuron_in_layer1,neuron_in_layer2,set_type) {
  neural_network_model <- ""
  if(set_type == "A"){
    neural_network_model = neuralnet(x11_00~previous1DaySet_A,
                                     data=normalized_electricity_load_AR_train, 
                                     hidden=c(
                                       neuron_in_layer1,neuron_in_layer2), linear.output=TRUE)
  }else if(set_type == "B"){
    neural_network_model = neuralnet(x11_00~previous1DaySet_B + previous2DaySet_B, 
                                     data=normalized_electricity_load_AR_train, hidden=c(
                                       neuron_in_layer1,neuron_in_layer2), linear.output=TRUE)
  }else if(set_type == "C"){
    neural_network_model = neuralnet(x11_00~previous1DaySet_C + previous2DaySet_C + previous3DaySet_C,
                                        data=normalized_electricity_load_AR_train, hidden=c(
                                         neuron_in_layer1,neuron_in_layer2), linear.output=TRUE)
  }else if(set_type == "D"){
    neural_network_model = neuralnet(x11_00~previous1DaySet_D + previous2DaySet_D + previous3DaySet_D + previous4DaySet_D,
                                      data=normalized_electricity_load_AR_train, 
                                     hidden=c(
                                       neuron_in_layer1,neuron_in_layer2), linear.output=TRUE)
  }
  train_results = compute(neural_network_model,normalized_electricity_load_AR_test[,4:19])
  truthcol = electricity_load_dataFul_AR[431:500,4]$x11_00
  predcol = unnormalize(train_results$net.result,electricity_load_AR_min_train, electricity_load_AR_max_train)[,1]
  relevant_pred_stat(truthcol,predcol,
                            "Two Hidden Layers") %>%
    mutate(hidden_layers_variation = paste0(neuron_in_layer1, " and ",neuron_in_layer2),
           input_set = set_type) %>%
    filter(.metric != "rsq")
}



#################################################################################################################################
#                   creation of different models with Two hidden layer                                                          #
#################################################################################################################################



#################################################################################################################################
#      creation of NN with two hidden layer with logistic activation function to set A                                          # 
#################################################################################################################################

results_two_hidden_layers_logistic_4_A = bind_rows(
  lapply(1:5, function(n) {
    bind_rows( 
      lapply(1:5, function(m) { 
        two_hidden_layer_model(n,m,"A")
      })
    )
  }))%>%
  janitor::clean_names()

############## save the stat indices to a dataframe #############################################################################
call_kable(results_two_hidden_layers_logistic_4_A)



#################################################################################################################################
#      creation of NN with two hidden layer with logistic activation function to set B                                          # 
#################################################################################################################################

results_two_hidden_layers_logistic_4_B = bind_rows(
  lapply(1:5, function(n) {
    bind_rows( 
    lapply(1:5, function(m) { 
      two_hidden_layer_model(n,m,"B")
    })
    )
  }))%>%
  janitor::clean_names()

############## save the stat indices to a dataframe #############################################################################

call_kable(results_two_hidden_layers_logistic_4_B)


#################################################################################################################################
#      creation of NN with two hidden layer with logistic activation function to set C                                          # 
################################################################################################################################# ##########

results_two_hidden_layers_logistic_4_C = bind_rows(
  lapply(1:5, function(n) {
    bind_rows( 
      lapply(1:5, function(m) { 
        two_hidden_layer_model(n,m,"C")
      })
    )
  }))%>%
  janitor::clean_names()

############## save the stat indices to a dataframe #############################################################################
call_kable(results_two_hidden_layers_logistic_4_C)



#################################################################################################################################
#      creation of NN with two hidden layer with logistic activation function to set D                                          # 
#################################################################################################################################

results_two_hidden_layers_logistic_4_D = bind_rows(
  lapply(1:5, function(n) {
    bind_rows( 
      lapply(1:5, function(m) { 
        two_hidden_layer_model(n,m,"D")
      })
    )
  }))%>%
  janitor::clean_names()

############## save the stat indices to a dataframe #############################################################################
call_kable(results_two_hidden_layers_logistic_4_D)





#################################################################################################################################
#                     Final results                                                                                             # 
#################################################################################################################################

nn_model_final = neuralnet(x11_00~previous1DaySet_D + previous2DaySet_D + previous3DaySet_D + previous4DaySet_D,
                             data=normalized_electricity_load_AR_train, hidden=c(5,4),
                           learningrate = 0.1, act.fct = "logistic", linear.output=FALSE)
plot(nn_model_final)






                   ########################### CReating models using NARX ############################ 


##################################################################################################################################
#          We can create a function to normalize the data from 0 to 1 for NARX Approach 
#
##################################################################################################################################

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }


################################# All the variables are normalized ###############################################################

normalized_electricity_load_NARX = electricity_load_dataFul_NARX %>%
  mutate(across(2:14, ~normalize(.x)))


##################################  Look at the data that has been normalized ####################################################
summary(normalized_electricity_load_NARX)

set.seed(123)

normalized_electricity_load_NARX_train <- normalized_electricity_load_NARX[1:430,]
normalized_electricity_load_NARX_test <- normalized_electricity_load_NARX[431:500,]

summary(normalized_electricity_load_NARX_train)
summary(normalized_electricity_load_NARX_test)



################################# We can create a function to unnormalize the data ###############################################
unnormalize <- function(x, min, max) {
  return( (max - min)*x + min ) }


################################## Get the min and max of the original training values ###########################################

electricity_load_NARX_min_train <- min(electricity_load_dataFul_NARX[1:430,4])
electricity_load_NARX_max_train <- max(electricity_load_dataFul_NARX[1:430,4])


################################## Get the min and max of the original testing values ############################################

electricity_load_NARX_min_test <- min(electricity_load_dataFul_NARX[431:500,4])
electricity_load_NARX_max_test <- max(electricity_load_dataFul_NARX[431:500,4])


################################## Check the range of the min and max of the training dataset ####################################

electricity_load_NARX_min_test
electricity_load_NARX_min_train
electricity_load_NARX_max_test
electricity_load_NARX_max_train

relevant_pred_stat <- function(true_value, predicted_value, model_kind) {
  rbind((tibble(truth = true_value, prediction = predicted_value) %>%
           metrics(truth,prediction) %>% mutate(type = model_kind))
        ,(tibble(truth = true_value, prediction = predicted_value) %>%
            mape(truth,prediction) %>% mutate(type = model_kind)))
}



#################################################################################################################################
#                    function setup that creates 1 layer model using NARX                                                                 #
#################################################################################################################################

model_one_hidden_layer_NARX = function(set_type,hidden,learning_rate,acc_fnn) {
  if(set_type == "A"){
    nn_model_true = neuralnet(x11_00~previous1DaySet_A_x09_00 + previous1DaySet_A_x10_00 + previous1DaySet_A_x11_00,
                              data=normalized_electricity_load_NARX_train, hidden=c(hidden), learningrate = learning_rate,
                              act.fct = acc_fnn, linear.output=FALSE)
  }else if(set_type == "B"){
    nn_model_true = neuralnet(x11_00~previous1DaySet_B_x09_00 +previous1DaySet_B_x10_00+previous1DaySet_B_x11_00+
                                previous2DaySet_B_x09_00+ previous2DaySet_B_x10_00 + previous2DaySet_B_x11_00,
                              data=normalized_electricity_load_NARX_train, hidden=c(hidden), learningrate = learning_rate,
                              act.fct = acc_fnn, linear.output=FALSE)
  }else if(set_type == "C"){
    nn_model_true = neuralnet(x11_00~previous1DaySet_C_x09_00 + previous1DaySet_C_x10_00 + previous1DaySet_C_x11_00+
                                previous2DaySet_C_x09_00 + previous2DaySet_C_x10_00 +previous2DaySet_C_x11_00+
                                previous3DaySet_C_x09_00 + previous3DaySet_C_x10_00 + previous3DaySet_C_x11_00,
                              data=normalized_electricity_load_NARX_train, hidden=c(hidden), learningrate = learning_rate,
                              act.fct = acc_fnn, linear.output=FALSE)
  }
  train_results = compute(nn_model_true,normalized_electricity_load_NARX_test[,4:22])
  truthcol = electricity_load_dataFul_NARX[431:500,4]$x11_00
  predcol = unnormalize(train_results$net.result,electricity_load_NARX_min_train, electricity_load_NARX_max_train)[,1]
  relevant_pred_stat(truthcol,predcol, "One Hidden Layers") %>%
    mutate(hidden_layers = hidden, input_set = paste(set_type)) %>%
    filter(.metric != "rsq")
}

model_one_hidden_layer




#################################################################################################################################
#                   creation of different models with one hidden layer using NARX                                                          #
#                                                                                                                               #
#################################################################################################################################


#################################################################################################################################
#      creation of NN with one hidden layer with logistic activation function to set A using NARX                                         # 
#################################################################################################################################

results_one_hidden_layers_logistic_4_A_NARX = bind_rows(
  lapply(1:10, function(n) {
    model_one_hidden_layer_NARX("A",n,0.1,"logistic")
  })) %>%
  janitor::clean_names()


############## save the stat indices to a dataframe #############################################################################

call_kable(results_one_hidden_layers_logistic_4_A_NARX)



#################################################################################################################################
#      creation of NN with one hidden layer with logistic activation function to set B using NARX                                         # 
#################################################################################################################################

results_one_hidden_layers_logistic_4_B_NARX = bind_rows(
  lapply(1:10, function(n) {
    model_one_hidden_layer_NARX("B",n,0.1,"logistic")
  })) %>%
  janitor::clean_names()

############## save the stat indices to a dataframe #############################################################################

call_kable(results_one_hidden_layers_logistic_4_B_NARX)



#################################################################################################################################
#      creation of NN with one hidden layer with logistic activation function to set B using NARX                                         # 
#################################################################################################################################

results_one_hidden_layers_logistic_4_B_NARX = bind_rows(
  lapply(1:10, function(n) {
    model_one_hidden_layer_NARX("B",n,0.1,"logistic")
  })) %>%
  janitor::clean_names()

############## save the stat indices to a dataframe #############################################################################

call_kable(results_one_hidden_layers_logistic_4_B_NARX)


#################################################################################################################################
#                    function setup that creates 2 layer model  using NARX                                                      #
#################################################################################################################################


two_hidden_layer_model_NARX = function(neuron_in_layer1,neuron_in_layer2,set_type) {
  neural_network_model <- ""
  if(set_type == "A"){
    nn_model_true = neuralnet(x11_00~previous1DaySet_A_x09_00 + previous1DaySet_A_x10_00 + previous1DaySet_A_x11_00,
                              data=normalized_electricity_load_NARX_train, hidden=c(neuron_in_layer1,neuron_in_layer2),linear.output=TRUE)
  }else if(set_type == "B"){
    nn_model_true = neuralnet(x11_00~previous1DaySet_B_x09_00 +previous1DaySet_B_x10_00+previous1DaySet_B_x11_00+
                                previous2DaySet_B_x09_00+ previous2DaySet_B_x10_00 + previous2DaySet_B_x11_00,
                              data=normalized_electricity_load_NARX_train, hidden=c(neuron_in_layer1,neuron_in_layer2), linear.output=TRUE)
  }else if(set_type == "C"){
    nn_model_true = neuralnet(x11_00~previous1DaySet_C_x09_00 + previous1DaySet_C_x10_00 + previous1DaySet_C_x11_00+
                                previous2DaySet_C_x09_00+previous2DaySet_C_x10_00+previous2DaySet_C_x11_00+
                                previous3DaySet_C_x09_00+previous3DaySet_C_x10_00+previous3DaySet_C_x11_00,
                              data=normalized_electricity_load_NARX_train, hidden=c(neuron_in_layer1,neuron_in_layer2),  linear.output=TRUE)
  }
  train_results = compute(neural_network_model,normalized_electricity_load_NARX_test[,4:22])
  truthcol = electricity_load_dataFul_NARX[431:500,4]$x11_00
  predcol = unnormalize(train_results$net.result,electricity_load_NARX_min_train, electricity_load_NARX_max_train)[,1]
  relevant_pred_stat(truthcol,predcol,
                     "Two Hidden Layers") %>%
    mutate(hidden_layers_variation = paste0(neuron_in_layer1, " and ",neuron_in_layer2),
           input_set = set_type) %>%
    filter(.metric != "rsq")
}




#################################################################################################################################
#                     Model training                                                                                           # 
#################################################################################################################################


train_results_final = compute(nn_model_final,normalized_electricity_load_AR_test[,2:5])
test_original = electricity_load_dataFul_AR[431:500,4]$x11_00
prediction <- unnormalize(train_results_final$net.result,electricity_load_AR_min_train, electricity_load_AR_max_train)

plot(test_original ,prediction, col='blue',main='Real values vs Predicted values',pch=10,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend = 'predictions',pch = 10,col='blue',bty='n')


final_result<-cbind(test_original ,prediction)
colnames(final_result)<-c("actual","prediction")
final_result

library(Metrics)

################################### calculate root means sequre error between two vectors #########################################

rmse(test_original[50],prediction[50])

################################### calculate average absolute difference between two numeric vectors #############################

mae(test_original[50],prediction[50])

################################## average absolute percent difference between two numeric vectors ################################

mape(test_original[50],prediction[50])



