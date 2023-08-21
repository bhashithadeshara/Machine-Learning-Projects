library(tidyverse)
library(readxl)
library(NbClust)
library(knitr)
library(tidymodels)
library(flexclust)
library(funtimes)
library(factoextra)
library(caret)
library(cluster)

#####################################################################################################################
#                                Load White wine Data set                                                           #
#####################################################################################################################

Whitewine_original <- read_excel("C:/Users/bhast/Downloads/Whitewine_v2.xlsx") %>%
  janitor::clean_names() %>%
  mutate(class = as_factor(quality))




######################### Get a birds eye view of how the dataset looks like ########################################
summary(Whitewine_original)





#####################################################################################################################
#                                Outlier Detection                                                                  #
#####################################################################################################################



############################  Detecting outliers in quality = 5 #####################################################
Whitewine_original %>%
  pivot_longer(1:11,names_to = "labels") %>%
  filter(class == "5") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for quality: 5")




###########################  Detecting outliers in quality = 6 #####################################################
Whitewine_original %>%
  pivot_longer(1:11,names_to = "labels") %>%
  filter(class == "6") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class: 6")




###########################  Detecting outliers in quality = 7 #####################################################
Whitewine_original %>%
  pivot_longer(1:11,names_to = "labels") %>%
  filter(class == "7") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class: 7")




#############################Detecting outliers in quality = 8 #####################################################
Whitewine_original %>%
  
  pivot_longer(1:11,names_to = "labels") %>%
  filter(class == "8") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class: 8")





####################################################################################################################
#                           Removing Outlier                                                                       #
####################################################################################################################


quality_5 = Whitewine_original %>%
  filter(class == "5") %>%
  mutate(across(1:11, ~squish(.x, quantile(.x, c(.05, .95)))))

quality_6 = Whitewine_original %>%
  filter(class == "6") %>%
  mutate(across(1:11, ~squish(.x, quantile(.x, c(.05, .95)))))

quality_7 = Whitewine_original %>%
  filter(class == "7") %>%
  mutate(across(1:11, ~squish(.x, quantile(.x, c(.05, .95)))))

quality_8 = Whitewine_original %>%
  filter(class == "8") %>%
  mutate(across(1:11, ~squish(.x, quantile(.x, c(.05, .95)))))







####################################################################################################################
#               Print combinedWhitewineDataset After Removing Outliers                                             #
####################################################################################################################

combinedWhitewineDataset = bind_rows(list(quality_5,quality_6,quality_7,quality_8))
print(combinedWhitewineDataset)






####################################################################################################################
#               Ploting combinedWhitewineDataset After Removing Outliers                                           #
####################################################################################################################



################## After removing outliers in quality = 5 ##########################################################

combinedWhitewineDataset %>%
  pivot_longer(1:11,names_to = "labels") %>%
  filter(class == "5") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Transformed Outliers class: 5")




################## After removing outliers in quality = 6 ##########################################################

combinedWhitewineDataset %>%
  pivot_longer(1:11,names_to = "labels") %>%
  filter(class == "6") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Transformed Outliers class: 6")




################## After removing outliers in quality = 7 ##########################################################

combinedWhitewineDataset %>%
  pivot_longer(1:11,names_to = "labels") %>%
  filter(class == "7") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Transformed Outliers class: 7")




################## After removing outliers in quality = 8 ##########################################################

combinedWhitewineDataset %>%
  pivot_longer(1:11,names_to = "labels") %>%
  filter(class == "8") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Transformed Outliers class: 8")




#####################################################################################################################
#       Remove the sample name and the class name. Both of these will be remove so that only                        #
#       numerical data is left for the algorithm.                                                                   #
#####################################################################################################################

Whitewine_data_points = combinedWhitewineDataset %>%
  select(-quality, -class)




#####################################################################################################################
#       Now that we have the "Whitewine_data_points" dataset, scaling is performed                                  #
#####################################################################################################################

Whitewine_scaled = Whitewine_data_points %>%
  mutate(across(everything(), scale))




############### Analyse the dataset after scaling and removing outliers #############################################

summary(Whitewine_scaled)




#####################################################################################################################
#            Elbow Method for finding the optimal number of clusters                                                #
#####################################################################################################################


fn_kmeans_clust <- function(data,cluster_count){
  kmeans(data, cluster_count, iter.max = 300,nstart =7)
}

wcss <- vector()
arr_clusters <- 1:15

for (i in arr_clusters) wcss[i] <- sum(fn_kmeans_clust(Whitewine_scaled,i)$withinss)



######################## Plot Elbow method Output ###################################################################

plot(arr_clusters, wcss, type="b",
     main = "Elbow Mehod",
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares/WCSS")




#####################################################################################################################
#            Silhouette Method for finding the optimal number of clusters                                           #
#####################################################################################################################   




######################## Silhouette method###########################################################################

fviz_nbclust(Whitewine_scaled, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")


set.seed(123)



#####################################################################################################################
#   Finding optimal count of cluster centers using the NbClust function with Euclidean distance                     #
#####################################################################################################################

#cluster_euclidean = NbClust(Whitewine_scaled,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")




#####################################################################################################################
#   Finding optimal count of cluster centers using the NbClust function with manhattan distance                     #
#####################################################################################################################

cluster_manhattan = NbClust(Whitewine_scaled,distance="manhattan", min.nc=2,max.nc=15,method="kmeans",index="all")




#####################################################################################################################
#                         K Means  Analysis                                                                         #
#####################################################################################################################



########################  K means analysis for k = 2 ################################################################


result_c2<- kmeans(Whitewine_scaled,centers =2,nstart =25,)


######################## Plot to visualize k-means with 2 clusters ##################################################

fviz_cluster(result_c2,data=Whitewine_scaled,palette =c("#2E9FDF","#00AFBB"), 
             geom = "point", ellipse.type = "convex",ggtheme = theme_bw())



#######################  Confusion matrix for k-means with 2 clusters ##############################################

table(combinedWhitewineDataset$class ,result_c2$cluster)





#######################  K means analysis for k = 3 ################################################################

result_c3<- kmeans(Whitewine_scaled,centers =3,nstart =25,)

######################## Plot to visualize k-means with 3 clusters #################################################

fviz_cluster(result_c3,data=Whitewine_scaled,palette =c("#2E9FDF","#00AFBB", "#E7B800"),
             geom = "point", ellipse.type = "convex",ggtheme = theme_bw())


######################## Confusion matrix for k-means with 3 clusters ##############################################

table(combinedWhitewineDataset$class ,result_c3$cluster)






####################### K means analysis for k = 4  #################################################################

result_c4<- kmeans(Whitewine_scaled,centers =4,nstart =25,)


####################### Plot to visualize k-means with 4 clusters ###################################################

fviz_cluster(result_c4,data=Whitewine_scaled,palette =c("#2E9FDF","#00AFBB", "#E7B800","#e7008e"),
             geom = "point", ellipse.type = "convex",ggtheme = theme_bw())


######################## Confusion matrix for k-means with 4 clusters ###############################################

table(combinedWhitewineDataset$class ,result_c4$cluster)





#####################################################################################################################
#                                        K Means = 2 Results Analysis                                               #
#####################################################################################################################


################################# Centroids of k-means with 2 clusters #############################################

result_c2


kMeans2ClustersCenters <- result_c2$centers
kMeans2ClustersCenters

################################ Mean of centroids with 2 clusters #################################################

colMeans(kMeans2ClustersCenters)

################################ Calculating Evalation indices #####################################################

confusion_matrix_result_C2 <-table(combinedWhitewineDataset$class ,result_c2$cluster)

################################ Print the confusion-like matrix ###################################################

print(confusion_matrix_result_C2)

############################### Calculate accuracy of K Means = 2 ##################################################

accuracy_result_C2 <- sum(diag(confusion_matrix_result_C2)) / sum(confusion_matrix_result_C2)


################################ Calculate recall of K Means = 2 ###################################################

recall_result_C2 <- diag(confusion_matrix_result_C2) / rowSums(confusion_matrix_result_C2)

################################ Calculate precision ###############################################################

precision_result_C2 <- diag(confusion_matrix_result_C2) / colSums(confusion_matrix_result_C2)

############################### Print the results Calculated Evalation indices of K Means = 2 ######################

cat("Accuracy for k = 2: ", accuracy_result_C2, "\n")
cat("Recall for k = 2: ", recall_result_C2, "\n")
cat("Precision for k = 2: ", precision_result_C2, "\n")





#####################################################################################################################
#                                        K Means = 3 Results Analysis                                               #
#####################################################################################################################


################################# Centroids of k-means with 3 clusters #############################################

result_c3


kMeans3ClustersCenters <- result_c3$centers
kMeans3ClustersCenters

################################ Mean of centroids with 3 clusters #################################################

colMeans(kMeans3ClustersCenters)

################################ Calculating Evalation indices #####################################################

confusion_matrix_result_C3 <-table(combinedWhitewineDataset$class ,result_c3$cluster)

################################ Print the confusion-like matrix ###################################################

print(confusion_matrix_result_C3)

############################### Calculate accuracy of K Means = 3 ##################################################

accuracy_result_C3 <- sum(diag(confusion_matrix_result_C3)) / sum(confusion_matrix_result_C3)


################################ Calculate recall of K Means =  ###################################################

recall_result_C3 <- diag(confusion_matrix_result_C3) / rowSums(confusion_matrix_result_C3)

################################ Calculate precision ###############################################################

precision_result_C3 <- diag(confusion_matrix_result_C3) / colSums(confusion_matrix_result_C3)

############################### Print the results Calculated Evalation indices of K Means = 3 ######################

cat("Accuracy for k = 3: ", accuracy_result_C3, "\n")
cat("Recall for k = 3: ", recall_result_C3, "\n")
cat("Precision for k = 3: ", precision_result_C3, "\n")





#####################################################################################################################
#                                        K Means = 4 Results Analysis                                               #
#####################################################################################################################


################################# Centroids of k-means with 4 clusters #############################################

result_c4


kMeans4ClustersCenters <- result_c4$centers
kMeans4ClustersCenters

################################ Mean of centroids with 4 clusters #################################################

colMeans(kMeans4ClustersCenters)

################################ Calculating Evalation indices #####################################################

confusion_matrix_result_C4 <-table(combinedWhitewineDataset$class ,result_c4$cluster)

################################ Print the confusion-like matrix ###################################################

print(confusion_matrix_result_C4)

############################### Calculate accuracy of K Means = 4 ##################################################

accuracy_result_C4 <- sum(diag(confusion_matrix_result_C4)) / sum(confusion_matrix_result_C4)


################################ Calculate recall of K Means =  ####################################################

recall_result_C4 <- diag(confusion_matrix_result_C4) / rowSums(confusion_matrix_result_C4)

################################ Calculate precision ###############################################################

precision_result_C4 <- diag(confusion_matrix_result_C4) / colSums(confusion_matrix_result_C4)

############################### Print the results Calculated Evalation indices of K Means = 3 ######################

cat("Accuracy for k = 4: ", accuracy_result_C4, "\n")
cat("Recall for k = 4: ", recall_result_C4, "\n")
cat("Precision for k = 4: ", precision_result_C4, "\n")




#####################################################################################################################
#                       Applying PCA for scaled data set                                                            #
#####################################################################################################################

pca_data <- prcomp(Whitewine_scaled%>% select_if(is.numeric), scale = T)
summary(pca_data)

##################################### Creating new dataset with highest PCA values ##################################

data_transformed <- Whitewine_scaled[, c("p_h", "sulphates", "alcohol")]
summary(data_transformed)




#####################################################################################################################
#                       applying k means for data_transformed                                                       #
#####################################################################################################################

result_c2_transformed<- kmeans(data_transformed,centers =2,nstart =25,)
result_c2_transformed


######################## Plot to visualize k-means to data_transformed to with 2 cluster #############################

fviz_cluster(result_c2_transformed,data=data_transformed,palette =c("#2E9FDF","#E7B800"), 
             geom = "point", ellipse.type = "convex",ggtheme = theme_bw())


################################# Centroids of k-means of data_transformed to with 2 cluster #########################

result_c2_transformed


result_c2_transformedClustersCenters <- result_c2_transformed$centers
result_c2_transformedClustersCenters

################################ Mean of centroids data_transformed to with 2 cluster ###############################

colMeans(result_c2_transformedClustersCenters)

################################ Calculating Evalation indices #####################################################

confusion_matrix_result_c2_transformed <-table(combinedWhitewineDataset$class ,result_c2_transformed$cluster)


################################ Print the confusion-like matrix ###################################################

print(confusion_matrix_result_c2_transformed)

############################### Calculate accuracy of data_transformed to with 2 cluster ###########################

accuracyconfusion_matrix_result_c2_transformed <- sum(diag(confusion_matrix_result_c2_transformed)) / sum(confusion_matrix_result_c2_transformed)


################################ Calculate recall #################################################################

recall_confusion_matrix_result_c2_transformed <- diag(confusion_matrix_result_c2_transformed) / rowSums(confusion_matrix_result_c2_transformed)

################################ Calculate precision ###############################################################

precision_confusion_matrix_result_c2_transformed <- diag(confusion_matrix_result_c2_transformed) / colSums(confusion_matrix_result_c2_transformed)

####################Print the results Calculated Evalation indices of data_transformed to with 2 clusters ##########

cat("Accuracy for k = 2 in result_c2_transformed: ", accuracyconfusion_matrix_result_c2_transformed, "\n")
cat("Recall for k = 2 in result_c2_transformed: ", recall_confusion_matrix_result_c2_transformed, "\n")
cat("Precision for k = 2 in result_c2_transformed: ", precision_confusion_matrix_result_c2_transformed, "\n")







#####################################################################################################################
#                       performance for this “PCA-based” dataset                                                    #
#####################################################################################################################



#######################  wss for White wine scaled dataset before applying PCA ######################################
wss_result_c2 <- result_c2$withinss
wss_result_c2

######################## wss for transformed dataset ################################################################
wss_result_c2_transformed <- result_c2_transformed$withinss
wss_result_c2_transformed

####################################### bss for White wine scaled dataset ###########################################
bss_result_c2 <- result_c2$betweenss
bss_result_c2

#######################################  bss for transformed dataset ################################################
bss_result_c2_transformed <- result_c2_transformed$betweenss
bss_result_c2_transformed

