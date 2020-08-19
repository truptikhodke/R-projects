library(data.table)
library(dplyr)
library(ggplot2)
library(caret)
library(corrplot)
library(xgboost)
library(cowplot)
train = fread("Train_UWu5bXk.csv") 
test = fread("Test_u94Q5KV.csv") 
submission = fread("SampleSubmission_TmnO39y.csv")
dim(train)
dim(test)
names(train)
names(test)
str(train)
test[,Item_Outlet_Sales := NA] 
combi = rbind(train, test) # combining train and test datasets 
dim(combi)
ggplot(train) + geom_histogram(aes(train$Item_Outlet_Sales), 
                               binwidth = 100, fill ="blue") +
                               xlab("Item_Outlet_Sales")

p1 = ggplot(combi) + geom_histogram(aes(Item_Weight), binwidth = 0.5,
                                    fill = "Green")

p2 = ggplot(combi) + geom_histogram(aes(Item_Visibility), binwidth = 0.005,
                                    fill = "Green")


p3 = ggplot(combi) + geom_histogram(aes(Item_MRP), binwidth = 1,
                                    fill = "Green")

plot_grid(p1, p2, p3, nrow = 1)

ggplot(combi %>% group_by(Item_Fat_Content) %>% 
         summarise(Count = n())) +   
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")

combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular"

ggplot(combi %>% group_by(Item_Fat_Content) %>% 
         summarise(Count = n())) +   
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")


 p4 = ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) +   
  geom_bar(aes(Item_Type, Count), stat = "identity", fill = "coral1") +
   xlab("") + geom_label(aes(Item_Type, Count, label = Count), vjust = 0.5) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Item_Type")
 
 
 p5 = ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) +  
   geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "coral1") + 
   geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5) + 
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 p6 = ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) +   
   geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "coral1") + 
   geom_label(aes(Outlet_Size, Count, label = Count), vjust = 0.5) +  
   theme(axis.text.x = element_text(angle = 45, hjust = 1))

 
 second_row = plot_grid(p5, p6, nrow = 1)

plot_grid(p4, second_row, ncol = 1)

p7 = ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) +   
  geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = "identity", fill = "coral1") + 
  geom_label(aes(factor(Outlet_Establishment_Year), Count, label = Count), vjust = 0.5) +  
  xlab("Outlet_Establishment_Year") +  theme(axis.text.x = element_text(size = 8.5))

p8 = ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) +
  geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "coral1") +  
  geom_label(aes(factor(Outlet_Type), Count, label = Count), vjust = 0.5) +  
  theme(axis.text.x = element_text(size = 8.5))
# ploting both plots together 
plot_grid(p7, p8, ncol = 2)

p9 = ggplot(train) +      geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +  
  theme(axis.title = element_text(size = 8.5))
p9

p10 = ggplot(train) +       
  geom_point(aes(Item_Visibility, Item_Outlet_Sales), 
 colour = "violet", alpha = 0.3) +      
  theme(axis.title = element_text(size = 8.5))
p10

p11 = ggplot(train) +
  geom_point(aes(Item_MRP, Item_Outlet_Sales), 
             colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))
p11

second_row_2 = plot_grid(p10, p11, ncol = 2)
plot_grid(p9, second_row_2, nrow = 2)


p12 = ggplot(train) + geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "magenta") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text = element_text(size = 6),  
            axis.title = element_text(size = 8.5))

p13 = ggplot(train) + 
  geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "magenta") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),    
           axis.text = element_text(size = 8), 
              axis.title = element_text(size = 8.5))
p14 = ggplot(train) + 
  geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "magenta") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        axis.text = element_text(size = 8),   
        axis.title = element_text(size = 8.5))
second_row_3 = plot_grid(p13, p14, ncol = 2) 
plot_grid(p12, second_row_3, ncol = 1)

sum(is.na(combi$Item_Weight))


missing_index = which(is.na(combi$Item_Weight))
for (i in missing_index) {
  item = combi$Item_Identifier[i]
  combi$Item_Weight[i] = mean(combi$Item_Weight[combi$Item_Identifier == item], na.rm = TRUE)
}

sum(is.na(combi$Item_Weight))

zero_index = which(combi$Item_Visibility == 0) 
for(i in zero_index)
{    item = combi$Item_Identifier[i] 
  combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item],
                                  na.rm = T) 
  }
ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)

non_perishable = c("Baking Goods", "Canned", "Frozen Foods", 
               "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")

perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")

combi[,Item_Type_new := ifelse(Item_Type %in% perishable, "perishable", 
                               ifelse(Item_Type %in% non_perishable, "non_perishable", "not_sure"))]

table(combi$Item_Type, substr(combi$Item_Identifier, 1, 2))

combi[,Item_category := substr(combi$Item_Identifier, 1, 2)]
combi$Item_Fat_Content[combi$Item_category == "NC"] = "Non-Edible" 
combi[,Outlet_Years := 2013 - Outlet_Establishment_Year] 
combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year) 
combi[,price_per_unit_wt := Item_MRP/Item_Weight]
combi[,Item_MRP_Clusters := ifelse(Item_MRP <69 , "1st",
                            ifelse(Item_MRP >= 69 & Item_MRP <36, "2nd",
                            ifelse(Item_MRP >= 136 & Item_MRP < 203, "3rd", "4th")))]

combi[,Outlet_Size_num := ifelse(Outlet_Size == "Small", 0,
                          ifelse(Outlet_Size == "Medium", 1, 2))]


combi[,Outlet_Location_Type_num := ifelse(Outlet_Location_Type == "Tier 3", 0,    
                                          ifelse(Outlet_Location_Type == "Tier 2", 1, 2))]      

combi[,c("Outlet_Size","Outlet_Location_Type") := NULL]      
      
ohe = dummyVars("~.", data = combi[,-c("Item_Identifier",
                                      "Outlet_Establishment_Year", 
                                      "Item_Type")], fullRank = T) 
ohe_df = data.table(predict(ohe, combi[,-c("Item_Identifier", 
                                           "Outlet_Establishment_Year", 
                                           "Item_Type")]))     

combi[,Item_Visibility := log(Item_Visibility + 1)]
combi[,price_per_unit_wt := log(price_per_unit_wt + 1)]

num_vars = which(sapply(combi, is.numeric))      
num_vars_names = names(num_vars)
combi_numeric = combi[,setdiff(num_vars_names, "Item_Outlet_Sales"), with = F]
prep_num = preProcess(combi_numeric, method=c("center", "scale"))
combi_numeric_norm = predict(prep_num, combi_numeric)
combi[,setdiff(num_vars_names, "Item_Outlet_Sales") := NULL]
combi = cbind(combi, combi_numeric_norm)

train = combi[1:nrow(train)] 
test = combi[(nrow(train) + 1):nrow(combi)] 
test[,Item_Outlet_Sales := NULL]
cor_train = cor(train[,-c("Item_Identifier")]) 
corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.9)

linear_reg_mod = lm(Item_Outlet_Sales ~ ., data = train[,-c("Item_Identifier")])
submission$Item_Outlet_Sales = predict(linear_reg_mod, test[,-c("Item_Identifier")]) 
write.csv(submission, "Linear_Reg_submit.csv", row.names = F)
library(glmnet)
set.seed(1235)
my_control = trainControl(method="cv", number=5) 
Grid = expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0002)) 
lasso_linear_reg_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")],
                             y = train$Item_Outlet_Sales,      
                             method='glmnet', trControl= my_control, tuneGrid = Grid)

set.seed(1236) 
my_control = trainControl(method="cv", number=5) 
Grid = expand.grid(alpha = 0, lambda = seq(0.001,0.1,by = 0.0002)) 
ridge_linear_reg_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], y = train$Item_Outlet_Sales,   
                             method='glmnet', trControl= my_control, tuneGrid = Grid)
ridge_linear_reg_mod

set.seed(1237) 
my_control = trainControl(method="cv", number=5) # 5-fold CV 
tgrid = expand.grid(
  .mtry = c(3:10),
  .splitrule = "variance",
  .min.node.size = c(10,15,20)
)
library(e1071)
library(ranger)
library(RcppEigen)

rf_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")],
               y = train$Item_Outlet_Sales,
               method='ranger',
               trControl= my_control,
               tuneGrid = tgrid,
               num.trees = 400,
               importance = "permutation")


