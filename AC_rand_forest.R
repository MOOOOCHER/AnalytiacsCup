library(tidymodels)
library(tidyverse)
library(GGally)
library(lubridate)
library(data.table)
library(ROSE)

setwd("/Users/maximilianmatscher/Documents/UNI/WS22/BA&ML/accup")

#read as tibble
classification = read_csv("classification.csv")
customers = read_csv("customers.csv")
sales_orders_header = read_csv("sales_orders_header.csv")
sales_orders = read_csv("sales_orders.csv")
service_map = read_csv("service_map.csv")
business_units = read_csv("business_units.csv")

class_custom = inner_join(classification, customers, by = c('Customer_ID' = 'Customer_ID'))
class(class_custom$Item_Position) = "numeric"

sales_items_direkt_match = inner_join(class_custom, sales_orders, by = c('Sales_Order' = 'Sales_Order', 'Item_Position' = 'Item_Position') )
sales_items_no_match = anti_join(sales_orders, class_custom, by = c('Sales_Order' = 'Sales_Order', 'Item_Position' = 'Item_Position') )
sales_items_no_match <- sales_items_no_match %>% mutate(Item_Position = 0) 

sales_items_indirekt_match = inner_join(class_custom, sales_items_no_match, by = c('Sales_Order' = 'Sales_Order', 'Item_Position' = 'Item_Position'))

sales_items = bind_rows(sales_items_direkt_match, sales_items_indirekt_match)

sales_items =  inner_join(sales_items, sales_orders_header, by = c('Sales_Order' = 'Sales_Order'))
sales_items =  inner_join(sales_items, business_units, by = c('Cost_Center' = 'Cost_Center'))

sales_items = sales_items[!is.na(sales_items$Reseller),]

sales_items = mutate(sales_items, is_service = if_else(Material_Class %in% service_map$MATKL_service, 1, 0))
#sales_items = mutate(sales_items, Material_Class = factor(Material_Class, ordered = TRUE, levels = c("S", "M", "L", "XL", "XXL", "XXXL"))

sales_items$Item_Position = NULL
sales_items$Material_Code = NULL
sales_items$Net_Value.x = NULL
sales_items$Creation_Date = NULL
sales_items$Creator = NULL
sales_items$Release_Date = NULL
sales_items$YHKOKRS = NULL

df = sales_items %>% group_by(Customer_ID)  %>%
  summarise(Reseller = if_else(sum(Reseller) >= 1, 1, 0),
            total_sales = sum(Net_Value.y),
            total_num_items = sum(Num_Items),
            total_num_item_positions = n(),
            total_unique_sales_order = n_distinct(Sales_Order),
            items_per_order=sum(Num_Items)/n_distinct(Sales_Order), 
            avg_price_per_item= if_else(sum(Num_Items) > 0, sum(Net_Value.y)/sum(Num_Items), sum(Net_Value.y)),
            avg_order_size=sum(Net_Value.y)/n_distinct(Sales_Order),
            type_sum_stp = sum(if_else(Type=="STP", 1, 0)) / n(),
            #type_sum_sop = sum(if_else(Type=="SOP", 1, 0)) / n(),
            sales_organization_A = sum(if_else(Sales_Organization=="A", 1, 0)) / n(),
            #sales_organization_B = sum(if_else(Sales_Organization=="B", 1, 0)),
            document_type_Order = sum(if_else(Document_Type=="Order", 1, 0)) / n(),
            document_type_Credit = sum(if_else(Document_Type=="Credit memo", 1, 0)) / n(),
            document_type_OrderCharge = sum(if_else(Document_Type=="Order w/o charge", 1, 0)) / n(),
            document_type_Contract = sum(if_else(Document_Type=="Contract", 1, 0) / n()),
            #document_type_Returns = sum(if_else(Document_Type=="Returns", 1, 0)),
            delivery_not_rel = sum(if_else(Delivery=="Not relevant", 1, 0)) / n(),
            delivery_com_processed = sum(if_else(Delivery=="Completely processed", 1, 0)) / n(),
            delivery_part_processed = sum(if_else(Delivery=="Partially processed", 1, 0)) / n(),
            #delivery_not_yet_processed = sum(if_else(Delivery=="Not yet processed"   , 1, 0)),
            business_unit_A = sum(if_else(Business_Unit=="BU_A" , 1, 0)) / n(),
            business_unit_B = sum(if_else(Business_Unit=="BU_B" , 1, 0)) / n(),
            #business_unit_C = sum(if_else(Business_Unit=="BU_C" , 1, 0)),
            total_is_service = sum(is_service) / n(),
            .groups = 'drop')


# Train-test split
train_test_split <- initial_split(df, prop = 0.80, strata=Reseller)
train_df <- training(train_test_split)
train_df = ovun.sample(Reseller ~ ., data = train_df, method = "under", p = 0.4, seed = 5)$data

test_df <- testing(train_test_split)

# View proportions of Reseller in train/test-split
train_df %>% 
  count(Reseller) %>% 
  mutate(prop = n/sum(n))

test_df %>% 
  count(Reseller) %>% 
  mutate(prop = n/sum(n))

train_df$Customer_ID = NULL
test_df$Customer_ID = NULL

train_model <- rand_forest(mode = 'classification',
                           mtry=10,
                           trees = 10000) %>% 
  set_engine("ranger", importance = 'impurity')

# Define Workflow
training_workflow <- 
  workflow() %>%
  add_model(train_model) %>%
  add_formula(as.factor(Reseller) ~ .)

# Get trained model
trained_model <- training_workflow %>% 
  fit(data=train_df)

# Variable Importance Plot
trained_model %>% extract_fit_parsnip() %>%
  vip::vip(geom='col', num_features = ncol(train_df))


# Apply on test set
test_df <- 
  predict(trained_model, test_df)%>% 
  bind_cols(predict(trained_model, test_df, type = "prob")) %>% 
  bind_cols(test_df %>% select(Reseller))

test_df %>% group_by(Reseller, .pred_class) %>% summarise(count = n())

test_df = test_df %>% mutate(Reseller = factor(Reseller, labels = c(0,1)),
                             pred = factor(.pred_class, labels = c(0,1)))
## Error rate of Logit model

rec = test_df %>% recall(Reseller, pred)
spec = test_df %>% specificity(Reseller, pred)

BAC = (rec$.estimate + spec$.estimate) / 2

