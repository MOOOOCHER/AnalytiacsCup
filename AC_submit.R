library(tidymodels)
library(tidyverse)
library(GGally)
library(lubridate)
library(data.table)
library(ROSE)
set.seed(2032)
#setwd("/Users/maximilianmatscher/Documents/UNI/WS22/BA&ML/accup")

#read as tibble
classification = read_csv("classification.csv")
customers = read_csv("customers.csv")
sales_orders_header = read_csv("sales_orders_header.csv")
sales_orders = read_csv("sales_orders.csv")
service_map = read_csv("service_map.csv")
business_units = read_csv("business_units.csv")

class_custom = inner_join(classification, customers, by = c('Customer_ID' = 'Customer_ID'))
class(class_custom$Item_Position) = "numeric"

#Joining trickery to get correct assignment of ItemPositions
sales_items_direkt_match = inner_join(class_custom, sales_orders, by = c('Sales_Order' = 'Sales_Order', 'Item_Position' = 'Item_Position') )
sales_items_no_match = anti_join(sales_orders, class_custom, by = c('Sales_Order' = 'Sales_Order', 'Item_Position' = 'Item_Position') )
sales_items_no_match <- sales_items_no_match %>% mutate(Item_Position = 0) 
sales_items_no_match <- sales_items_no_match %>% group_by(Sales_Order, Item_Position, Material_Code, Material_Class, Cost_Center, Net_Value) %>%
  summarise_all(sum)

sales_items_indirekt_match = inner_join(class_custom, sales_items_no_match, by = c('Sales_Order' = 'Sales_Order', 'Item_Position' = 'Item_Position'))

#merge direct + indirect matches
sales_items = bind_rows(sales_items_direkt_match, sales_items_indirekt_match)

#join remaining tables
sales_items =  inner_join(sales_items, sales_orders_header, by = c('Sales_Order' = 'Sales_Order'))
sales_items =  inner_join(sales_items, business_units, by = c('Cost_Center' = 'Cost_Center'))

#get infromation whether a Customer shipped to third party
STP_SOP = sales_items %>% group_by(Sales_Order, Item_Position, Material_Code) %>%
  summarise(shipped_to_third_party = if_else(n_distinct(Customer_ID) == 2 & Type == 'SOP', 1, 0),
            Type,
            Customer_ID,
            .groups = 'drop')

STP_SOP = STP_SOP %>% group_by(Customer_ID) %>%
  summarise(sum_shipped_to_third_party = sum(shipped_to_third_party),
            .groups = 'drop')

#check in service_map table whether a used Material class is classified as servidce
sales_items = mutate(sales_items, is_service = if_else(Material_Class %in% service_map$MATKL_service, 1, 0))

#get tabe of all possible Material classes, important later
material_classes = as.character(unique(sales_items$Material_Class))
material_classes = setdiff(material_classes, NA)

#remove unnecessary columns
sales_items$Item_Position = NULL
sales_items$Net_Value.y = NULL
sales_items$Creator = NULL
sales_items$YHKOKRS = NULL

#calcualte a date span between Release and Creation date
sales_items$Release_Date = as.POSIXct(strptime(sales_items$Release_Date, "%Y-%m-%d %H:%M:%S.000"))
sales_items$Release_Date = if_else(is.na(sales_items$Release_Date), Sys.time(), sales_items$Release_Date)
sales_items$Date_Span = sales_items$Release_Date - sales_items$Creation_Date

#remove unnecessary columns
sales_items$Release_Date = NULL
sales_items$Creation_Date = NULL

#split all values of Material_Class in a single column
sales_items = sales_items %>% mutate(val = 1) %>% distinct() %>% pivot_wider(names_from = Material_Class, values_from = "val", values_fill = 0)

#get Customers for test set (everyone with unknown Reseller)
test_df = sales_items
test_df = test_df[is.na(test_df$Reseller),]
test_df = unique(test_df[c("Customer_ID", "Reseller", "Test_set_id")])

#aggregate item positions to have a single value for each Customer
df = sales_items %>% group_by(Customer_ID)  %>%
  summarise(
            Reseller = if_else(sum(Reseller) >= 1, 1, 0),
            total_sales = sum(Net_Value.x),
            total_num_items = sum(Num_Items),
            total_num_item_positions = n(),
            total_unique_sales_order = n_distinct(Sales_Order),
            items_per_order=sum(Num_Items)/n_distinct(Sales_Order), 
            avg_price_per_item= if_else(sum(Num_Items) > 0, sum(Net_Value.x)/sum(Num_Items), sum(Net_Value.x)),
            avg_order_size=sum(Net_Value.x)/n_distinct(Sales_Order),
            type_sum_stp = sum(if_else(Type=="STP", 1, 0)) / n(),
            sales_organization_A = sum(if_else(Sales_Organization=="A", 1, 0)) / n(),
            document_type_Order = sum(if_else(Document_Type=="Order", 1, 0)) / n(),
            document_type_Credit = sum(if_else(Document_Type=="Credit memo", 1, 0)) / n(),
            document_type_Contract = sum(if_else(Document_Type=="Contract", 1, 0) / n()),
            document_type_Returns = sum(if_else(Document_Type=="Returns", 1, 0)) / n(),
            delivery_not_rel = sum(if_else(Delivery=="Not relevant", 1, 0)) / n(),
            delivery_com_processed = sum(if_else(Delivery=="Completely processed", 1, 0)) / n(),
            delivery_part_processed = sum(if_else(Delivery=="Partially processed", 1, 0)) / n(),
            business_unit_A = sum(if_else(Business_Unit=="BU_A" , 1, 0)) / n(),
            business_unit_B = sum(if_else(Business_Unit=="BU_B" , 1, 0)) / n(),
            total_is_service = sum(is_service) / n(),
            num_diff_material_code = n_distinct(Material_Code),
            #num_diff_material_class = n_distinct(Material_Class), --- removed because each Material_Class has his own column now (and that caused to remove it)
            average_Date_Span = sum(Date_Span) / n(),
            across(all_of(material_classes), ~ sum(.x) / n(), .names = "Material_Class_{col}"),
            .groups = 'drop') 

#inser data about third party shippings
df = inner_join(df, STP_SOP, by = c('Customer_ID' = 'Customer_ID'))
df$sum_shipped_to_third_party = df$sum_shipped_to_third_party / df$total_num_item_positions

# Train-test split
train_df <- anti_join(df, sales_items_testset, by = c('Customer_ID' = 'Customer_ID'))
train_df = ovun.sample(Reseller ~ ., data = train_df, method = "under", p = 0.5, seed = 12)$data

# Create Test data with aggregated data
test_df <- inner_join(test_df,df , by = c('Customer_ID' = 'Customer_ID'))
test_df = test_df %>% mutate(Reseller = Reseller.x)
test_df['Reseller.x'] = NULL
test_df['Reseller.y'] = NULL


# View proportions of Reseller in train/test-split
train_df %>% 
  count(Reseller) %>% 
  mutate(prop = n/sum(n))

train_df$Customer_ID = NULL
test_df$Customer_ID = NULL

train_model <- rand_forest(mode = 'classification',
                           mtry=35,
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
  vip::vip(geom='col', num_features = 39)

# Apply on test set
test_df <- 
  predict(trained_model, test_df)%>% 
  bind_cols(test_df %>% select(Test_set_id))

#reformat predictions to export
test_df %>% group_by(.pred_class) %>% summarise(count = n())
test_df <- test_df %>% mutate(id = Test_set_id, prediction = .pred_class)
test_df['.pred_class'] = NULL
test_df['Test_set_id'] = NULL
test_df = test_df[order(test_df$id),]

#export submission
write_csv(test_df, 'submissions/predictions_pending_teamname_0.csv')

