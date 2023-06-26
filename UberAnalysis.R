#install the packages if necessary
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("sqldf")) install.packages("sqldf")
if(!require("formattable")) install.packages("formattable")
if(!require("readxl")) install.packages("readxl")

#load packages
library(tidyverse)
library(sqldf)
library(formattable)
library(readxl)


#Change the working directory to the folder in which the data is stored
setwd("C:/Users/Dell Latitud E7450/Desktop/UberTest")


#Load the Uber provided data
base_data_set <- data.frame(read.csv("Data Set - Territory Analyst, Uber Eats CenAm.csv"))


#Cleaning Uber Provided data
colnames(base_data_set)[1] <- "date"

base_data_set <- base_data_set %>% 
                separate(. , date, into = c("date", "time"), sep = " ") %>%  
                na.omit(.) %>% 
                mutate_at(vars(basket, total_eater_promos), 
                          ~currency(., format = "f", digits = 2)) %>%
                 na.omit(.)
  

#Exploratory Analysis
avg_basket_by_cuisine <- sqldf("SELECT cuisine, avg(basket) as avg_basket, count(distinct(order_id)) as total_orders, avg(total_eater_promos) as avg_promo FROM base_data_set
                             WHERE basket > 0
                             GROUP BY cuisine
                             ORDER by avg_basket DESC
                            ")


avg_rev_by_cuisine <- sqldf("SELECT cuisine, avg(basket - total_eater_promos) as rev FROM base_data_set
                             WHERE basket > 0
                             GROUP BY cuisine
                             ORDER by rev DESC
                            ")

avg_rev_by_user_top10 <- sqldf("SELECT user_id, avg(basket - total_eater_promos) as rev FROM base_data_set
                             WHERE basket > 0
                             GROUP BY user_id
                             ORDER by rev DESC
                             Limit 10
                            ")


distinct_orders <- sqldf("SELECT count(distinct(order_id)) as orders FROM base_data_set
      group by user_id")

mean(distinct_orders$orders)



#Hipotesis: Is there an specifc time of the day in which there are more orders place?
ggplot(data = base_data_set, aes(x = time, y = basket-total_eater_promos, col = cuisine))+
  geom_line()+ 
  geom_vline(xintercept = "17:45:55", col = "red", linetype = "dashed")+
  annotate(geom="text", x=4000, y=780, label="17:45:55",
             color="red")+
  ylab("Total Revenue")+ xlab("Time of the day")+ labs(caption = "Source: Internal data")+
  labs(title ="Revenue by the Hour") +
  theme(plot.title = element_text(hjust = 0.5))


#Which cuisine drive the most revenue?
ggplot(data = base_data_set, aes(x = cuisine, y = (basket - total_eater_promos), fill = cuisine))+
 geom_boxplot() + ylab("Total Revenue")+ labs(caption = "Source: Internal data")+
  labs(title ="Revenue by cuisine type") +
  theme(plot.title = element_text(hjust = 0.5))


#External data: Supermarkets price monitoring, source: CNP https://www.cnp.go.cr/sim/Precios_Nac_Mensuales.aspx
path <- "PNM_supermercados_2007-2023.xlsx"

supermarket <- path %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel,
      path = path)

#Separate each table from the supermarket file into its own table
sub_list <- lapply(supermarket, as.data.frame)

for (i in 1:length(sub_list)){
  sublist_name <- names(sub_list)[i]
  assign(sublist_name, sub_list[[i]])
                                      }


#Cleaning special characters from all tables
colnames(ABARROTES) <- colnames(ABARROTES) %>% gsub(" ", "_", .) %>% gsub("-", "", .) %>% gsub("/", "", .)
colnames(GRANOS)<- colnames(GRANOS) %>% gsub(" ", "_", .) %>% gsub("-", "", .) %>% gsub("/", "", .)
colnames(CARNICOS)<- colnames(CARNICOS) %>% gsub(" ", "_", .) %>% gsub("-", "", .) %>% gsub("/", "", .)
colnames(HORTALIZAS)<- colnames(HORTALIZAS) %>% gsub(" ", "_", .) %>% gsub("-", "", .) %>% gsub("/", "", .)
colnames(FRUTAS)<- colnames(FRUTAS) %>% gsub(" ", "_", .) %>% gsub("-", "", .) %>% gsub("/", "", .)


#Grouping all prices into a single table
clean_supermarket_prices <- sqldf("SELECT * FROM ABARROTES
        JOIN GRANOS
        USING(FechaProducto)
        JOIN CARNICOS
        USING(FechaProducto)
        JOIN HORTALIZAS
        USING(FechaProducto)
        JOIN FRUTAS
        USING(FechaProducto) 
        ")

rm(ABARROTES)
rm(GRANOS)
rm(FRUTAS)
rm(CARNICOS)
rm(HORTALIZAS)
rm(sub_list)
rm(sublist_name)

supermarket_price_variability <- list(sort(sapply(subset(clean_supermarket_prices,
                                                         select = -FechaProducto), 
                                                  mad, na.rm = TRUE), decreasing =T))

test <- unlist(supermarket_price_variability)


#exporting the supermarket price analysis

write.csv(clean_supermarket_prices, file = "data1.csv", row.names = FALSE)
write.csv(supermarket_price_variability, file = "data2.csv", row.names = FALSE)
