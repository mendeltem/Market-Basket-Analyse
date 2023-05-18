setwd("E:/Computational Science/Sommersemester 2018/Data Discovery/Pro")
#options(java.parameters = "-Xmx2048m")
#rm(list=ls())
library(grid)
library(Matrix)
library(fpc)
library(arules)
require(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(anytime)
library(readr)
library(markdown)
library(knitr)
library(shiny)
library(stringr)
library(corrplot)
library(RColorBrewer)
library(arulesViz)

create_transactions <- function(df, class_num) {
    transactions <- df %>% filter(class == class_num)
    transactions <- as(split(transactions$Description, transactions[,"InvoiceNo"]), "transactions")
    return(transactions)
}

item_frequency_plot <- function(transactions, class_num) {
    itemFrequencyPlot(transactions,
                      type = "absolute",
                      topN = 10,
                      horiz = TRUE,
                      col = 'steelblue3',
                      xlab = '',
                      main = paste('Item frequency in class', class_num, 'Customer'))
}

create_rules <- function(transactions) {
    transaction_rules <- apriori(transactions, 
                                 parameter = list(supp = 0.015, conf = 0.7, target = "rules"))
    return(transaction_rules)
}

customer_full_class = customer_money_spend %>% select(CustomerID, class) %>% right_join(full_group_Id.df, by = "CustomerID")

classes <- c(1, 2, 3, 4)

for (class_num in classes) {
    transactions <- create_transactions(customer_full_class, class_num)
    item_frequency_plot(transactions, class_num)
    transaction_rules <- create_rules(transactions)
    print(transaction_rules)
}





#What is the the Benefit?
#Sending customized ads to special segments of customers


# Wir wollen die viel versprechendsten Waren finden die vom vielversprechendsten Kunden gekauft werden.
# Mit den Waren wollen wir eine Market Basket Analyse durchführen so dass wir eine Gezielt Verkaufstacktik dazu 
# erstellen können.
# Das Problem ist die schier riesige Datenmenge und daruas sollte ich eine gewinnbringende Strategie 
# für Firmen allgemein finden.
# Was kann ich tun damit die Unternehmen aus dem Verkauf möglichst beste Gewinnbringende Information 
#extrahieren kann

#import the data from xlsx
df <- read_excel("Online Retail.xlsx", 1, col_types = c("text","numeric","text","numeric","date","numeric","numeric","text"))

#Input the Cancelation Column through invoiceNo that beginnst with "c"
cancel.df = df %>% filter(grepl("C",df$InvoiceNo)) %>% mutate(cancel = 1 )
not_cancel.df = df %>% filter(!grepl("C",df$InvoiceNo)) %>% mutate(cancel = 0 )
#Clean The data 
not_cancel.df= not_cancel.df %>% filter( UnitPrice < 20000,UnitPrice > 0, !is.na(UnitPrice),!is.na(InvoiceNo),Quantity >0)
cancel.df = cancel.df %>% filter( UnitPrice < 20000,UnitPrice > 0, !is.na(UnitPrice),!is.na(InvoiceNo))

full_group_c.df = cancel.df %>% bind_rows(not_cancel.df )

#verkleinere um schneller zu arbeiten
#sample = cleaned_df[sample(nrow(cleaned_df),0.01*length(cleaned_df$InvoiceDate)),]
#Suche mir die Missing CustomerID
#generiere für jede missing InvoiseNo eine eigene Nummer

missing.df = full_group_c.df %>% group_by(InvoiceNo) %>%
                     filter(is.na(CustomerID)) %>%
                     transform(CustomerID=20000 + as.numeric(factor(InvoiceNo)))
#Suche mir die nicht Missing CustomerID 
not_missing.df =  full_group_c.df %>% filter(!is.na(CustomerID)) 
#Binde die beiden getrennten Dataframes zusammen
full_group_Id.df = not_missing.df %>% bind_rows(missing.df)


#retail <- retail[complete.cases(retail), ]
full_group_Id.df <- full_group_Id.df %>% mutate(Description = as.factor(Description))
full_group_Id.df <- full_group_Id.df %>% mutate(Country = as.factor(Country))
full_group_Id.df$Date <- as.Date(full_group_Id.df$InvoiceDate)
full_group_Id.df$Time <- format(full_group_Id.df$InvoiceDate,"%H:%M:%S")
#full_group_Id.df$InvoiceNo <- as.numeric(as.character(full_group_Id.df$InvoiceNo))


full_group_Id.df = full_group_Id.df %>% filter(grepl("WHITE",Description) | grepl("BLACK",Description) | grepl("RED",Description) | grepl("BLUE",Description)
                                    | grepl("GREEN",Description)| grepl("PINK",Description), Quantity >=0)


str(full_group_Id.df$Description)
#Erstelle einen CustomerID Profil um profitable Kunden zu entdecken
customer.df = full_group_Id.df %>% 
                              mutate( Quantity = ifelse( Quantity >=0 , Quantity  , 0  ) )%>%
                              group_by(CustomerID) %>% 
                              summarise(
                                        n_Items =  sum(Quantity),
                                        margin = round(sum(UnitPrice) * n_Items),
                                        days_since_lastOrder =as.numeric( round(Sys.time() -max(InvoiceDate)) ),
                                        n_Orders= n_distinct(InvoiceNo),
                                        n_canceled =as.numeric( n_distinct(grep('C', InvoiceNo, value=TRUE))),
                                        return_ratio = round( n_canceled/n_Orders, 2),
                                        margin_PerOrder = round( margin / n_Orders, 2 ) ,
                                        margin_perItem =round(as.numeric(margin / n_Items),2 ),
                                        
                                        meanItem_Price = round(mean(UnitPrice),2),
                                        item_perOrder = round(n_Items/n_Orders),
                                        days_between_first_last = as.numeric(round(as.Date(max(InvoiceDate)) - as.Date(min(InvoiceDate))))
                                        #futureMargin
                                        ) %>% 
                              filter(return_ratio<1) 


#Margin for last Month
last.df =full_group_Id.df  %>% 
                               mutate( Quantity = ifelse( Quantity >=0 , Quantity  , 0  ) )%>%
                               group_by(CustomerID) %>% filter(as.Date(InvoiceDate) >"2011-11-1") %>% 
                               summarise(margin_last_month  = round(sum(UnitPrice) * sum(Quantity)))
#Margin for last 11 Month                      
not_last.df= full_group_Id.df  %>% 
                                mutate( Quantity = ifelse( Quantity >=0 , Quantity  , 0  ) )%>%
                                group_by(CustomerID) %>% filter(!as.Date(InvoiceDate) >"2011-11-1") %>% 
                                summarise(margin_11Month  = round(sum(UnitPrice) * sum(Quantity)))


full_time_customer.df = customer.df %>% left_join(last.df, by="CustomerID" ) %>%
                                left_join(not_last.df, by="CustomerID" )

full_time_customer.df = full_time_customer.df %>% 
                           mutate( margin_last_month = ifelse( !is.na(margin_last_month), margin_last_month  , 0  ) ) %>%
                           mutate( margin_11Month = ifelse( !is.na(margin_11Month), margin_11Month  , 0  ) ) 






#which direction is going margin
full_time_customer.df %>% summarise( meanlast = mean(margin_last_month),
                                     meanelf  = mean(margin_11Month)/11,
                                     win =  meanlast/meanelf
                                     )
str(full_time_customer.df)

# Visualization of correlations
x=full_time_customer.df %>% 
  select(margin, n_Items, n_Orders,return_ratio,margin_PerOrder,margin_perItem,item_perOrder,days_between_first_last ) %>% 
  cor() %>%
  corrplot()



#controlls
#bloabl = customer.df %>%filter(CustomerID == 12349)
#b=full_group_Id.df %>% filter(CustomerID == 14911)



#Noch muss Zeitinterval hinzufügen weil jetzt Kundengesammtausgaben berechnet werden 
#statt durchscihnittliche ausgabe
summary(full_time_customer.df$margin)

# We remove the outliers only fot the graph
full_time_customer.df %>% filter(margin < 10000) %>% ggplot(aes(margin)) + geom_histogram(binwidth = 100)

customer_money_spend=full_time_customer.df %>% mutate(class = ifelse(margin >2.865e+05 , 4,
                    ifelse(margin > 4.073e+04, 3,
                    ifelse(margin > 5.654e+03, 2 , 1))))

#full_group_g= left_join(full_time_customer.df, customer_money_spend, by="CustomerID")

customer_full_class = customer_money_spend %>% select(CustomerID, class) %>% right_join(full_group_Id.df, by="CustomerID")










transaction_class4 = customer_full_class %>% filter(class==4)

transaction_class3 = customer_full_class %>% filter(class==3)
transaction_class2 = customer_full_class %>% filter(class==2)


transaction_class1 = customer_full_class %>% filter(class==1)



class4 <- as(split(transaction_class4$Description, transaction_class4[,"InvoiceNo"]), "transactions")
class1 <- as(split(transaction_class1$Description, transaction_class4[,"InvoiceNo"]), "transactions")
class2 <- as(split(transaction_class2$Description, transaction_class4[,"InvoiceNo"]), "transactions")
class3 <- as(split(transaction_class3$Description, transaction_class4[,"InvoiceNo"]), "transactions")

summary(class4)



itemFrequencyPlot(class2,
                  type="absolute",
                  topN=10,
                  horiz=TRUE,
                  col='steelblue3',
                  xlab='',
                  main='Item frequency in class 2 Customer')


itemFrequencyPlot(class3,
                  type="absolute",
                  topN=10,
                  horiz=TRUE,
                  col='steelblue3',
                  xlab='',
                  main='Item frequency in class 3 Customer')

itemFrequencyPlot(class1,
                  type="absolute",
                  topN=10,
                  horiz=TRUE,
                  col='steelblue3',
                  xlab='',
                  main='Item frequency in class 1 Customer')


itemFrequencyPlot(class4,
                  type="absolute",
                  topN=10,
                  horiz=TRUE,
                  col='steelblue3',
                  xlab='',
                  main='Item frequency in class 4 Customer')



trans_action_all <- as(split(full_group_Id.df$Description, full_group_Id.df[,"InvoiceNo"]), "transactions")



inspect(trans_action_all[1:10 ])



itemFrequencyPlot(trans_action_all,
                  type="absolute",
                  topN=10,
                  horiz=TRUE,
                  col='steelblue3',
                  xlab='',
                  main='Item frequency for all Customer')

itemFrequencyPlot(class4,
                  topN=20,
                  col=brewer.pal(8,'Pastel2'),
                  main='Relative Item Frequency Plot',
                  type="relative",
                  ylab="Item Frequency")


inspect(class4[1:10])

transaction_rules_special <- 
  apriori(trans_action_all, 
   parameter = list(supp=0.001, conf=0.09,target="rules"),
  appearance = list(default ="rhs", lhs="WHITE HANGING HEART T-LIGHT HOLDER")
            )


top <- sort(transaction_rules_special, decreasing = TRUE, na.last = NA, by = "confidence")


inspect(top)
top10Rules <- transaction_rules_special[1:10]
plot(top10Rules, method="graph")

trans_action_color <- as(split(full_group_Id.df$Description, full_group_Id.df[,"InvoiceNo"]), "transactions")




#transaction for class4 customer
transaction_rules <- apriori(trans_action_all, 
                             parameter = list(supp=0.015, conf=0.7,target="rules"))
inspect(transaction_rules)


#How is sales for number 1 item developing
ACBO.df = full_group_Id.df %>% group_by(Description) %>% filter(Description == "WHITE HANGING HEART T-LIGHT HOLDER")
ACBO.df %>% filter(Quantity >0) %>%
ggplot(aes(InvoiceDate, Quantity)) + geom_point() + geom_smooth(method = lm, se = FALSE) 



##Most bought items
tmp <- full_group_Id.df %>% 
  group_by(StockCode, Description) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))
tmp <- head(tmp, n=10)
tmp
tmp %>% 
  ggplot(aes(x=reorder(Description,count), y=count))+
  geom_bar(stat="identity",fill="green")+
  coord_flip()



#color = full_group_Id.df %>% filter(grepl("WHITE",Description) | grepl("BLACK",Description) | grepl("RED",Description) | grepl("BLUE",Description)
 #                                   | grepl("GREEN",Description)| grepl("PINK",Description), Quantity >=0)

head(color)

trans_action_color <- as(split(color$Description, color[,"InvoiceNo"]), "transactions")


itemFrequencyPlot(trans_action_color,
                  topN=20,
                  col=brewer.pal(8,'Pastel2'),
                  main='Relative Item Frequency Plot',
                  type="relative",
                  ylab="Item Frequency")

image(top10ColorRules, method="graph")


transaction_rules_color <- apriori(trans_action_color, 
                             parameter = list(supp=0.015, conf=0.7,target="rules"))


top <- sort(transaction_rules_color, decreasing = TRUE, na.last = NA, by = "confidence")




inspect(transaction_rules_color[1:19])




#what hour does the customer buy?
#full_group_Id.df$Time <- as.factor(full_time_customer.df$Time)
#a <- hms(as.character(full_group_Id.df$Time))
#full_group_Id.df$Time = hour(a)
full_group_Id.df %>% 
  ggplot(aes(x=Time)) + 
  geom_histogram(stat="count",fill="blue")





#d=full_group_Id.df %>% left_join(customer_full_class, by="CustomerID")






transaction_rules_special <- 
  apriori(trans_action_color, 
          parameter = list(supp=0.001, conf=0.09,target="rules"),
          appearance = list(default ="rhs", lhs="WHITE HANGING HEART T-LIGHT HOLDER"))





top <- sort(transaction_rules_special, decreasing = TRUE, na.last = NA, by = "confidence")

str(transaction_rules_special)

top10Rules <- transaction_rules_special[c(3,5,7,11,12,13,14,15,16)]

inspect(top10Rules)
plot(top10Rules, method="graph")








transaction_rules_color_class1 <- apriori(class1, 
                                   parameter = list(supp=0.007, conf=0.7,target="rules"))

redundant <- which (colSums (is.subset (transaction_rules_color_class1, transaction_rules_color_class1)) > 1) # get redundant rules in vector

transaction_rules_color_class1 <- transaction_rules_color_class1[-redundant] # remove redundant rules



transaction_rules_color_class1 <- sort(transaction_rules_color, decreasing = TRUE, na.last = NA, by = "confidence")



df = data.frame(
  lhs = labels(lhs(transaction_rules_color_class1)),
  rhs = labels(rhs(transaction_rules_color_class1)), 
  transaction_rules_color_class1@quality)


df %>% filter(grepl("PINK",lhs), grepl("GREEN",rhs)) %>% summarise(m_support = mean(support), m_confidence = mean(confidence))
df %>% filter(grepl("RED",lhs), grepl("RED",rhs)) %>% summarise(m_support = mean(support), m_confidence = mean(confidence))

df %>% filter(grepl("PINK",lhs), grepl("RED",rhs)) %>% summarise(m_support = mean(support), m_confidence = mean(confidence))
df %>% filter(grepl("GREEN",lhs), grepl("PINK",rhs)) %>% summarise(m_support = mean(support), m_confidence = mean(confidence))


df %>% filter(grepl("WHITE",lhs), grepl("RED",rhs)) %>% summarise(m_support = mean(support), m_confidence = mean(confidence))

df %>% filter(grepl("BLUE",lhs), grepl("RED",rhs)) %>% summarise(m_support = mean(support), m_confidence = mean(confidence))



