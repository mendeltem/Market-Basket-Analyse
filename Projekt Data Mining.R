setwd("E:/Computational Science/Sommersemester 2018/Data Discovery/Pro")
#options(java.parameters = "-Xmx2048m")
#rm(list=ls())

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



class1 = customer_money_spend %>% filter(class == 1)
class2 = customer_money_spend %>% filter(class == 2)
class3 = customer_money_spend %>% filter(class == 3)
class4 = customer_money_spend %>% filter(class == 4)


customer_full_class = customer_money_spend %>% select(CustomerID, class) %>% right_join(full_group_Id.df, by="CustomerID")


transaction_class4 = customer_full_class %>% filter(class==4)
transaction_class3 = customer_full_class %>% filter(class==3)
transaction_class2 = customer_full_class %>% filter(class==2)
transaction_class1 = customer_full_class %>% filter(class==1)



class4 <- as(split(transaction_class4$Description, transaction_class4[,"InvoiceNo"]), "transactions")

summary(class4)

itemFrequencyPlot(class4,
                  type="absolute",
                  topN=10,
                  horiz=TRUE,
                  col='steelblue3',
                  xlab='',
                  main='Item frequency in class 4 Customer')


trans_action_all <- as(split(full_group_Id.df$Description, full_group_Id.df[,"InvoiceNo"]), "transactions")

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

transaction_rules_special <- apriori(trans_action_all, 
                                parameter = list(supp=0.001, conf=0.1,target="rules"),
                                appearance = list(default ="rhs", lhs="WHITE HANGING HEART T-LIGHT HOLDER")
                                )
inspect(transaction_rules_special)


transaction_rules <- apriori(class4, 
                             parameter = list(supp=0.015, conf=0.7,target="rules"))
inspect(transaction_rules)



ACBO.df = full_group_Id.df %>% group_by(Description) %>% filter(Description == "WHITE HANGING HEART T-LIGHT HOLDER")

ACBO.df %>% filter(Quantity >0) %>%
ggplot(aes(InvoiceDate, Quantity)) + geom_point() + geom_smooth(method = lm, se = FALSE) 


