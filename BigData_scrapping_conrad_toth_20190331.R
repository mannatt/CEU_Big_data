# SCRAPING + Data cleaning #####
library (rvest)
library (data.table)
library (httr)
library(randomForest)
library(ggplot2)
library(cowplot)
library(data.table)
library(randomForestExplainer)
library(summarytools)

##Webscraping codes

#codes for the first page 
my_url <- 'https://www.glassdoor.com/Reviews/us-reviews-SRCH_IL.0,2_IN1.htm'
x <- read_html(my_url)

my_companynames <-
  trimws(x%>%
           html_nodes('.tightAll') %>%
           html_text())
my_companynames

my_companylinks <-
  x%>%
  html_nodes('.tightAll') %>%
  html_attr('href')
paste0('https://www.glassdoor.com/', my_companylinks)
my_companylinks2 <-  paste0('https://www.glassdoor.com', my_companylinks) 
my_companylinks2

t <- read_html('https://www.glassdoor.com/Reviews/us-reviews-SRCH_IL.0,2_IN1.htm')
write_html(t, 't.html')

my_boxes<- 
  x%>%
  html_nodes('.snug')
write_html(my_boxes[[1]], 'z.html')

rating_list <-
  x%>%
  html_nodes('.h1')%>%
  html_text()

location<- x%>%
  html_nodes('.value')%>%
  html_text()

review_list <-
  x%>%
  html_nodes('.reviews .h2')%>%
  html_text()

library(data.table)
my_table1=
  data.table('companynames'= my_companynames, 'company_link'=my_companylinks2, 'rating'=rating_list,  
             'reviews'= trimws(review_list), 'location'=location )


#building the loop 2-300 
datatable <- NULL
datatable <- data.table(datatable)

for (i in 2:300) {
  my_url <- paste0('https://www.glassdoor.com/Reviews/us-reviews-SRCH_IL.0,2_IN1_IP',i, '.htm')
  glassdoor <- read_html(my_url)
  
  x <- read_html(my_url)
  
  my_companynames2 <-
    trimws(x%>%
             html_nodes('.tightAll') %>%
             html_text())
  my_companynames2
  
  my_companylinks3 <-
    x%>%
    html_nodes('.tightAll') %>%
    html_attr('href')
  paste0('https://www.glassdoor.com/', my_companylinks3)
  my_companylinks3 <-  paste0('https://www.glassdoor.com', my_companylinks3) 
  my_companylinks3
  
  t1 <- read_html('https://www.glassdoor.com/Reviews/us-reviews-SRCH_IL.0,2_IN1_IP2.htm')
  write_html(t1, 't1.html')
  
  my_boxes2<- 
    x%>%
    html_nodes('.snug')
  write_html(my_boxes2[[1]], 'z1.html')
  
  rating_list2 <-
    x%>%
    html_nodes('.h1')%>%
    html_text()
  
  location2<- x%>%
    html_nodes('.value')%>%
    html_text()
  
  review_list2 <-
    x%>%
    html_nodes('.reviews .h2')%>%
    html_text()
  
  my_table3=
    data.table('companynames'= my_companynames2, 'company_link'=my_companylinks3, 'rating'=rating_list2,  
               'reviews'= trimws(review_list2), 'location'=location2)
  
  datatable <- rbind(datatable,my_table3) 
  message(i)
}

# merging page 1 and all the other pages 
final_data <- rbind(datatable, my_table1)

#cleaning + merging  
final_data_cleaned <- unique(datatable, by="companynames")

#filtering for companies with more than 1000 reviews
test4 <- final_data_cleaned
deletek <- test4 [reviews  %like% "k"]
deletek
deletek$reviews = as.numeric(gsub ("k","", deletek$reviews ))
deletek$reviews <- deletek$reviews*1000

datatable_cleaned_deletek <- unique(deletek, by="companynames")


saveRDS(datatable_cleaned_deletek, 'company_lists.rds')
write.csv(datatable_cleaned_deletek, 'final.csv', row.names = F)

#loop for industry, size and revenue
datatable44 <- NULL
datatable44 <- data.table(datatable44)

for (i in 1:852) {
  k <-read_html(deletek$company_link[i])
  my_companynames4 <-
    trimws(k%>%
             html_nodes('.tightAll') %>%
             html_text())
  my_companynames4
  
  my_boxes <- k%>%
    html_nodes('#EmpBasicInfo div') %>%
    html_text()
  my_boxes
  
  my_table66=
    data.table('companynames'= my_companynames4,'size'= my_boxes [[5]], 'industry'= my_boxes [[8]], 'revenue'= my_boxes [[9]])
  
  datatable44 <-rbind(datatable44,my_table66)
  message(i)
  
}  

########cleaning + merging 
final_data_cleaned44 <- unique(datatable44, by="companynames")
