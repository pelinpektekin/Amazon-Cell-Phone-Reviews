###Intsalling packages
install.packages("dplyr")
install.packages("tidyverse")
install.packages("tidyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("tidytext")
install.packages("broom")
install.packages("stringr")
install.packages("textdata")
install.packages("scales")
install.packages("wordcloud")
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(tidytext)
library(broom)
library(stringr)
library(textdata)
library(scales)
library(wordcloud)
library(reshape2)

library(readr)
X20191226_items <- read_csv("~/Dropbox/*TEDU_ADS/ADS531_informational_retrieval/term_project/amazon-cell-phones-reviews/20191226-items.csv")
library(readr)
X20191226_reviews <- read_csv("~/Dropbox/*TEDU_ADS/ADS531_informational_retrieval/term_project/amazon-cell-phones-reviews/20191226-reviews.csv")
items <- X20191226_items
reviews <- X20191226_reviews


###Expoloring datasets
cat("Data Set has ",dim(items)[1], " Rows and ", dim(items)[2], " Columns" )
cat("Data Set has ",dim(reviews)[1], " Rows and ", dim(reviews)[2], " Columns" )
str(items)
str(reviews)
sapply(items, function(x) sum(is.na(x)))
sapply(reviews, function(x) sum(is.na(x)))


#Dropping only NAs in items because 4 have not brand names
items <- na.omit(items)

#Merging dataset
names(reviews)[names(reviews)== "rating"] <- "reviewer_rating"
names(items)[names(items)=="rating"] <- "product_rating"
names(items)[names(items)=="title"] <- "product"
names(reviews)[names(reviews)=="title"] <- "review_title"

amazon <- merge(reviews, items, by = "asin")
amazon

sapply(amazon, function(x) sum(is.na(x)))


summary(amazon$name)
summary(amazon$reviewer_rating)
summary(amazon$date)
summary(amazon$verified) #valid consumer
summary(amazon$review_title)
summary(amazon$body)
summary(amazon$helpfulVotes)

summary(amazon$brand)
amazon %>% select(brand) %>% distinct()

summary(amazon$product)
amazon %>% select(product) %>% distinct()

summary(amazon$product_rating)
amazon %>% select(product_rating) %>% distinct()

summary(amazon$totalReviews)
amazon %>% select(totalReviews) %>% distinct()

summary(amazon$price)
amazon %>% select(price) %>% distinct()

#Important variables are brand-rating-totalreviews-price
amazon <- amazon %>% select(brand, product_rating, reviewer_rating, totalReviews, price, everything())

str(items)
summary(items)
summary(reviews)
amazon$verified <- as.factor(amazon$verified)
########PLOTSSS##############
amazon <- amazon %>% separate(date, c("daynmonth","year"), ",")
amazon %>% select(year) %>% distinct() #years btw 2005-2019
#Distiribution of Reviews by year
barplot(table(amazon$year), col = "blue", main = "Number of reviews in Years", ylab = "Frequency", las=1)
#Disribution of total reviews
ggplot(amazon, aes(x = totalReviews)) + 
  geom_density(alpha = 0.6, color="darkblue", fill="lightblue") +
  labs(x = "Total Reviews", y = "") +
  ggtitle("Distribution of Total Reviews")

#Distribution of Price #Exludig price =0.00
amazonp <- amazon %>% 
  filter(price != 0.00)
ggplot(amazonp, aes(x = price)) + 
  geom_density(alpha = 0.6, color="darkblue", fill="lightblue") +
  labs(x = "Price", y = "") +
  ggtitle("Distribution of Price")

#grouping price
amazonp <- amazonp %>% 
  mutate(price_group = if_else(between(price, 0, 250), "low_price", 
                               if_else(between(price, 250, 450), "medium_price",
                                       if_else(price > 450, "high_price",
                                               "unknown_price"))), 
         price_group = if_else(is.na(price_group), "unknown_price", price_group)
  ) %>% 
  rownames_to_column(var = "id")
p_group_cl <- amazonp %>% 
  dplyr::filter(!is.na(price_group)) %>% 
  ggplot(aes(x = price, fill = price_group)) +
  geom_density(alpha = 0.3) +
  labs(title = "Distribution of Price Groups")
p_group_cl

#Price and relationship with total Reviews
ggplot(amazonp, aes(totalReviews, price), na.rm=TRUE) +
  geom_point(color = "blue")

ggplot(amazonp, aes(totalReviews, price), na.rm=TRUE) +
  geom_point(color = "blue") +
  facet_wrap(~price_group)


#Distribution of total reviews by brand
barplot(table(amazon$`brand`), col = "blue", main = "Total Reviews by brand"
        , ylab = "Frequency", las=1)

#Ratings vs. Number of reviews
ggplot(amazon, aes(x=product_rating)) + geom_histogram(binwidth = 1, color="darkblue", fill="lightblue") + 
  coord_cartesian(xlim = c(.5, 5.5))+
  labs(title ="Distribution of Product Ratings ", x = "Ratings", y = "Number  of Reviews") +
  scale_x_continuous(breaks=seq(1,5, by = 1))

#Ratings fill by brand
ggplot(amazon, aes(x = product_rating,
                   y = ..count../tapply(..count..,..PANEL..,sum)[..PANEL..], # to normalize each facet
                   fill = `brand` )) +
  geom_histogram (binwidth = 1, col = "grey" , stat="bin") +
  labs(title = "Distribution of the Product Ratings amongst Brands") +
  labs(x = "Ratings", y = "Percent") + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(1,2, 3,4, 5)) +
  theme(axis.text.x = element_text(size= 8, angle=45), legend.title = element_blank())


#Distribution of Product Rating/percent
ggplot(amazon, aes(x = product_rating)) + 
  geom_bar(aes(y=..prop..,fill=factor(..x..)), alpha = 0.6, color="darkblue", fill="lightblue") +
  scale_y_continuous(labels=scales::percent) +
  labs(x = "Product Ratings", y = "Percent") +
  ggtitle("Distribution of Product Ratings")

#Distribution of Product Rating/amount
ggplot(amazon, aes(x = product_rating)) + 
  geom_bar(aes(y=..prop..,fill=factor(..x..)), alpha = 0.6, color="darkblue", fill="lightblue") +
  scale_y_continuous(labels=scales::percent) +
  labs(x = "Product Ratings", y = "Percent") +
  ggtitle("Distribution of Product Ratings")

#Total price and rating
ggplot(amazonp, aes(product_rating, price), na.rm=TRUE) +
  geom_point(color = "blue")

ggplot(amazonp, aes(product_rating, price), na.rm=TRUE) +
  geom_point(color = "blue") +
  facet_wrap(~price_group)

#Brand and helpful votes 
amazon %>% select(brand, helpfulVotes)%>% 
  na.omit() -> brnd_hv
ggplot(brnd_hv, aes(factor(brand))) +
  geom_bar(position = "dodge", aes(y=(..count..)/sum(..count..)), color="darkblue", fill="lightblue") + 
  scale_y_continuous(labels=scales::percent) +
  ylab("Precentage of HelpfulVotes")+
  xlab("Brands")

#Brand and rating with facetwRAP
price <- amazonp %>% 
  mutate(rating_group = if_else(between(product_rating, 0, 1), "1", 
                                if_else(between(product_rating, 1, 2), "2",
                                        if_else(between(product_rating, 2, 3), "3",
                                                if_else(between(product_rating, 3, 4), "4",
                                                        if_else(product_rating > 5, "5",
                                                                "5"))))), 
         rating_group = if_else(is.na(rating_group), "", rating_group)
  ) %>% 
  rownames_to_column(var = "idd")

price <- na.omit(price)

ggplot(price, aes(x=rating_group, group=brand))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~brand)+
  labs(x="Brand",y="Percentage",title="Product Ratings by Brand")+
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +
  scale_fill_discrete(name="Product Ratings", label=c("1", "2", "3", "4", "5"))


ggplot(amazon, aes(x=reviewer_rating, group=brand))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~brand)+
  labs(x="Brand",y="Percentage",title="Reviewer Ratings by Brand")+
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +
  scale_fill_discrete(name="Reviewer Ratings", label=c("1", "2", "3", "4", "5"))

#Price and brand
ggplot(price, aes(x=brand,group=price_group))+
  geom_bar(color="darkblue", fill="lightblue")+
  facet_wrap(~price_group) +
  scale_fill_brewer(palette="Set2")

ggplot(price, aes(x=price_group ,group=brand))+
  geom_bar(color="darkblue", fill="lightblue")+
  facet_wrap(~brand) +
  scale_fill_brewer(palette="Set2")

ggplot(price, aes(product_rating, price, colour=factor(brand)), na.rm=TRUE) +
  geom_point()+
  scale_fill_discrete(name="Brand")

amazon %>% select(brand, helpfulVotes)%>% 
  na.omit() %>% group_by(brand) %>% 
  summarize(n=sum(helpfulVotes), .groups='drop') -> h

amazon %>% select(brand, totalReviews)%>% 
  na.omit() %>% group_by(brand) %>% 
  summarize(n=sum(totalReviews), .groups='drop') -> t

#helpdul
ht <- merge(h, t, by = "brand")
names(ht)[names(ht)=="n.x"] <- "Helpful_Votes"
names(ht)[names(ht)=="n.y"] <- "Total_Votes"

ht <- ht %>% mutate(Share_of_HelpfulVotes=Helpful_Votes/Total_Votes*100)

ggplot(ht, aes(Total_Votes, Helpful_Votes, color = brand )) +
  geom_point()

amazon %>% select(brand, helpfulVotes)%>% 
  na.omit() -> a

ggplot(a, aes(factor(brand))) +
  geom_bar(position = "dodge", aes(y=(..count..)/sum(..count..)), color="darkblue", fill="lightblue") + 
  scale_y_continuous(labels=scales::percent) +
  ylab("Precentage of HelpfulVotes")+
  xlab("Brands") +
  labs(title = "Helpful Votes by Brand")

install.packages("stopwords")
library(stopwords)
data(stop_words)
stopwords_phone <- c(stopwords("english"),"phone","Samsung","Nokia","Apple","ASUS","OnePlus","Motorola","HUAWEI","Sony","Google","Xiaomi","great","like","good",
                     "samsung","nokia","iphone","apple","asus","oneplus","motorola","huawei","sony","google","xiaomi", "phones")

amazon_clean<- amazon %>% 
  select(brand,body) %>% 
  unnest_tokens(input=body,output=word) %>%
  count(brand,word,sort=T) %>% 
  filter(nchar(word)>3) %>%
  filter(!word %in% stopwords_phone) %>%
  group_by(brand)

amazon_clean2 <- amazon_clean %>% 
  anti_join(stop_words) 


#Most commonwords by brand
amazon_clean2 %>%
  top_n(n=10,n)%>%
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(x=word, y=n,fill=brand)) + 
  geom_col(show.legend=F,col="black")+
  facet_wrap(~brand, ncol=2,scales="free")+
  xlab(NULL) + 
  ylab("Count")+
  ggtitle("Most Common Words in Reviews by Brand") +
  coord_flip()

#Most common words all reviews
amazon_clean2 %>%
  top_n(5)%>%
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(x=reorder(word, -n), y=n)) + 
  geom_col(show.legend=F,color="darkblue", fill="lightblue")+
  xlab(NULL) + 
  ylab("Count")+
  ggtitle("Most Common Words in Reviews") +
  coord_flip()

amazon_clean2 %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(color="darkblue", fill="lightblue") +
  labs(title ="Top 20 Significant Words From All Reviews", x = "Total Number of Occurances", y = "Word") +
  coord_flip()

#New dataset with ranking
amazon_clean_rank<- amazon %>% 
  select(reviewer_rating,body) %>% 
  unnest_tokens(input=body,output=word) %>%
  count(reviewer_rating,word,sort=T) %>% 
  filter(nchar(word)>3) %>%
  filter(!word %in% stopwords_phone) %>%
  group_by(reviewer_rating)

amazon_clean_rank2 <- amazon_clean_rank %>% 
  anti_join(stop_words) 

#Most common words used in reviews by Rank
amazon_clean_rank2 %>%
  top_n(n=10,n)%>%
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(x=word, y=n,fill=reviewer_rating)) + 
  geom_col(show.legend=F,col="black")+
  facet_wrap(~reviewer_rating, ncol=2,scales="free")+
  xlab(NULL) + 
  ylab("Count")+
  ggtitle("Most Common Words in Reviews by Rating") +
  coord_flip()

#Wordcloud for most common words.
library(wordcloud) 
wordcloud(words = amazon_clean2$word, freq = amazon_clean2$n, min.freq = 10, max.words = 175, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(9, "Set1") )


#Wordcloud for most common words by ranking.
#5
amazon_Clean_rank2_5 <-amazon_clean_rank2 %>% filter(reviewer_rating == 5) 
wordcloud(words = amazon_Clean_rank2_5$word, freq = amazon_Clean_rank2_5$n, min.freq = 10, max.words = 100, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(9, "Set1") )

#4
amazon_Clean_rank2_4 <-amazon_clean_rank2 %>% filter(reviewer_rating == 4) 
wordcloud(words = amazon_Clean_rank2_4$word, freq = amazon_Clean_rank2_4$n, min.freq = 10, max.words = 100, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(9, "Set1") )


#3
amazon_Clean_rank2_3 <-amazon_clean_rank2 %>% filter(reviewer_rating == 3) 
wordcloud(words = amazon_Clean_rank2_3$word, freq = amazon_Clean_rank2_3$n, min.freq = 10, max.words = 100, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(9, "Set1") )

#2
amazon_Clean_rank2_2 <-amazon_clean_rank2 %>% filter(reviewer_rating == 2) 
wordcloud(words = amazon_Clean_rank2_2$word, freq = amazon_Clean_rank2_2$n, min.freq = 10, max.words = 100, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(9, "Set1") )

#1
amazon_Clean_rank2_1 <-amazon_clean_rank2 %>% filter(reviewer_rating == 1) 
wordcloud(words = amazon_Clean_rank2_1$word, freq = amazon_Clean_rank2_1$n, min.freq = 10, max.words = 100, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(9, "Set1") )


amazon_clean %>% select(brand) %>% distinct()
#Wordlcloud most common words by brand
amazon_clean2_Samsung <-amazon_clean2 %>% filter(brand == "Samsung") 
wordcloud(words = amazon_clean2_Samsung$word, freq = amazon_clean2_Samsung$n, min.freq = 10, max.words = 60, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(9, "Set1") )

amazon_clean2_Motorola <-amazon_clean2 %>% filter(brand == "Motorola") 
wordcloud(words = amazon_clean2_Motorola$word, freq = amazon_clean2_Motorola$n, min.freq = 10, max.words = 60, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(9, "Set1") )

amazon_clean2_Nokia <-amazon_clean2 %>% filter(brand == "Nokia")
wordcloud(words = amazon_clean2_Nokia$word, freq = amazon_clean2_Nokia$n, min.freq = 10, max.words = 60, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(9, "Set1") )

amazon_clean2_Sony <-amazon_clean2 %>% filter(brand == "Sony")
wordcloud(words = amazon_clean2_Sony$word, freq = amazon_clean2_Sony$n, min.freq = 10, max.words = 60, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(9, "Set1") )

amazon_clean2_Google <-amazon_clean2 %>% filter(brand == "Google")
wordcloud(words = amazon_clean2_Google$word, freq = amazon_clean2_Google$n, min.freq = 10, max.words = 60, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(9, "Set1") )

amazon_clean2_Apple <-amazon_clean2 %>% filter(brand == "Apple")
wordcloud(words = amazon_clean2_Apple$word, freq = amazon_clean2_Apple$n, min.freq = 10, max.words = 60, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(9, "Set1") )

amazon_clean2_Xiaomi <-amazon_clean2 %>% filter(brand == "Xiaomi")
wordcloud(words = amazon_clean2_Xiaomi$word, freq = amazon_clean2_Xiaomi$n, min.freq = 10, max.words = 60, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(9, "Set1") )

amazon_clean2_HUAWEI  <-amazon_clean2 %>% filter(brand == "HUAWEI")
wordcloud(words = amazon_clean2_HUAWEI$word, freq = amazon_clean2_HUAWEI$n, min.freq = 10, max.words = 60, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(9, "Set1") )

amazon_clean2_OnePlus <-amazon_clean2 %>% filter(brand == "OnePlus")
wordcloud(words = amazon_clean2_OnePlus$word, freq = amazon_clean2_OnePlus$n, min.freq = 10, max.words = 60, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(9, "Set1") )

amazon_clean2_ASUS <-amazon_clean2 %>% filter(brand == "ASUS")
wordcloud(words = amazon_clean2_ASUS$word, freq = amazon_clean2_ASUS$n, min.freq = 10, max.words = 60, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(9, "Set1") )

#Sentiment analysis with bing by brand
get_sentiments("bing")

amazon_clean2 %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(linenumber=row_number()) %>%
  count(index=linenumber,sentiment)%>%
  spread(sentiment,n,fill=0) %>%
  group_by(brand) %>% summarize(neg=sum(negative)*-1,
                                pos=sum(positive), .groups="drop") %>%
  gather(type,value,c(neg,pos)) %>% arrange(brand) %>%
  ggplot(aes(x=brand,y=value,fill=brand))+
  geom_col(show.legend=F,aes(fill=ifelse(value>0,"steelblue","orange")),col="black",width=0.5)+geom_hline(yintercept=0)+
  coord_flip()+geom_text(aes(label=brand),size=4)+ylab("<-Negative --Sentiments-- Positive->")+theme_classic()+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line=element_blank(),
    axis.title.x=element_text(hjust=0.69))+xlab("")+ggtitle("Sentiments of reviews Negative/Positive")

#Sentiment analysis with bing by ranking
get_sentiments("bing")

amazon_clean_rank2 %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(linenumber=row_number()) %>%
  count(index=linenumber,sentiment)%>%
  spread(sentiment,n,fill=0) %>%
  group_by(reviewer_rating) %>% summarize(neg=sum(negative)*-1,
                                          pos=sum(positive)) %>%
  gather(type,value,c(neg,pos)) %>% arrange(reviewer_rating) %>%
  ggplot(aes(x=reviewer_rating,y=value,group=reviewer_rating))+
  geom_col(show.legend=F,aes(fill=ifelse(value>0,"steelblue","orange")),col="black",width=0.5)+geom_hline(yintercept=0)+
  coord_flip()+geom_text(aes(label=reviewer_rating),size=4)+ylab("<-Negative --Sentiments-- Positive->")+theme_classic()+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line=element_blank(),
    axis.title.x=element_text(hjust=0.69))+xlab("")+ggtitle("Sentiments of reviews Negative/Positive")

library(reshape2)
amazon_clean2 %>%   
  inner_join(get_sentiments("bing")) %>%   
  count(word, sentiment, sort = TRUE) %>%   
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%   
  comparison.cloud(words = amazon_clean2$word, freq = amazon_clean2$n, min.freq = 0.5, max.words = 100, random.order = FALSE, rot.per = 0.35, color = c("steelblue","orange"))

#sentiment by brand  
amazon_clean2_sentiment <- amazon_clean2 %>%   
  inner_join(get_sentiments("bing")) %>%
  mutate(linenumber=row_number()) %>%
  count(word, index=linenumber,sentiment) %>%   
  spread(sentiment, n, fill = 0) %>%   
  mutate(sentiment = positive - negative)

#most_common_negative <- p %>%
#amazon_clean2_sentiment %>% mutate(linenumber=row_number()) %>% group_by(index = linenumber) %>%    
#summarise(sentiment = sum(sentiment))   

ggplot(amazon_clean2_sentiment, aes(index, sentiment, fill = brand)) +   
  geom_col(show.legend = FALSE) +   
  facet_wrap(~brand, ncol = 2, scales = "free")

#BİNG ANALYSS
bingpositive <- get_sentiments("bing") %>%    
  filter(sentiment == "positive") 

A_bingpositive <-amazon_clean2 %>%   
  inner_join(bingpositive) %>%   
  count(word, sort = TRUE)

A_bingpositive %>%   
  count(word, sort = TRUE)

bingnegative <- get_sentiments("bing") %>%    
  filter(sentiment == "negative")

A_bingnegative <-amazon_clean2 %>%   
  inner_join(bingnegative) %>%   
  count(word, sort = TRUE) 

wordcloud(words = A_bingnegative$word, freq = A_bingnegative$n, min.freq = 1, max.words = 100, random.order = FALSE, rot.per = 0.35,color = c("steelblue"))
wordcloud(words = A_bingpositive$word, freq = A_bingpositive$n, min.freq = 1, max.words = 100, random.order = FALSE, rot.per = 0.35,color = c("orange"))


negative <- A_bingnegative %>% 
  select(brand,n) %>% 
  na.omit() %>% group_by(brand) %>% 
  summarize(n=sum(n), .groups="drop") 

positive <- A_bingpositive %>% 
  select(brand,n) %>% 
  na.omit() %>% group_by(brand) %>% 
  summarize(n=sum(n), .groups="drop") 

#Lolipop positive
ggplot(positive, aes(x=brand, y=n)) +
  geom_segment( aes(x=brand, xend=brand, y=0, yend=n), color="purple") +
  geom_point( color="pink", size=9) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Number of Positive Words")

#Lolipop Negative
ggplot(negative, aes(x=brand, y=n)) +
  geom_segment( aes(x=brand, xend=brand, y=0, yend=n), color="purple") +
  geom_point( color="pink", size=9) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Number of Negative Words")




AR_bingpositive <-amazon_clean_rank2 %>%   
  inner_join(bingpositive) %>%   
  count(word, sort = TRUE)

AR_bingnegative <-amazon_clean_rank2 %>%   
  inner_join(bingnegative) %>%   
  count(word, sort = TRUE) 


negative_r <- AR_bingnegative %>% 
  select(reviewer_rating,n) %>% 
  na.omit() %>% group_by(reviewer_rating) %>% 
  summarize(n=sum(n), .groups="drop") 

positive_r <- AR_bingpositive %>% 
  select(reviewer_rating,n) %>% 
  na.omit() %>% group_by(reviewer_rating) %>% 
  summarize(n=sum(n), .groups="drop") 

#LOLİPOPPPPP

ggplot(negative_r, aes(x=reviewer_rating, y=n)) +
  geom_segment( aes(x=reviewer_rating, xend=reviewer_rating, y=0, yend=n), color="purple") +
  geom_point( color="pink", size=9) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Number of Negative Words")

ggplot(positive_r, aes(x=reviewer_rating, y=n)) +
  geom_segment( aes(x=reviewer_rating, xend=reviewer_rating, y=0, yend=n), color="purple") +
  geom_point( color="pink", size=9) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Number of Positive Words")

####APPLE VS SAMSUNG####

samsung <- amazon %>%
  filter(brand == "Samsung")

samsung2 <- amazon %>%
  filter(year >= 2017)

samsung_apple <- amazon %>% filter(brand== "Samsung"  | brand== "Apple")

samsung_apple %>% filter(year >= 2017)

samsung_apple %>% select(brand) %>% distinct()

samsung_apple<- samsung_apple %>% 
  mutate(price_group = if_else(between(price, 0, 250), "low_price", 
                               if_else(between(price, 250, 450), "medium_price",
                                       if_else(price > 450, "high_price",
                                               "unknown_price"))), 
         price_group = if_else(is.na(price_group), "unknown_price", price_group)
  ) %>% 
  rownames_to_column(var = "id")

ggplot(samsung_apple,aes(x=reviewer_rating,group=brand))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~brand)+
  labs(x="Rating",y="Percentage",title="Apple vs. Samsung")+
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +
  scale_fill_discrete(name="Rating")

ggplot(samsung_apple,aes(x=brand,group=price_group))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~price_group)+
  labs(x="",y="Percentage",title="Apple vs. Samsung")+
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +
  scale_fill_discrete(name="Brand")

ggplot(samsung_apple,aes(x=price_group,group=brand))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~brand)+
  labs(x="",y="Percentage",title="Apple vs. Samsung")+
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +
  scale_fill_discrete(name="Price Group", label=c("High Price", "Low Price", "Medium Price"))



#APPLE VS: SAMSUNG SENTIMENT

samsung_apple_clear<- samsung_apple %>% 
  select(brand,body) %>% 
  unnest_tokens(input=body,output=word) %>%
  count(brand,word,sort=T) %>% 
  filter(nchar(word)>3) %>%
  filter(!word %in% stopwords_phone) %>%
  group_by(brand)

samsung_apple_clear2 <- samsung_apple_clear %>% 
  anti_join(stop_words) 

samsung_apple_clear2 %>%
  top_n(n=10,n)%>%
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(x=word, y=n,fill=brand)) + 
  geom_col(show.legend=F,col="black")+
  facet_wrap(~brand, ncol=2,scales="free")+
  xlab(NULL) + 
  ylab("Count")+
  ggtitle("Most Common Words in Reviews by Brand") +
  coord_flip()


samsung_apple_clear2_Samsung <-samsung_apple_clear2 %>% filter(brand == "Samsung") 
wordcloud(words = samsung_apple_clear2_Samsung$word, freq = samsung_apple_clear2_Samsung$n, min.freq = 10, max.words = 150, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(9, "Set1") )

samsung_apple_clear2_Apple <-samsung_apple_clear2 %>% filter(brand == "Apple") 
wordcloud(words = samsung_apple_clear2_Apple$word, freq = samsung_apple_clear2_Apple$n, min.freq = 10, max.words = 150, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(9, "Set1") )

samsung_apple_clear2 %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(linenumber=row_number()) %>%
  count(index=linenumber,sentiment)%>%
  spread(sentiment,n,fill=0) %>%
  group_by(brand) %>% summarize(neg=sum(negative)*-1,
                                pos=sum(positive)) %>%
  gather(type,value,c(neg,pos)) %>% arrange(brand) %>%
  ggplot(aes(x=brand,y=value,fill=brand))+
  geom_col(show.legend=F,aes(fill=ifelse(value>0,"steelblue","orange")),col="black",width=0.5)+geom_hline(yintercept=0)+
  coord_flip()+geom_text(aes(label=brand),size=4)+ylab("<-Negative --Sentiments-- Positive->")+theme_classic()+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line=element_blank(),
    axis.title.x=element_text(hjust=0.69))+xlab("")+ggtitle("Sentiments of reviews Negative/Positive")


samsung_apple_clear2_sentiment <- samsung_apple_clear2 %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort=TRUE)

ggplot(samsung_apple_clear2_sentiment,aes(x=sentiment,group=brand))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~brand)+
  labs(x="Sentiment",y="Percentage",title="Sentiment Anlysis - Apple vs. Samsung")+
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +
  scale_fill_discrete(name="Sentiment", label=c("Negative", "Positive"))


