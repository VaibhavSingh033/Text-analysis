# RT parse ----------------------------------------------------------------
library(tidyverse) # general-purpose data_RT wrangling
library(rvest) # parsing of HTML/XML files
library(stringr) # string manipulation
library(rebus) # verbose regular expressions
library(lubridate) # eases DateTime manipulation
library(progress)
library(tidyverse) # general utility & workflow functions
library(tidytext) # tidy implimentation of natural language processing (NLP) methods
library(topicmodels) # for latent dirichlet allocation (LDA) topic modelling
library(tm) # general text mining functions
library(SnowballC) # for stemming
library(wordcloud)
library(e1071)
library(syuzhet)
library(janitor)
library(gridExtra) #For grid arrange

quiet <- TRUE
# get_sentiments("nrc")
# search query
query <- "New Zealand"


# in case query contains >1 word, substitue " " with "+"
if (length(unlist(strsplit(query, " "))) > 1) {
  query <- sub(" ", "+", query)
}

# create sequence of date
dates_seq <- seq(as.Date("1-02-2019", format = "%d-%m-%Y"), to = as.Date("16-03-2019", format = "%d-%m-%Y"), by = "days")

# data_RT frame to store all texts
data_RT <- data.frame("text" = NA, "date"=NA)

N <- length(dates_seq)
# N <- 10

pb <- progress_bar$new(total = N)


for (k in 1:N) {
  # time span
  temp_date <- as.character(dates_seq[k])
  start_date <- paste(substr(temp_date, 9, 10),
                      substr(temp_date, 6, 7),
                      substr(temp_date, 1, 4),
                      sep = "-"
  )
  end_date <- start_date
  
  # create url of search results
  url <- paste("https://www.rt.com/search?q=", query, "&type=&df=",
               start_date, "&dt=", end_date,
               sep = ""
  )
  
  # parse webpage
  webpage <- read_html(url)
  
  # extract all links in the search results
  all_links <- webpage %>%
    html_nodes(".link_hover") %>%
    html_attr("href")
  
  # extract all links in the search results
  all_timestamps <- webpage %>%
    html_nodes(".top") %>%
    html_nodes(".display-date") %>%
    html_attr('datetime')
  
  all_links <- unique(all_links)
  
  if (length(all_links) == 0) {
    pb$tick()
    next
  }
  
  # data_RT frame to store texts
  
  # data_bbc frame to store texts
  text_links <- data.frame("text" = rep(NA, length(all_links)),
                           "date" = end_date)
  
  # run through all links and scrape texts
  for (i in 1:length(all_links)) {
    # create url
    url_temp <- paste("https://www.rt.com", all_links[i], sep = "")
    # parse webpage
    webpage_temp <- read_html(url_temp)
    # scrape texts
    text_temp <- webpage_temp %>%
      html_nodes(".text") %>%
      html_text()
    
    if (length(text_temp) == 0) next
    # run through texts and remove unwanted text (text between "\n" in case of Russia Today)
    for (j in 1:length(text_temp)) {
      # find positions of "\n"
      ind <- gregexpr("\n", text_temp[j])[[1]]
      # proceed to the next chunk of text in case nothing was found
      if (min(ind) == -1) next
      # extract text before the first "\n"
      part_1 <- substr(text_temp[j], 1, min(ind) - 1)
      # extract text after the last "\n"
      part_2 <- substr(text_temp[j], max(ind) + 1, nchar(text_temp[j]))
      # combine both parts
      text_temp[j] <- paste(part_1, part_2, sep = " ")
    }
    # save cleaned text
    text_links$text[i] <- paste(text_temp, collapse = " ")
    # Sys.sleep(2)
  }
  data_RT <- rbind(data_RT, text_links)
  pb$tick()
}

# remove NA
names(data_RT)
dim(data_RT)
#data_RTframe for RT Text
data_RT <- na.omit(data_RT)
data_RT$date <- dmy(data_RT$date)

# Article Datewise mentioning NewZealand ----------------------------------

ggplot(data_RT, aes(as.factor(date), group=1))+geom_line(stat="count")+labs(x="",title="Number of mentions of New Zealand in RT", y="Count")+scale_y_continuous(breaks=c(1,3,7,10))+
  theme_light()


data_RT$event <- ifelse(data_RT$date < "2019-03-15","Before Attacks","After Attacks")

before_attacks <- subset(data_RT, event=="Before Attacks")
after_attacks <- subset(data_RT, event=="After Attacks")

# Sentiment Analysis BEFORE ------------------------------------------------------

word.df <- as.vector(before_attacks$text)

emotion.df <- get_nrc_sentiment(word.df)

emotion.df2 <- cbind(before_attacks, emotion.df)

#head(emotion.df2)

total <- emotion.df2 %>% adorn_totals("row")
total_gg_bef <- as.data.frame(base::t(total[nrow(total), 4:ncol(total)]))
total_gg_bef$names <- rownames(total_gg_bef)
names(total_gg_bef) <- c("count", "emotion")
total_gg_bef <- total_gg_bef[-1,]
total_gg_bef$count <- as.numeric(as.character(total_gg_bef$count))

RT_GG_before <- ggplot(total_gg_bef, aes(x = reorder(emotion, -count), count, fill = rownames(total_gg_bef))) +  geom_bar(stat = "identity") + theme_bw() + labs(title = "Sentiment for NZ Before Attacks", x = "", y = "Frequency") + guides(fill = FALSE) + theme(plot.title = element_text(hjust = 0.5))

# Sentiment Analysis After ------------------------------------------------------

word.df <- as.vector(after_attacks$text)

emotion.df <- get_nrc_sentiment(word.df)

emotion.df2 <- cbind(after_attacks, emotion.df)

#head(emotion.df2)

total <- emotion.df2 %>% adorn_totals("row")
total_gg_after <- as.data.frame(base::t(total[nrow(total), 3:ncol(total)]))
total_gg_after$names <- rownames(total_gg_after)
names(total_gg_after) <- c("count", "emotion")

total_gg_after <- total_gg_after[-1,]
total_gg_after$count <- as.numeric(as.character(total_gg_after$count))

RT_GG_after <- ggplot(total_gg_after, aes(x = reorder(emotion, -count), count, fill = rownames(total_gg_after))) +  geom_bar(stat = "identity") + theme_bw() + labs(title = "Sentiment for NZ After Attacks", x = "", y = "Frequency") + guides(fill = FALSE) + theme(plot.title = element_text(hjust = 0.5))


grid.arrange(RT_GG_before,RT_GG_after)


# Topic Modeling ----------------------------------------------------------
newsCorpus <- Corpus(VectorSource(data_RT$text))
newsCorpus <- tm_map(newsCorpus, content_transformer(tolower))
newsCorpus <- tm_map(newsCorpus, removeNumbers)
# newsCorpus <- tm_map(newsCorpus, stemDocument)
newsCorpus <- tm_map(newsCorpus, removePunctuation)
newsCorpus <- tm_map(newsCorpus, removeWords,c("the",stopwords("english")))
newsCorpus <- tm_map(newsCorpus, stripWhitespace)

DTM <- DocumentTermMatrix(newsCorpus)
# check dimension of this object
dim(DTM)

# remove any empty rows in our document term matrix (if there are any
# we'll get an error when we try to run our LDA)
# get the index of each unique value
unique_indexes <- unique(DTM$i)
# get a subset of only those indexes
DTM <- DTM[unique_indexes, ]


# run LDA
lda <- LDA(DTM, k = 3, control = list(seed = 1234))
# convert the LDA output to a tidy
topics <- tidy(lda, matrix = "beta")

# get the top ten terms for each topic
top_terms <- topics %>% # take the topics data_RT frame and..
  group_by(topic) %>% # treat each topic as a different group
  top_n(20, beta) %>% # get the top 10 most informative words
  ungroup() %>% # ungroup
  arrange(topic, -beta) # arrange words in descending informativeness


top_terms <- top_terms %>%
  mutate(event=ifelse(topic==1,"Christchurch Shooting (Attack Day)",ifelse(topic==2,"Global NZ News (Before Shooting)","Senator Egging (After Attacks)")))

## Reordering top_terms$event
top_terms$event <- factor(top_terms$event, levels=c("Global NZ News (Before Shooting)", "Christchurch Shooting (Attack Day)", "Senator Egging (After Attacks)"))

# plot the top ten terms for each topic in order
RT_topic <- top_terms %>%
  # take the top terms
  mutate(term = reorder(term, beta)) %>% # sort terms by beta value
  ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
  geom_col(show.legend = FALSE) + # as a bar plot
  facet_wrap(~event, scales = "free") + # which each topic in a seperate plot
  labs(x = NULL, y = "Beta", title="Topic Modeling New Zealand") + # no x label, change y label
  coord_flip() # turn bars sideways

RT_topic

#WORD CLOUD FOR NZ MENTION
dtm <- TermDocumentMatrix(newsCorpus)

m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d_bbc <- data.frame(word = names(v),freq=v)
head(d, 20)

wordcloud(words = d_bbc$word, freq = d_bbc$freq, min.freq = 1,
          max.words=200, 
          colors=brewer.pal(8, "Dark2"))
