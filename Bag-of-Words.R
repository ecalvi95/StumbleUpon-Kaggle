library(tidyverse) #for reaching tsv
library(readr)
library(tidytext) #for word of bags
df <- read_tsv('~/Babson/Spring/MSB6300/Intro Project/train.tsv')

#removing all variables except boilerplate

df$avglinksize = NULL
df$commonlinkratio_1 = NULL
df$commonlinkratio_2 = NULL
df$commonlinkratio_3 = NULL
df$commonlinkratio_4 = NULL
df$compression_ratio = NULL
df$embed_ratio = NULL
df$frameTagRatio = NULL
df$hasDomainLink = NULL
df$html_ratio = NULL
df$image_ratio = NULL
df$lengthyLinkDomain = NULL
df$linkwordscore = NULL
df$non_markup_alphanum_characters = NULL
df$numberOfLinks = NULL
df$numwords_in_url = NULL
df$parametrizedLinkRatio = NULL
df$spelling_errors_ratio = NULL
df$url = NULL # 
df$urlid = NULL
df$framebased = NULL
df$is_news = NULL
df$news_front_page = NULL
df$alchemy_category = NULL
df$alchemy_category_score = NULL

df = df %>% unique() %>% mutate(ID = c(1:nrow(df)))
#delete duplicates if exist --> aren't any

#creates an ID column for all
outcome = df %>% select(ID, label)

#zipfs' law
#breaks up the words and creates the count table for each word
#there's a plot to see how the words rank - the top dot in the very left is "the"

counts = df %>% unnest_tokens(word, 'boilerplate') %>%
  count(word, sort = TRUE)

counts = counts %>% mutate(rank = c(1:nrow(counts)))

ggplot(counts, aes(x= rank, y = n)) + geom_point() + scale_x_log10() + scale_y_log10()

View(counts)

#removes any stop words (filler words) and also removes any words that were used less than 5 times
#if useful, we could adjust the filter
word_list = df %>% unnest_tokens(word, 'boilerplate') %>%
  anti_join(stop_words) %>% #remove stop words (preprogrammed)
  count(word, sort = TRUE) %>%
  filter(n >= 500) %>% #remove less than 500
  pull(word)

#I think bag_of_words is probably your best bet for association rules
#it's just really big and hard to navigate to check how it works
#Lemme know what you think! 

bag_of_words = df %>% unnest_tokens(word, 'boilerplate') %>%
  filter(word %in% word_list) %>%
  count(ID, word) %>%
  pivot_wider(id_cols = ID, names_from=word, values_from=n) %>%
  right_join(outcome, by = 'ID')%>%
  map_df(replace_na, 0) %>%
  select(-ID)

#this will show you for each of your top words, how often they were 0 or 1
word_plus_labels = df %>%
  unnest_tokens(word, 'boilerplate') %>%
  filter(word %in% word_list) %>%
  count(label, word, sort = TRUE) %>%
  pivot_wider(names_from = label, values_from = n) %>%
  map_df(replace_na, 0.0001)

View(word_plus_labels)

#creates the ratio table so you can see the ratio of 1 (evergreen) to 0 (not) for each word
#the words with the highest ratios will not be the most popular words in the data set
# you can see this by taking a word and searching it in the counts table

ratio = df %>%
  unnest_tokens(word, 'boilerplate') %>%
  filter(word %in% word_list) %>%
  count(label, word, sort = TRUE) %>%
  pivot_wider(names_from = label, values_from = n) %>%
  map_df(replace_na, 0.0001) %>% # you cant replace NA with 0, so its close to 0
  mutate(ratio = `1`/`0`) %>%
  arrange(desc(ratio)) %>%
  select(word, ratio)

View(ratio)
#top words -> sinus, springform, gnocchi, dumplings
#if you search them in the words_plus_labels table - you can see that
#they basically don't have any non evergreen labels - always considered relevent when used

#finding the most informative features


#identifies the top 10 words in ratio and bottom 10 words
most_informative_words = combine(head(ratio$word, 10), tail(ratio$word, 10))
#probably won't need bottom

clean_counts <-counts %>% # takes the count table and removes filler words
  anti_join(stop_words)

View(clean_counts)

ggplot(clean_counts, aes(x= rank, y = n)) + geom_point() + scale_x_log10() + scale_y_log10()
#the first dot is "1", first dot is "2", third dot "body"
#of course we don't consider these words to be relevant, if we felt the chart would be useful for something
#we could try to remove those first words before "recipe"

#bunch of food words
# I think "cup" goes with "world" a little bit lower --> World Cup -> there was one in 2008
# not sure if we want to explore other options outside food
#perhaps we can find a few other topics that people found interesting

library(data.table)
fwrite(bag_of_words,"~/Babson/Spring/MSB6300/Intro Project/bag_of_words_500.csv" )
#for exporting
