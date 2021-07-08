##########INITIAL EXPLORATION##########

#Initialisation
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(lubridate)) install.packages("lubridate")
if(!require(tidytext)) install.packages("tidytext")
if(!require(textdata)) install.packages("textdata")
if(!require(gridExtra)) install.packages("gridExtra")

library(tidyverse)
library(lubridate)
library(tidytext)
library(textdata)
library(gridExtra)

#Import saved data if needed
load(file = "combined_data.RData")
nrow(combined_data)

#Look at raw data for views and subscribers
p1 <- combined_data %>% 
  ggplot(aes(x = views)) +
  geom_density() +
  labs(x = "Number of views",
       title = "Density plot of the number of views in the sample")

p2 <- combined_data %>% 
  ggplot(aes(x = subscribers)) +
  geom_density() +
  labs(x = "Number of subscribers",
       title = "Density plot of the number of subscribers in the sample")

grid.arrange(p1, p2)

#Look at raw data for views and subscribers, log10-transformed
p1 <- combined_data %>% 
  filter(views > 0) %>%
  ggplot(aes(x = views)) +
  geom_density() +
  scale_x_continuous(trans = "log10") +
  labs(x = "Number of views",
       title = "Density plot of the number of views in the sample, log10-transformed")

p2 <- combined_data %>%  
  filter(subscribers > 0) %>%
  ggplot(aes(x = subscribers)) +
  geom_density() +
  scale_x_continuous(trans = "log10") +
  labs(x = "Number of subscribers",
       title = "Density plot of the number of views in the sample, log10-transformed")

grid.arrange(p1, p2)

#Visualize views as a function of subscribers
combined_data %>% 
  filter(views > 0 & subscribers > 0) %>%
  ggplot(aes(x = subscribers, y = views)) +
  geom_point(alpha = 0.1) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  labs(x = "Number of subscribers",
       y = "Number of views",
       title = "Number of views by number of subscribers, log10 transformed")

#Add log-transformed metrics to our data set
wrangled_data <- combined_data %>%
  filter(subscribers > 0 & views > 0) %>%
  mutate(log10_views = log10(views), 
         log10_subs = log10(subscribers))

n_removed <- nrow(wrangled_data) - nrow(combined_data)

#Look at views as a function of age
wrangled_data <- wrangled_data %>% 
  mutate(age_w = floor(age / 7))

borders <- wrangled_data %>% 
  filter(age_w < 64) %>%
  group_by(age_w) %>% 
  summarise(mean_v = mean(log10_views)) %>%
  summarize(mean = mean(mean_v), sd = sd(mean_v))

wrangled_data %>%
  group_by(age_w) %>% 
  summarise(mean_v = mean(log10_views)) %>%
  ggplot(aes(x = age_w, y = mean_v)) +
  geom_point() +
  geom_vline(xintercept = 63.5, col = "red") +
  geom_hline(yintercept = borders$mean, col = "red") +
  geom_hline(yintercept = borders$mean + 2 * borders$sd, col = "red", linetype = "88") +
  labs(x = "Age in weeks",
       y = "Average of log10(number of views)",
       title = "Overview of the average number of views over time, using weekly buckets") +
  geom_text(aes(x = 5, y = 4.23, label = "average from 0
                to 63 weeks"), col = "red") +
  geom_text(aes(x = 7, y = 4.52, label = "average + 2 x SD 
                from 0 to 63 weeks"), col = "red")

#Look at subscribers as a function of age
age_w_range <- 1:63

borders <- wrangled_data %>% 
  filter(age_w %in% age_w_range) %>%
  group_by(age_w) %>% 
  summarise(mean_s = mean(log10_subs)) %>%
  summarize(mean = mean(mean_s), sd = sd(mean_s))

wrangled_data %>%
  group_by(age_w) %>% 
  summarise(mean_s = mean(log10_subs)) %>%
  ggplot(aes(x = age_w, y = mean_s)) +
  geom_point() +
  geom_vline(xintercept = 63.5, col = "red") +
  geom_vline(xintercept = 0.5, col = "red") +
  geom_hline(yintercept = borders$mean, col = "red") +
  geom_hline(yintercept = borders$mean + 2 * borders$sd, col = "red", linetype = "88") +
  labs(x = "Age in weeks",
       y = "Average of log10(number of subscribers)",
       title = "Overview of the distribution of average number of subscribers in 
       our sample, based on the age of the video") +
  geom_text(aes(x = 68, y = 4.7, label = "average from 1
                to 63 weeks"), col = "red") +
  geom_text(aes(x = 7, y = 4.88, label = "average + 2 x SD 
                from 1 to 63 weeks"), col = "red")

#Filter on the age range
wrangled_data <- wrangled_data %>% 
  filter(age_w %in% age_w_range)

n_removed <- nrow(wrangled_data) - nrow(combined_data)

##########DATA WRANGLING##########
#Import a list of categories
load(file = "yt_categories.RData")

#Define regex patterns to look for
regex_text <- "â\200\231\\w*|[éæãâœðŸŒŽ]|&#39;\\w*|&quot;|\\d|http.*|[^\\w\\s#]"
regex_length <- "[^a-zA-Z0-9\\s]"
regex_capw <- "[A-Z]\\w*"
regex_capl <- "[A-Z]"

wrangled_data <- wrangled_data %>%
  filter(subscribers > 0 & views > 0 & likes > 0 & dislikes > 0 & comments > 0) %>%
  mutate(
    
    #Clean and trim the title and description
    title_l_trim = str_trim(
      str_replace_all(title, 
                      regex_length,
                      "")),
    title_trim = str_trim(
      str_replace_all(title, 
                      regex_text,
                      "")),
    descr_trim = str_trim(
      str_replace_all(description, 
                      regex_text,
                      "")),
    
    #Count the characters in the title, knowing the max is 100
    title_l = ifelse(
      str_count(title_l_trim) <= 100, 
      str_count(title_l_trim),
      100),
    
    #Count the words in the title
    n_titl = str_count(
      title_trim,
      "\\w+")
  ) %>%
  
  filter(title_l > 0 & n_titl > 0) %>%
  
  mutate(
    #Split the individual title words
    title_c = str_split(
      title_trim,
      "\\s+"),
    
    #Count the amount of hashtags in the title
    n_titl_hashtags = str_count(
      title_trim,
      "#.[^\\s]*"),
    
    #Split the individual description words
    description_c = str_split(
      descr_trim,
      "\\s+"),
    
    #Count the amount of hashtags in the description
    n_desc_hashtags = str_count(
      descr_trim,
      "#.[^\\s]*"),
    
    #Count the words in the description, excluding hashtags
    n_desc = str_count(
      descr_trim,
      "\\w+") - 
      n_desc_hashtags,
    
    #Count the number of capitalized words in the title
    n_cap_words = str_count(title_trim, 
                            pattern = regex_capw),
    
    #Count the number of capitalized letters in the title, excluding at the start of words
    n_cap_letters = str_count(title_trim, 
                              pattern = regex_capl) - n_cap_words,
    
    #Count the proportion of capitalised elements in the title (letters and words)
    pcap_tit_l = ifelse((n_cap_letters / (title_l - n_cap_words)) > 1, 
                        1,
                        n_cap_letters / (title_l- n_cap_words)),
    
    pcap_tit_w = ifelse(n_cap_words / n_titl > 1, 
                        1, 
                        n_cap_words / n_titl),
    
    pcap_tit_w = ifelse(!is.na(pcap_tit_w), 
                        pcap_tit_w, 
                        0),
    
    #Group likes and dislikes together
    votes = likes + dislikes,
    
    #Add additional potentially useful ratios
    engagement = (comments + votes) / views,
    likes_p_v = likes / views,
    dislikes_p_v = dislikes / views,
    votes_p_c = votes / comments,
    rating_odds = likes / dislikes
    
  ) %>%
  
  #Remove unnecessary items
  select(-title_l_trim,
         -descr_trim,
         -n_cap_letters,
         -n_cap_words,
         -votes) %>% 
  
  #Add category names
  left_join(yt_categories, by = "category_id")

n_removed <- nrow(wrangled_data) - nrow(combined_data)

#Remove videos without any topic assigned
wrangled_data <- wrangled_data %>%
  filter(topics_vid != "list()") %>%
  rowwise() %>%
  filter(!is.null(topics_vid)) #Changed the data saving process at some point, so both required

n_removed <- nrow(wrangled_data) - nrow(combined_data)

#Assess if at least one of the video topics is aligned with the channel's
topics_alig <- sapply(1:nrow(wrangled_data), function(i){
  mean(wrangled_data$topics_vid[[i]] %in% wrangled_data$topics_ch[[i]])
})

topics_alig <- topics_alig %>% 
  as_tibble() %>%
  mutate(topics_alig = ifelse(!is.na(topics_alig), topics_alig, 1),
         topics_alig = ifelse(topics_alig > 0, 1, 0),
         topics_alig = as.factor(topics_alig)) %>%
  pull(topics_alig)

wrangled_data <- wrangled_data %>% 
  bind_cols(topics_alig = topics_alig)

#Removing time selection bias
age_stable <- 27

mean <- wrangled_data %>%
  filter(age_w >= age_stable) %>%
  group_by(age_w) %>%
  summarize(n = n()) %>%
  pull(n) %>%
  mean()

wrangled_data %>%
  group_by(age_w) %>%
  summarize(n = n()) %>% 
  ggplot(aes(x = age_w, y = n)) + 
  geom_point() + 
  geom_vline(xintercept = age_stable - 0.5, col = "red") +
  geom_hline(yintercept = mean, col = "red") +
  labs(x = "Age in weeks",
       y = "Number of observations extracted",
       title = "Number of observations (videos) in our sample,
       by age of the video in weeks") +
  geom_text(aes(x = 10, y = 345, label = paste("average from", 
                                               age_stable,
                                               "to 63 weeks")),
            col = "red")

wrangled_data %>%
  filter(age_w >= age_stable) %>%
  group_by(age_w) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = n)) +
  geom_density() +
  labs(x = "Number of observations",
       title = paste("Density plot of the number of observations extracted,
       for videos aged between", age_stable, "and 63 weeks"))

s_test <- wrangled_data %>%
  filter(age_w >= age_stable) %>%
  group_by(age_w) %>%
  summarize(n = n()) %>%
  pull(n) %>%
  shapiro.test()

#Remove time bias in sample size
mean_r <- round(mean)

set.seed(5, sample.kind = "default")

wrangled_data_u <- wrangled_data %>%
  filter(age_w < age_stable) %>%
  group_by(age_w) %>%
  sample_n(min(n(), mean_r)) %>%
  ungroup()

wrangled_data_o <- wrangled_data %>%
  filter(age_w >= age_stable)

wrangled_data <- bind_rows(wrangled_data_u, wrangled_data_o)

n_removed <- nrow(wrangled_data) - nrow(combined_data)

rm(wrangled_data_u, wrangled_data_o)

#Other predictors
names(wrangled_data)

p1 <- wrangled_data %>%
  ggplot(aes(x = title_l)) +
  geom_bar() +
  labs(x = "Title length in characters")

p2 <- wrangled_data %>%
  ggplot(aes(x = n_titl)) +
  geom_bar() +
  labs(x = "Title length in words")

p3 <- wrangled_data %>%
  ggplot(aes(x = n_titl_hashtags)) +
  geom_bar() +
  labs(x = "Number of hashtags in title")

p4 <- wrangled_data %>%
  ggplot(aes(x = n_desc_hashtags)) +
  geom_bar() +
  labs(x = "Number of hashtags in description")

p5 <- wrangled_data %>%
  ggplot(aes(x = pcap_tit_l)) +
  geom_density() +
  labs(x = "% of capital letters in title")

p6 <- wrangled_data %>%
  ggplot(aes(x = pcap_tit_w)) +
  geom_density() +
  labs(x = "% of capitalized words in title")

p7 <- wrangled_data %>%
  ggplot(aes(x = engagement)) +
  geom_density() +
  labs(x = "Engagement ratio")

p8 <- wrangled_data %>%
  ggplot(aes(x = likes_p_v)) +
  geom_density() +
  labs(x = "Likes per view")

p9 <- wrangled_data %>%
  ggplot(aes(x = dislikes_p_v)) +
  geom_density() +
  labs(x = "Dislikes per view")

p10 <- wrangled_data %>%
  ggplot(aes(x = votes_p_c)) +
  geom_density() +
  labs(x = "Votes per comment")

p11 <- wrangled_data %>%
  ggplot(aes(x = rating_odds)) +
  geom_density() +
  labs(x = "Rating odds ratio")

p12 <- wrangled_data %>%
  ggplot(aes(x = topics_alig)) +
  geom_bar() +
  labs(x = "Alignment of topics")

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)

#With log10 transforms

p7 <- wrangled_data %>%
  ggplot(aes(x = engagement)) +
  geom_density() +
  scale_x_continuous(trans = "log10") +
  labs(x = "log10(Engagement ratio)")

p8 <- wrangled_data %>%
  ggplot(aes(x = likes_p_v)) +
  geom_density() +
  scale_x_continuous(trans = "log10") +
  labs(x = "log10(Likes per view)")

p9 <- wrangled_data %>%
  ggplot(aes(x = dislikes_p_v)) +
  geom_density() +
  scale_x_continuous(trans = "log10") +
  labs(x = "log10(Dislikes per view)")

p10 <- wrangled_data %>%
  ggplot(aes(x = votes_p_c)) +
  geom_density() +
  scale_x_continuous(trans = "log10") +
  labs(x = "log10(Votes per comment)")

p11 <- wrangled_data %>%
  ggplot(aes(x = rating_odds)) +
  geom_density() +
  scale_x_continuous(trans = "log10") +
  labs(x = "log10(Rating odds ratio)")

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)
remove(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)

#Create final wrangled data set
exploration_data <- wrangled_data %>%
  mutate(engagement = log10(engagement),
         likes_p_v = log10(likes_p_v),
         dislikes_p_v = log10(dislikes_p_v),
         votes_p_c = log10(votes_p_c),
         rating_odds = log10(rating_odds))

#Save data
save(exploration_data, file = "exploration_data.RData")

invisible(gc())

##########DATA EXPLORATION ON RESIDUALS##########

#Initialisation
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(caret)) install.packages("caret")
if(!require(moments)) install.packages("moments")

library(ggrepel)
library(caret)
library(moments)
options(ggrepel.max.overlaps = Inf)

#Use previously wrangled data
load(file = "exploration_data.RData")

#Visualize views as a function of time in our sample, within our selected range
exploration_data %>%
  group_by(age_w) %>%
  summarize(mean_v = mean(log10_views)) %>%
  ggplot(aes(x = age_w, y = mean_v)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "loess") +
  labs(x = "Age in weeks",
       y = "Average of log10(number of views)",
       title = "Overview of views as a function of time")

#Neutralize time effect on log10(views)
time_mean <- exploration_data %>%
  group_by(age_w) %>%
  summarize(time_pred = mean(log10_views))

exploration_data <- exploration_data %>%
  left_join(time_mean, by = "age_w") %>%
  mutate(resid_time = log10_views - time_pred)

#Plot the time residuals
exploration_data %>%
  ggplot(aes(x = resid_time)) + 
  geom_density() +
  labs(x = "Time residuals (log10 basis)",
       title = "Density plot of the residuals, after neutralizing the time effect")

exploration_data %>%
  ggplot(aes(x = log10_subs, y = resid_time)) +
  geom_point(alpha = 0.1) +
  labs(x = "Number of subscribers (log10 basis)",
       y = "Time residuals (log10 basis)",
       title = "Overview of residuals by number of subscribers")

#Fit a model to resid_time as f(log10_subs)
fit_subs <- exploration_data %>% 
  train(resid_time ~ log10_subs, data = ., 
        method = "glm")

b <- round(fit_subs$finalModel$coefficients[1], 3)
a <- round(fit_subs$finalModel$coefficients[2], 3)

pred_subs <- predict(fit_subs, exploration_data)

exploration_data %>%
  ggplot(aes(x = log10_subs, y = resid_time)) +
  geom_point(alpha = 0.1) + 
  geom_point(aes(x = log10_subs), y = pred_subs, col = "red") +
  labs(x = "Number of subscribers (log10 basis)",
       y = "Time residuals (log10 basis)",
       title = "Overview of residuals by number of subscribers, 
       with linear regression estimate in red")

#Compute our final residuals, i.e. the views we will compare to the average expectation (0) for videos with comparable age and number of subscribers
exploration_data <- exploration_data %>%
  mutate(resid_views = resid_time - pred_subs)

#Plot residuals
mean_resid <- mean(exploration_data$resid_views)
sd_resid <- sd(exploration_data$resid_views)

colors <- c("Observed distribution" = "black", "Theoretical distribution" = "red")

exploration_data %>%
  ggplot(aes(x = resid_views, color = "Observed distribution")) + 
  geom_density() +
  geom_line(data = tibble(x =  seq(-4, 4, length=1000),
                          y = dnorm(x, mean = 0, sd = sd_resid)),
            aes(x, y, color = "Theoretical distribution"))  +
  labs(x = "Residuals (log10 basis)",
       color = "Legend",
       title = "Density plot of the residuals") +
  scale_color_manual(values = colors)

#Other visualizations (not used in report)
exploration_data %>%
  mutate(scalar = floor(log10_views)) %>%
  ggplot(aes(y = resid_views, x = scalar, group = scalar)) + 
  geom_violin()

exploration_data %>%
  mutate(scalar = floor(log10_views)) %>%
  group_by(scalar) %>%
  summarize(n = n())

exploration_data %>% 
  ggplot(aes(sample = resid_views)) + 
  geom_qq() +
  geom_abline(col = "red")

#Skewness and kurtosis of the residuals' distribution
skewness(exploration_data$resid_views) #fairly symmetrical if between -0.5 and 0.5
kurtosis(exploration_data$resid_views) #mesokurtic if around 3

#Explore possible relationships with verbal elements
p1 <- exploration_data %>%
  ggplot(aes(x = n_titl, y = resid_views)) +
  geom_point(alpha = 0.1) +
  labs(x = "Title length in words", y = "Residuals")

p2 <- exploration_data %>%
  ggplot(aes(x = title_l, y = resid_views)) + 
  geom_point(alpha = 0.1) +
  labs(x = "Title length in characters", y = "Residuals")

p3 <- exploration_data %>%
  filter((n_titl_hashtags + n_desc_hashtags) > 0) %>%
  ggplot(aes(y = resid_views, x = n_titl_hashtags + n_desc_hashtags)) + 
  geom_point(alpha = 0.1) +
  scale_x_continuous(trans = "log10") +
  labs(x = "log10(total number of hashtags)", y = "Residuals")

p4 <- exploration_data %>%
  filter(title_l > 0) %>%
  ggplot(aes(y = resid_views, x = pcap_tit_w)) + 
  geom_point(alpha = 0.1)  +
  labs(x = "% of capitalized words in title", y = "Residuals")

p5 <- exploration_data %>%
  filter(n_titl > 0) %>%
  ggplot(aes(y = resid_views, x = pcap_tit_l)) + 
  geom_point(alpha = 0.1)  +
  labs(x = "% of capitalized characters in title", y = "Residuals")

grid.arrange(p1, p2, p3, p4, p5)

#Explore possible relationships with numeric elements
p1 <- exploration_data %>% 
  ggplot(aes(x = votes_p_c, y = resid_views)) + 
  geom_point(alpha = 0.1) +
  labs(x = "log10(Votes per comment)", y = "Residuals")

p2 <- exploration_data %>% 
  ggplot(aes(x = likes_p_v, y = resid_views)) + 
  geom_point(alpha = 0.1) +
  labs(x = "log10(Likes per view)", y = "Residuals")

p3 <- exploration_data %>% 
  ggplot(aes(x = dislikes_p_v, y = resid_views)) + 
  geom_point(alpha = 0.1) +
  labs(x = "log10(Dislikes per view)", y = "Residuals")

p4 <- exploration_data %>% 
  ggplot(aes(x = rating_odds, y = resid_views)) + 
  geom_point(alpha = 0.1) +
  labs(x = "log10(Rating odds)", y = "Residuals")

p5 <- exploration_data %>% 
  ggplot(aes(x = engagement, y = resid_views)) + 
  geom_point(alpha = 0.1) +
  labs(x = "log10(Engagement ratio)", y = "Residuals")

grid.arrange(p1, p2, p3, p4, p5)

#Explore possible relationships with topics elements
p1 <- exploration_data %>%
  ggplot(aes(x = as.factor(topics_alig), y = resid_views, group = topics_alig)) +
  geom_boxplot() +
  labs(x = "Alignment of topics", 
       y = "Residuals",
       title = "Boxplot of residuals based on topic alignment")

regex_topics_t <- "https://en.wikipedia.org/wiki/"

p2 <- exploration_data %>%
  unnest(topics_vid) %>%
  mutate(topics_vid = str_replace_all(topics_vid,
                                      regex_topics_t,
                                      "")) %>%
  group_by(topics_vid) %>%
  summarize(mean = mean(resid_views), n = n()) %>%
  filter(n > 100) %>%
  ggplot(aes(x = reorder(topics_vid, mean), y = mean)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Topic name",
       y = "Average residual ratings",
       title = "Overview of residuals based on video topic")

grid.arrange(p1, p2, 
             ncol = 2)

remove(p1, p2, p3, p4, p5)

#Create our final data sets to use, removing unused variables
names(exploration_data)
final_data <- exploration_data %>% 
  select(video_id,
         category_name,
         resid_views,
         title_l,
         n_titl,
         n_titl_hashtags,
         n_desc, 
         n_desc_hashtags,
         engagement,
         likes_p_v,
         dislikes_p_v,
         votes_p_c,
         rating_odds,
         pcap_tit_l,
         pcap_tit_w) %>%
  mutate(category_name = as.factor(category_name))


#Visualize correlation between variables
if(!require(corrplot)) install.packages("corrplot")

library(corrplot)

cor_final_data <- final_data %>% 
  select(3:ncol(final_data)) %>% 
  cor(.)

testRes <- cor.mtest(cor_final_data, conf.level = 0.95)

cor_final_data %>% 
  corrplot(.,
           p.mat = testRes$p,
           insig = 'pch',
           sig.level = 0.5)

#Spearman correlation plot (not used in the report)
cor_final_data <- final_data %>% 
  select(3:ncol(final_data)) %>% 
  cor(., method = c("spearman"))

cor_final_data %>% 
  corrplot(.)

#Check bivariate distributions across correlated predictors
p1 <- final_data %>%
  ggplot(aes(x = n_titl, y = title_l)) + 
  geom_point(alpha = 0.1) +
  labs(x = "Title length in words", y = "Title length in characters")

p2 <- final_data %>%
  ggplot(aes(x = engagement, y = likes_p_v)) + 
  geom_point(alpha = 0.1) +
  geom_abline(slope = 1, intercept = 0, col = "red")+
  labs(x = "log10(Engagement ratio)", y = "log10(Likes per view)") +
  geom_text(aes(x = -3, y = -1, label = "Exclusion area"), col = "red")

p3 <- final_data %>%
  ggplot(aes(x = dislikes_p_v, y = rating_odds)) + 
  geom_point(alpha = 0.1)+
  labs(x = "log10(Dislikes per view)", y = "Log10(Rating odds ratio)")

grid.arrange(p1, p2, p3, ncol = 3)

rm(p1, p2, p3)

#Remove unused variables
final_data <- final_data %>% 
  select(-n_titl_hashtags,
         -n_desc_hashtags,
         -n_desc,
         -pcap_tit_l,
         -title_l,
         -likes_p_v,
         -rating_odds)

#Generate a matrix of video topics
regex_topics_t <- "https://en.wikipedia.org/wiki/"

exploration_data %>% 
  unnest(topics_vid) %>%
  pull(topics_vid)

topics_mat <- exploration_data %>%
  unnest(topics_vid) %>%
  mutate(topics_vid = str_replace_all(topics_vid,
                                      regex_topics_t,
                                      "")) %>%
  select(video_id, topics_vid) %>%
  mutate(video_id = as.factor(video_id),
         topics_vid = as.factor(topics_vid),
         index = 1) %>%
  as.data.frame() %>%
  spread(topics_vid, index) %>%
  as.matrix()

rownames(topics_mat) <- topics_mat[,1]
topics_mat <- topics_mat[,-1]

topics_mat[is.na(topics_mat)] <- 0

topics_mat <- apply(topics_mat, 2, as.integer)

dims <- dim(topics_mat)

#Create a cluster dendrogram to visualize topics and their relative proximity
d_features <- dist(t(topics_mat))
plot(hclust(d_features), hang = -1, cex = 0.6, xlab = "Features", sub = "")

#Overview of the number of observations per topic (not shown in report)
topics_mat %>% 
  colSums() %>% 
  data.frame() %>%
  arrange(desc(.)) %>%
  mutate(share = ./nrow(exploration_data))

#Decompose the matrix
set.seed(5, sample.kind = "default")
pca <- prcomp(topics_mat)
var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
cols <- which.max(var_explained >= 0.8)

plot(var_explained, ylim = c(0,1), ylab = "Proportion of variability explained")

pcs <- data.frame(pca$rotation, name = colnames(topics_mat))
p1 <- pcs %>%  ggplot(aes(PC1, PC2)) + 
  geom_point() + 
  geom_text_repel(aes(PC1, PC2, label=name),
                  data = filter(pcs, 
                                PC1 < -0.1 | PC1 > 0.1 | PC2 < -0.1 | PC2 > 0.1))

p2 <- pcs %>%  ggplot(aes(PC3, PC4)) + 
  geom_point() + 
  geom_text_repel(aes(PC3, PC4, label=name),
                  data = filter(pcs, 
                                PC3 < -0.15 | PC3 > 0.15 | PC4 < -0.15 | PC4 > 0.15))

p3 <- pcs %>%  ggplot(aes(PC5, PC6)) + 
  geom_point() + 
  geom_text_repel(aes(PC5, PC6, label=name),
                  data = filter(pcs, 
                                PC5 < -0.1 | PC5 > 0.1 | PC6 < -0.1 | PC6 > 0.1))

p4 <- pcs %>%  ggplot(aes(PC7, PC8)) + 
  geom_point() + 
  geom_text_repel(aes(PC7, PC8, label=name),
                  data = filter(pcs, 
                                PC7 < -0.1 | PC7 > 0.1 | PC8 < -0.1 | PC8 > 0.1))


grid.arrange(p1, p2, p3, p4,
             ncol = 2)

p1 <- pcs %>%  ggplot(aes(PC9, PC10)) + 
  geom_point() + 
  geom_text_repel(aes(PC9, PC10, label=name),
                  data = filter(pcs, 
                                PC9 < -0.15 | PC9 > 0.15 | PC10 < -0.15 | PC10 > 0.15))

p2 <- pcs %>%  ggplot(aes(PC11, PC12)) + 
  geom_point() + 
  geom_text_repel(aes(PC11, PC12, label=name),
                  data = filter(pcs, 
                                PC11 < -0.1 | PC11 > 0.1 | PC12 < -0.1 | PC12 > 0.1))

p3 <- pcs %>%  ggplot(aes(PC1, PC13)) + 
  geom_point() + 
  geom_text_repel(aes(PC1, PC13, label=name),
                  data = filter(pcs, 
                                PC1 < -0.1 | PC1 > 0.1 | PC13 < -0.1 | PC13 > 0.1))

grid.arrange(p1, p2, p3,
             ncol = 2)

remove(p1, p2, p3, p4)

#Reduce the dimensions of the matrix
cols <- which.max(var_explained >= 0.8)

reduced_mat <- pca$x[,1:cols]
fit_data <- cbind(final_data, reduced_mat)

#Save the data
save(final_data, file = "final_data.RData")
save(fit_data, file = "fit_data.RData")

##########MODELING##########
#Initialisation
if(!require(gtools)) install.packages("gtools")

library(gtools)

#Define fit parameters
frontier <- 0

fit_data <- fit_data %>%
  mutate(resid_dist = ifelse(resid_views <= frontier, 0, 1))

fit_data %>% 
  group_by(resid_dist) %>%
  summarize(mean_v = mean(resid_views))

#Create training and test sets
set.seed(5, sample.kind = "default")
test_ind <- createDataPartition(fit_data$resid_dist,
                                times = 1,
                                p = 0.2,
                                list = FALSE)

fit_data_mat <- fit_data %>%
  select(-video_id,
         -resid_views)

x <- fit_data_mat %>% 
  select(-resid_dist)
y <- fit_data_mat %>% 
  select(resid_dist) %>%
  pull(resid_dist) %>%
  as.factor()

test_set <- fit_data[test_ind,]
test_x <- x[test_ind,]
test_y <- y[test_ind]
train_x <- x[-test_ind,]
train_y <- y[-test_ind]

#Remove non-numeric data for numerical algorithms
x_num <- fit_data_mat %>% 
  select(-category_name, -resid_dist)

test_x_num <- x_num[test_ind,]
train_x_num <- x_num[-test_ind,]

#Look at accuracy of randomly guessing
set.seed(5, sample.kind = "default")
random_test <- sample(0:1, length(test_y), replace = T)
r_acc <- mean(random_test == test_y)

accuracies <- tibble(model = "Random guess", accuracy = round(r_acc,3))

#Fit a random tree algorithm
if(!require(rpart)) install.packages("rpart")
if(!require(rpart.plot)) install.packages("rpart.plot")

library(rpart)
library(rpart.plot)
set.seed(5, sample.kind = "default")
cps <- seq(0.0001, 0.0021, length.out = 21)

fit_rpart <- train(x = train_x,
                   y = train_y,
                   method = "rpart",
                   tuneGrid = data.frame(cp = cps),
                   trControl = trainControl(method = "cv",
                                            number = 10,
                                            p = .9,
                                            allowParallel = TRUE))

bfit <- fit_rpart$bestTune

pred_rpart <- predict(fit_rpart, test_x)
rpart_acc <- mean(pred_rpart == test_y)

accuracies <- bind_rows(accuracies,
                        tibble(model = "Classification tree", accuracy = round(rpart_acc,3)))

plot(fit_rpart)

prp(fit_rpart$finalModel,
    faclen = -1,
    fallen.leaves = F,
    tweak = 1.5,
    box.palette = "RdGn")

#Tree names (not shown in report)
ind <- !(fit_rpart$finalModel$frame$var == "<leaf>")
tree_terms <-
  fit_rpart$finalModel$frame$var[ind] %>%
  unique() %>%
  as.character()
tree_terms

#Fit a random forest algorithm (Rborist)
if(!require(Rborist)) install.packages("Rborist")

library(Rborist)
set.seed(5, sample.kind = "default")

nodes <- 12:16

fit_rf <- train(x = train_x,
                y = train_y,
                method = "Rborist",
                tuneGrid = data.frame(predFixed = 2, minNode = nodes),
                trControl = trainControl(method = "cv",
                                         number = 10,
                                         p = .9,
                                         allowParallel = TRUE))

plot(fit_rf)
bfit <- fit_rf$bestTune$minNode

pred_rf <- predict(fit_rf, test_x)
rf_acc <- mean(pred_rf == test_y)

accuracies <- bind_rows(accuracies,
                        tibble(model = "Random forests", accuracy = round(rf_acc,3)))

#Fit a random fern algorithm
if(!require(rFerns)) install.packages("rFerns")

library(rFerns)
invisible(gc())
set.seed(5, sample.kind = "default")
depths <- 13:16
fit_rfern <- train(x = train_x,
                   y = train_y,
                   method = "rFerns",
                   tuneGrid = data.frame(depth = depths),
                   trControl = trainControl(method = "cv",
                                            number = 10,
                                            p = .9,
                                            allowParallel = TRUE))

plot(fit_rfern)
bfit <- fit_rfern$bestTune

pred_rfern <- predict(fit_rfern, test_x)
rfern_acc <- mean(pred_rfern == test_y)

accuracies <- bind_rows(accuracies,
                        tibble(model = "Random ferns", accuracy = round(rfern_acc,3)))

#Fit a linear discriminant analysis model
set.seed(5, sample.kind = "default")

train_x_f <- train_x %>%
  mutate(category_name = as.factor(category_name))

fit_lda <- train(x = train_x_num,
                 y = train_y,
                 method = "lda")

pred_lda <- predict(fit_lda, test_x_num)
lda_acc <- mean(pred_lda == test_y)

accuracies <- bind_rows(accuracies,
                        tibble(model = "LDA", accuracy = round(lda_acc,3)))

#Fit a k-nearest neighbors model
invisible(gc())
set.seed(5, sample.kind = "default")
ks <- seq(70, 140, 10)

fit_knn <- train(x = train_x_num,
                 y = train_y,
                 method = "knn",
                 tuneGrid = data.frame(k = ks),
                 trControl = trainControl(method = "cv",
                                          number = 10,
                                          p = .9))

plot(fit_knn)

bfit <- fit_knn$bestTune

pred_knn <- predict(fit_knn, test_x_num)
knn_acc <- mean(pred_knn == test_y)

accuracies <- bind_rows(accuracies,
                        tibble(model = "KNN", accuracy = round(knn_acc,3)))

#Fit a single-hidden-layer neural network model
if(!require(nnet)) install.packages("nnet")

library(nnet)
invisible(gc())
set.seed(5, sample.kind = "default")
sizes <- seq(10, 20, 5) #number of units in the hidden layer
decays <- seq(0.15, 0.25, 0.05) #weight decays

fit_nnet <- train(x = train_x,
                  y = train_y,
                  method = "nnet",
                  tuneGrid = expand.grid(size = sizes, decay = decays),
                  trControl = trainControl(method = "cv",
                                           number = 10,
                                           p = .9),
                  trace = F)

bsize <- fit_nnet$bestTune$size
bdecay <- fit_nnet$bestTune$decay
pred_nnet <- predict(fit_nnet, test_x)
nnet_acc <- mean(pred_nnet == test_y)

accuracies <- bind_rows(accuracies,
                        tibble(model = "Neural network", accuracy = round(nnet_acc,3)))

#Combine models into a single table
pred <- cbind(pred_rpart,
              pred_rf, 
              pred_rfern, 
              pred_lda, 
              pred_knn, 
              pred_nnet)

#Give names to each model
pred_names <- c("Classification tree",
                "Random forests",
                "Random ferns",
                "LDA",
                "KNN",
                "Neural network")

#Use algorithm to determine best fit from a combination of models, on validation set
best_fits <- sapply(1:ncol(pred), function(i) {
  comb <- combinations(ncol(pred), i)
  temp <- apply(comb, 1, function(row) {
    pred_temp <- as.matrix(pred[,row])
    pred_fin <- apply(pred_temp, 1, mean) %>%
      tibble() %>%
      sweep(., 1, 1) %>%
      mutate(pred = round(.)) %>%
      pull(pred) %>%
      as.factor()
    mean_pred <- mean(pred_fin == test_y)
  })
  bind_rows(tibble(combination = list(comb[which.max(temp),]), accuracy = max(temp)))
})

best_fit <- best_fits[1,which.max(best_fits[2,])][[1]][[1]]

best_fit_names <- paste(pred_names[best_fit], collapse = ", ")

#Apply to test set
pred_best <- pred[,best_fit] %>% as.matrix()
pred_fin <- apply(pred_best, 1, mean) %>%
  tibble() %>%
  sweep(., 1, 1) %>%
  mutate(pred = round(.)) %>%
  pull(pred) %>%
  as.factor()

comb_acc <- mean(pred_fin == test_y)

accuracies <- bind_rows(accuracies,
                        tibble(model = "Best combination", accuracy = round(comb_acc,3)))

save(accuracies, file = "accuracies.RData")

max(accuracies$accuracy)


#Show accuracy table
knitr::kable(accuracies,
             caption = "Accuracy for each model")

#Focus on final model
mean_0 <- test_set[(pred_fin == 0),] %>%
  pull(resid_views) %>%
  mean()

mean_1 <- test_set[(pred_fin == 1),] %>%
  pull(resid_views) %>%
  mean()

cm <- confusionMatrix(pred_fin, test_y)
sens <- cm$byClass["Sensitivity"]
spe <- cm$byClass["Specificity"]

#Visualization of inaccuracies (not used in paper)
test_set %>%
  filter(resid_dist !=  pred_fin) %>%
  ggplot(aes(x = resid_views)) +
  geom_density()

missed_vids <- test_set %>%
  filter(resid_dist !=  pred_fin) %>%
  pull(video_id)

exploration_data %>%
  filter(video_id %in% missed_vids) %>%
  group_by(age_w) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  ggplot(aes(x = age_w, y = n)) + 
  geom_point()

rm(test_set, test_x, test_x_num, test_y, train_x, train_x_num, train_y, x, x_num)