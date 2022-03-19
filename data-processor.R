#### Preamble:  Setup & import libraries ####
if(!require("pacman")) install.packages("packman")
pacman::p_load(pacman,tidyverse,tsibble,ggplot2,reshape2,tidylog,hms,lmtest,ggsignif)
##YOU NEED TO UPDATE THIS WITH THE PATH TO THE PARENT FOLDER CONTAINING THIS SCRIPT
setwd("~/Desktop/dataprocessor") 

##### Custom Function Declarations ####
# Define functions to handle the export of visualizations
saveVis <- function (name, w = 7, h = 7, writeToDisk = FALSE){
  if (writeToDisk){
    ggsave(name,
           device = "pdf",
           plot = last_plot(), 
           path = paste0(getwd(), "/visualizations"),
           scale = 1, 
           width = w, 
           height = h, 
           dpi = 300, 
           limitsize = TRUE,
           units = "in")
  } else {
    return("Did not save. Provide boolean for variable: `writeToDisk`")
  }
}
# Define function for identifying Outliers
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
# define a classification function to return booleans for cases beyond a value (filtering analysis)
is_LsssThan <- function(x,keyValue) { 
  return (x >= keyValue)
}


##### Define Themes and colors #####
boldTextTheme <- theme(axis.text.x = element_text(face="bold"), 
                       axis.text.y = element_text(face="bold"), 
                       axis.title.y = element_text(face="bold"),
                       axis.title.x = element_text(face="bold"), 
                       plot.title = element_text(face = "bold"),
                       plot.background = element_rect(colour = "#353535", size = 1.5)
)
# set colors for factors ###
#google sheets default colors
# my_colors <- c("#4285F4" , "#EA4336", "#FBBC04")            # Create vector of colors (same as used in google)
my_colors <- c("#4FBAAE","#bc4b51","#ffc65c")
# my_colors <- c("#fff7bc" , "#fec44f", "#d95f0e")            # Create vector of colorblind colors
names(my_colors) <- levels(c("Control","Coverage","History")) # Extract all levels of both data
# Specify scale_fill_manual
my_edge_colors <- scale_colour_manual(name = "Condition", values = my_colors)
my_fill_colors <- scale_fill_manual(  name = "Condition", values = my_colors)

#color def for visualizations without control condition.
my_two_colors <- my_colors[2:3] #c( "#EA4336", "#FBBC04")            # Create vector of colors
names(my_two_colors) <- levels(c("Coverage","History")) # Extract all levels of both data
my_two_edge_colors <- scale_colour_manual(  name = "Condition", values = my_two_colors)
my_two_fill_colors <- scale_fill_manual(  name = "Condition", values = my_two_colors)

#### Import Data ##########
# Import Data form interaction logs
df.Participants <- read_csv("data/extracted.csv") %>%
  mutate(Condition = as.factor(Condition)) %>%
  mutate(Condition = recode_factor(Condition, `1`="Control",`2`='Coverage',`4`='History'))%>%
  mutate(grossFilter = `searchCount-Gross` + `AffiliationEvents-Gross`) %>%
  mutate(searchRatio = `searchCount-Gross`/grossFilter)
summary(df.Participants)

#getting values for table 1 in paper
df.Participants %>%
  filter(Condition == "Control") %>%
  select(grossDocCount,grossFilter,interactionRate,overlapWA,independence)%>%
  summary
df.Participants %>%
  filter(Condition == "Coverage") %>%
  select(grossDocCount,grossFilter,interactionRate,overlapWA,independence)%>%
  summary
df.Participants %>%
  filter(Condition == "History") %>%
  select(grossDocCount,grossFilter,interactionRate,overlapWA,independence)%>%
  summary
# Starting to make a faceted, visual version of Table 1
#todo: assign appropriate colors to conditions and update variables names.
#todo: make scales better
df.Participants %>%
  select(Condition,grossDocCount,grossFilter,interactionRate,overlapWA,independence) %>%
  melt() %>%
  ggplot(aes(y=Condition, x=value))+
  geom_boxplot(aes(fill=variable)) +
  facet_wrap(~variable, scales = "free", ncol=1) + 
  labs(x=NULL, y=NULL)+
  theme(legend.position="none")

##### Conclusion Conf. ####
df.Confidence <- read_csv("data/confidence.csv") %>%
  melt %>%
  rename(Conditions = variable) %>%
  rename(Count = value)
summary(df.Confidence)

##### Zhao Strategies #####
df.Strategies <- read_csv(file = "data/Participants-Use for Analysis.csv") %>%
  select(userID, Cond, strategy_zhao3) %>%
  mutate(Cond = as.factor(Cond)) %>%
  mutate(Cond = recode_factor(.x = Cond, `1`="Control",`2`='Coverage',`4`='History')) %>%
  mutate(strategy = as.factor(strategy_zhao3)) %>%
  group_by(Cond, strategy)
  # summarise(counts = n())
summary(df.Strategies)

#getting values for table 2 in paper
df.InteractionStrategies <- merge(df.Participants,df.Strategies,by="userID")
df.InteractionStrategies%>%
  filter(strategy == "Keyword Browsing") %>%
  select(grossDocCount,grossFilter,overlapWA,independence)%>%
  summary
df.InteractionStrategies%>%
  filter(strategy == "Random Access") %>%
  select(grossDocCount,grossFilter,overlapWA,independence)%>%
  summary

df.InteractionStrategies%>%
  filter(strategy == "Reviewing Origin") %>%
  select(grossDocCount,grossFilter,overlapWA,independence)%>%
  summary

df.InteractionStrategies%>%
  filter(strategy == "Starting over") %>%
  select(grossDocCount,grossFilter,overlapWA,independence)%>%
  summary

# Starting to make a faceted, visual version of Table 2
#todo: assign appropriate colors to conditions and update variables names.
#todo: make scales better
df.InteractionStrategies %>%
  select(strategy,grossDocCount,grossFilter,interactionRate,overlapWA,independence) %>%
  melt() %>%
  ggplot(aes(y=strategy, x=value))+
  geom_boxplot(aes(fill=variable)) +
  facet_wrap(~variable, scales = "free", ncol=1) + 
  labs(x=NULL, y=NULL)+
  theme(legend.position="none")

##### Filtering events as Percent completion Line chart ####
df.Filtering <- read_csv("data/100_affSearch_sum_rel.csv") %>% #can pull form this dataset since it's both searches and affiliation events
  mutate(cond=as.factor(cond)) %>% #convert to factor
  # cbind("type" = "Search") %>%  Residue from combining with Search and Tool Use
  filter(ID != "1e5bbcef") %>% #remove user who did not search
  filter(ID != "822b90c6") %>% # remove user who did not search
  # select(-total)%>% #remove the column with the total filtering interactions
  melt() %>% # make data tall
  mutate(cond = recode_factor(cond, `1`="Control",`2`='Coverage',`4`='History')) %>%
  mutate(variable = parse_hms(variable)) %>% #convert time columns to hms class.
  na.omit()%>% # remove any of the rows for total if needed
  filter(as_hms(variable) < 1800) %>% # removing interactions after 30 min.
  # filter(as_hms(variable) < 1920) %>% # removing interactions after 32 min.
  # mutate(variable = as.datetime(variable,'%m:%s')) #alternative datetime, that we've abandoned.
  # mutate(value = value * 100) %>% # multiply the fractional value to make percentage
  # filter(value < 100) %>%
  mutate(ID=as.factor(ID)) %>% #convert to factor
  group_by(ID) %>% #Draw lines according to this group.
  mutate(time2 = as.POSIXct(variable, origin = "2021-12-01", tx = "GMT"))
summary(df.Filtering)

#### Visualization ####

##### Conclusion Conf. ####
df.Confidence %>%
  ggplot(data=., aes(x=Confidence,y=Count,fill=Conditions))+
  my_fill_colors +
  geom_bar(position="dodge", stat="identity", col="#353535")+
  geom_text(aes(label = Count), position = position_dodge(0.9), vjust = 1.5, colour = "black")+
  # ylim(0,12)+
  scale_y_continuous(breaks= c(3,6,9,12))+
  labs(
    title = "Confidience in Conclusion by Condition",
    x = "Confidence",
    x = NULL,
    y="Number of participants"
  )+
  theme(legend.position="top")+
  boldTextTheme
saveVis("conclusion-conf.pdf",h = 3, writeToDisk = F)




df.Confidence %>%
  aov(data = ., Count ~ Conditions + Confidence) %>%
  summary()
# No significant difference between conditions or confidence.
#             Df Sum Sq Mean Sq F value Pr(>F)
# Conditions   2   0.00   0.000   0.000  1.000
# Confidence   1   2.67   2.667   0.129  0.754
# Residuals    2  41.33  20.667 

# doing this manually cause I can't figure out how to do it otherwise
#run a fisher test
matrix(c(3,4,9,9,8,3), ncol = 2, nrow = 3) %>%
  # view() #just to check the shape
  fisher.test(.)
# p-value = 0.03908 - Significant Difference in conclusion confidence by conditions
# alternative hypothesis: two.sided

# Post Hoc tests - Control vs Coverage 
matrix(c(3,4,9,8), ncol = 2, nrow = 2) %>%
  # view()
  fisher.test(.)
# p-value = 1 - can not detect difference in Control Confidence vs Coverage confidence
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:  0.07478974 5.44837199
# sample estimates: odds ratio   0.6780693 

matrix(c(3,9,9,3), ncol = 2, nrow = 2) %>%
  # view()
  fisher.test(.)
# p-value = 0.03913 - This is low, but not lower then the Bonferroni corrected p value (0.05/2 = 0.025) to be significant.
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:  0.01182317 0.92218820
# sample estimates: odds ratio   0.1241874 




##### Overlap with prior Analyst ####
df.Participants %>%
  ggplot( data = ., mapping = aes(x=Condition, y=overlapWA*15, fill=Condition)) +
  geom_boxplot( col="#353535")+
  # geom_jitter(height=0, width = 0.2)+
  # geom_point(alpha =0.3)+
  labs(
    title = "Overlap with Prior Analysis",
    x = NULL,
    y = "Number of Documents"
  )+
  my_fill_colors +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank())+
  ylim(0,15)+
  boldTextTheme
saveVis("overlap-box.pdf", h = 4, writeToDisk = F)

#running Anova
df.Participants %>%
  aov(overlapWA ~ Condition, data = .) %>%
  summary()
# # Do not see an effect:
# Df Sum Sq Mean Sq F value Pr(>F)
# Condition    2 0.0699 0.03494   0.501   0.61
# Residuals   33 2.3015 0.06974    


##### independence from Prior Analyst ####
df.Participants %>%
  ggplot( data = ., mapping = aes(x=Condition, y=independence, fill=Condition)) +
  geom_boxplot(col="#353535")+
  # geom_jitter(height=0, width = 0.2)+
  # geom_point(alpha =0.3)+
  geom_signif(comparisons = list(c("Control","History")),
              map_signif_level = TRUE,
              test = "wilcox.test",
              annotations = "p = 0.0066",
              tip_length = c(0.1,0.15),
              y_position = 0.95
  )+
  labs(
    title = "Independence from Prior Analysis",
    x = NULL,
    y = "Percent"
  )+
  my_fill_colors +
  scale_y_continuous(labels = scales::percent,
                     breaks = c(0.0,0.25,0.50,0.75,1.0))+
  theme(legend.position = "none",
        panel.grid.major.x = element_blank())+
  # ylim(0,15)+
  boldTextTheme
saveVis("independence-box.pdf", h = 4, writeToDisk = F)

#running Anova
model <- df.Participants %>%
  aov(independence ~ Condition, data = .)
summary(model)
anova(model)
# # see an omnibus effect:
#        Df Sum Sq Mean Sq F value Pr(>F)  
# Condition    2 0.3082 0.15408   4.274 0.0224 *
# Residuals   33 1.1896 0.03605      

#There's a strong skew in the independence metric. want to make sure this is okay before doing post-hoc comparisons.
hist(residuals(model))
plot(fitted(model), residuals(model))
#The residuals should be unbiased and homoscedastic 
gqtest(model, order.by = ~Condition, data=df.Participants, fraction = 0)
# GQ = 1.986, df1 = 15, df2 = 15, p-value = 0.09775
#close, but Not significantly different from homoscedastic. so we can work with the significant result.

TukeyHSD(model, conf.level=.95) 
plot (TukeyHSD(model, conf.level=.95) )

##### Filtering Linechart ####

# #Testing the potential models to use for the time.
# fit1 <- lm(value~poly(time2,1), data=df.Filtering) # ERROR: Ops.difftime not defined for "difftime" objects - not sure how I had this working before...
# fit2 <- lm(value~poly(time2,2), data=df.Filtering)
# fit3 <- lm(value~poly(time2,3), data=df.Filtering)
# fit4 <- lm(value~poly(time2,4), data=df.Filtering)
# fit5 <- lm(value~poly(time2,5), data=df.Filtering)
# fit6 <- lm(value~poly(time2,6), data=df.Filtering)
# fit7 <- lm(value~poly(time2,7), data=df.Filtering)
# # Looking at R^2 values to find the model that explains the curves best.
# plot(x = seq(1:7), 
#      y = c(
#        summary(fit1)$adj.r.squared,
#        summary(fit2)$adj.r.squared,
#        summary(fit3)$adj.r.squared, #largest step in r2 adj
#        summary(fit4)$adj.r.squared, #best r2 adj overall= 0.745307
#        summary(fit5)$adj.r.squared,
#        summary(fit6)$adj.r.squared,
#        summary(fit7)$adj.r.squared))
# confint(fit3, level=0.95) 
# #Look for significance (which terms in the model are significant)
# #all are significant in 3 factor model.
# 
# # test for homeostacity
# library(lmtest)
# plot(fitted(fit3),residuals(fit3))
# gqtest(fit3, order.by = ~cond, data=Filtering, fraction =0)
# # p value = 1 - so homostocastic representation.


df.Filtering %>%
ggplot(data = ., mapping = aes(x=time2, y=value, color= cond)) +
  geom_line(mapping=aes(group=df.Filtering$ID, alpha=0.4)) +
  geom_smooth(
    aes(size = 4),#col = "y ~ log(x)", fill = "y ~ log(x)"),
    method = "lm",
    formula = y~poly(x, 4),
    se = FALSE, # Plot the standard error
    level = 0.95,
    fullrange = TRUE, # The fit spans the full range of the horizontal axis
    show.legend = FALSE
  ) +
  geom_abline(slope = 0,intercept = 0.5, linetype = "dashed")+
  # geom_segment(aes(x = hms(724), y = 0.5, xend = hms(1170), yend = 0.5, size = 3), linetype = "dashed",  color="#bfbfbf")+ #"#FFA07A")+
  # geom_segment(aes(x = hms(900), y = 0.0, xend = hms(924), yend = 0.5, size = 3), color="#ababab")+ #"#FFA07A")+
  # geom_vline(xintercept = hms(924), linetype = "dashed")+
  # geom_vline(xintercept = (997), linetype = "dashed")+ #Time when more than 50% of participants had finished half their filters.
  # geom_line(mapping=aes(x=variable, y=0.5, size = 2))+
  # facet_grid(cond ~.)+
  my_edge_colors +
  boldTextTheme+
  labs(title = "Filtering Interactions Over Time by Condition",
       x = "Time",
       y = "Percent of Individual's Filtering Behaviors",
       caption = "2 participants removed from Control group due to 0 filtering behaviors.") +
  guides(
    size = "none",
    alpha = "none"
  )+
  scale_y_continuous(labels = scales::percent,
                     breaks = c(0.0,0.25,0.50,0.75,1.0))+
  scale_x_datetime(
    date_breaks = "3 min", 
    date_labels = "%M:%S", 
    date_minor_breaks = "3 min")+
  theme(
    # axis.text.x = element_text(angle=90),
    # axis.ticks = element_blank(),
    # panel.background = element_blank(),
    legend.position = c(0.15, 0.85),
    legend.background = element_rect(fill = "white"),
  )
saveVis("filtering-over-time-100.pdf", writeToDisk = F)

##### Strategies ####
df.Strategies %>%
  summarise(counts = n()) %>%
  ggplot(aes(x=strategy, y= counts, fill=Cond)) + 
  geom_bar(stat="identity", position = "dodge", col="#353535") + 
  geom_text(aes(label = counts), position = position_dodge(0.9), vjust = 1.5, colour = "black")+
  my_fill_colors + 
  boldTextTheme +
  theme(legend.position = "top")+
  labs(
    title = "Overall Strategies by Condition",
    x = "Strategy",
    y="Count"
  ) + 
  scale_y_continuous(breaks = c(0, 3, 6, 9),
                     minor_breaks = seq(0, 9, 1))
saveVis("strategy_zhao.pdf", h = 3, writeToDisk = F)

## Alternate - Stacked  column ##
df.Strategies %>%
  summarise(counts = n()) %>%
  ggplot(aes(x=strategy, y= counts, fill=Cond)) + 
  geom_col() +
  # geom_text(aes(label=counts))+ #TODO: fix the positioning of these numbers
  my_fill_colors + 
  boldTextTheme +
  theme(legend.position = "top")+
  labs(
    title = "Overall Strategies by Condition",
    x = "Strategy",
    y="Count"
  ) + 
  scale_y_continuous(breaks = c(0, 3, 6, 9, 12, 15),
                     minor_breaks = seq(0, 15, 1))
saveVis("strategy_zhao-stacked.pdf", h = 4, writeToDisk = F)

test.strategies <- df.Strategies %>%
  summarise(strategy) %>%
  matrix

test.strategies %>%
  table

fisher.test(table(test.strategies))
#p-value = 0.09339
chisq.test(table(test.strategies), correct = T)
#X-squared = 11.281, df = 6, p-value = 0.08006
#not the same... but also not significant difference between the different strategies.