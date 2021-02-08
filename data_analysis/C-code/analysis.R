######################################## # 
#
# Communicate “Hope” to Motivate Action Against Infectious COVID-19 Variants'
# 
# Code by A. Bor alexander.bor@ps.au.dk
# 
# 2021.02.08.
######################################## # 


# Setup  ------------------------------------------------------------------

# load packages
library(rio)
library(here)
library(patchwork)
library(stargazer)
library(broom)
library(tidyverse)

# custom function which creates z-scores based on Control group 
z_score <- function(x, group) {
        c_mean <- mean(x[group == "Control"])
        c_sd <- sd(x[group == "Control"])
        ((x - c_mean) / c_sd)
}



# Experimental data ------------------------------------------------------


# ~ Import and clean data ---------------------------------------------------

raw <- import(here("A-original-data/B117_experiment.dta"))

# clean data 
data <- raw %>% 
        transmute(
                # rename first 
                id = caseid,
                   fear = q1_1, 
                   hope = q1_2,
                   group = case_when(qGroup == 1 ~ "Control", 
                                     qGroup == 2 ~ "Threat", 
                                     qGroup == 3 ~ "Hope"),
                   group = fct_relevel(group, "Control", "Threat"),
                   
                # for each variable we have 3 versions of the question
                # clearly explained, helps me, helps others,
                # we average across these
                   healththreat = rowMeans(select(.,q1_3, q1_7, q1_11 )), 
                   guidelines =   rowMeans(select(.,q1_4, q1_8, q1_12 )), 
                   safely =       rowMeans(select(.,q1_5, q1_9, q1_13 )), 
                   vaccines =     rowMeans(select(.,q1_6, q1_10,q1_14 ))
                   ) %>% 
        # now we calculate the z-score based on control group mean and sd
        # for each dv
        mutate(across(.cols = c("fear", "hope", "healththreat", "guidelines",
                                "safely", "vaccines"),
                      .fns = ~z_score(.x, group), .names = "z_{col}" )) %>%
        glimpse

# make sure we managed to standardize well. 
# mean should be 0 in control group
stopifnot(near(mean(data$z_healththreat[data$group == "Control"]), 0))
stopifnot(near(mean(data$z_guidelines[data$group == "Control"]), 0))
stopifnot(near(mean(data$z_safely[data$group == "Control"]), 0))
stopifnot(near(mean(data$z_vaccines[data$group == "Control"]), 0))

# sd should be near 1
stopifnot(near(sd(data$z_healththreat[data$group == "Control"]), 1))
stopifnot(near(sd(data$z_guidelines[data$group == "Control"]), 1))
stopifnot(near(sd(data$z_safely[data$group == "Control"]), 1))
stopifnot(near(sd(data$z_vaccines[data$group == "Control"]), 1))

saveRDS(data, file = here("B-analysis-data/B117_experiment_clean.rds"))
data <- readRDS(file = here("B-analysis-data/B117_experiment_clean.rds"))



# ~ Analysis ----------------------------------------------------------------

# n of observations
nrow(data)

# Fit simple linear models with the condition as a factor 

fit1 <- lm(z_fear ~ group, data)
fit2 <- lm(z_hope ~ group, data)
fit3 <- lm(z_healththreat ~ group, data)
fit4 <- lm(z_guidelines ~ group, data)
fit5 <- lm(z_safely ~ group, data)
fit6 <- lm(z_vaccines ~ group, data)

# combine outputs into a table 

stargazer(fit1, fit2, fit3, fit4, fit5, fit6, 
          type = "text", 
          digits = 2, 
          covariate.labels = c("Threat", "Hope"),
          dep.var.labels = c("Fear", "Hope", "Public health threat", 
                             "Adhere to guidelines", 
                             "Safely get through", 
                             "Strong measures required"
                             ),
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.stat = c("rsq", "f", "ser"),
          out = here("D-documents/tables/experiment_results.html"))


# Also estimate models on the contrast between Hope and Threat condition 
#       excluding control group

fit1b <- lm(z_fear ~ fct_relevel(group, "Threat"), filter(data, group != "Control"))
fit2b <- lm(z_hope ~ fct_relevel(group, "Threat"), filter(data, group != "Control"))
fit3b <- lm(z_healththreat ~ fct_relevel(group, "Threat"), filter(data, group != "Control"))
fit4b <- lm(z_guidelines ~ fct_relevel(group, "Threat"), filter(data, group != "Control"))
fit5b <- lm(z_safely ~ fct_relevel(group, "Threat"), filter(data, group != "Control"))
fit6b <- lm(z_vaccines ~ fct_relevel(group, "Threat"), filter(data, group != "Control"))

# end export these model results too
stargazer(fit1b, fit2b, fit3b, fit4b, fit5b, fit6b, 
          type = "text", 
          digits = 2, 
          covariate.labels = c("Hope"),
          dep.var.labels = c("Fear", "Hope", "Public health threat", 
                             "Adhere to guidelines", 
                             "Safely get through", 
                             "Strong measures required"
          ),
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.stat = c("rsq", "f", "ser"),
          out = here("D-documents/tables/experiment_contrasts.html"))



# create figure 1

# define a custom function which will output little plots given the DV
myp2 <- function(model, label, tag = NULL){
        # tidy up the ols model
        results <- tidy(model) %>% 
                filter(term != "(Intercept)") %>% 
                mutate(term = gsub("group", "", term), 
                       DV = label)
        
        # draw plot
        ggplot(results, aes(x = term, fill = term)) + 
                geom_boxplot(aes(middle = estimate,
                                 lower =  estimate - 1.96 * std.error,
                                 upper =  estimate + 1.96 * std.error,
                                 ymin =  estimate - 3.09 * std.error,
                                 ymax =  estimate + 3.09 * std.error),
                             stat = "identity", width = .5) +
                geom_hline(aes(yintercept = 0)) +
                xlab("") + ylab("") + 
                labs(title = label, tag = tag)+ 
                
                scale_fill_manual(guide = NULL, 
                                  values = c("#BA25D6", "#E09509")) + 
                ylim(c(-0.2, 0.57)) + 
                theme_bw() +  
                theme(plot.title = element_text(size = 10, 
                                                face = "bold", 
                                                hjust = 0.5))
        
}

# now take custom plot function and go through all DVs 
#  use patchwork library to combine these into a single plot
(myp2(fit1, "Fearful\n", "C") |
                myp2(fit2, "Hopeful\n", "D") |
                myp2(fit3, "Public Health \nThreat", "E") |
                myp2(fit4, "Adhere to \nGuidelines", "F") |
                myp2(fit5, "Safely Get \nThrough", "G") |
                myp2(fit6, "Strong Measures \nRequired", "H"))
# save plot. This appears as part of Figure 1 in the paper,
ggsave(here("D-documents/figures/fig1_c-h.jpg"), width = 9.4, height = 2.6)


# Observational data -------------------------------------------------------------------------

# import raw data
epi_raw <- import(here("A-original-data/B117_observational.rds"))

# define a function which shall recode Danish DV
dk_scale5  <- function(x){
        case_when(x == "Helt uenig" ~ 0,
                  x == "Delvis uenig" ~ 0,
                  x == "Hverken enig eller uenig" ~ 0,
                  x == "Delvis enig" ~ 1,
                  x == "Helt enig" ~ 1,
                  TRUE ~ NA_real_)
}

# clean data 
epi <- epi_raw %>% 
        # drop responses before our b117 variables were added
        filter(ActualSurveyStartTime >= lubridate::ymd("2021-01-01")) %>% 
        # recode focal DVs
        transmute(heard = dk_scale5(qNew_10_18_resp), 
                  worried = dk_scale5(qNew_10_19_resp), 
                  follow = dk_scale5(qNew_10_20_resp), 
                  # and also create country codes 
                  country_code = case_when(country == "Danmark" ~ "DK", 
                                           country == "Sverige" ~ "SW", 
                                           country == "United Kingdom" ~ "UK", 
                                           country == "USA" ~ "US", 
                                           country == "Italien" ~ "IT", 
                                           country == "Frankrig" ~ "FR", 
                                           country == "Ungarn" ~ "HU", 
                                           country == "Tyskland" ~ "DE", 
                                           TRUE ~ NA_character_)
        ) %>% 
        na.omit() 
# save cleaned data
saveRDS(epi, file = here("B-analysis-data/B117_observational_clean.rds"))
# reload cleaned data
epi <- readRDS(file = here("B-analysis-data/B117_observational_clean.rds"))

nrow(epi)

# calculate mean support for each DV in pooled sample
epi %>% summarise(across(c(heard, worried, follow), mean)) %>% 
        round(2)

# test significance of difference btw heard and worried
t.test(epi$heard, epi$worried, paired = T)
#       heard and follow
t.test(epi$heard, epi$follow, paired = T)


# calculate means and SDs
epi_sum <- epi %>% 
        # we want this for each country
        group_by(country_code) %>%
        # calculate summary statistics
        summarise(across(.cols = everything(),
                         .fns = list(mean = mean, 
                                     sd = sd, 
                                     n = ~length(.x),
                                     se = ~sd(.x)/sqrt(length(.x)))
        )) %>%
        # a bit of wrangling to get proper long format
        pivot_longer(-country_code, names_to = "variable", values_to = "values") %>%
        separate(variable, c("variable", "statistic")) %>% 
        mutate(variable = fct_relevel(variable, "heard", "worried"),
               variable = fct_recode(variable, 
                                     `Heard about` = "heard", 
                                     `Worried about` = "worried", 
                                     `Follows advice` = "follow")) %>% 
        pivot_wider(names_from = statistic, values_from = values) %>% 
        glimpse

# country order. We want to arrange by levels of "heard about" 
c_order <-  epi_sum %>% 
        filter(variable == "Heard about") %>% 
        arrange(mean)

# draw plot
ggplot(epi_sum, aes(x = fct_relevel(country_code, 
                                    c_order$country_code), 
                    fill = variable)) +
        geom_boxplot(aes(middle = mean,
                         lower =  mean - 1.96 * se,
                         upper =  mean + 1.96 * se,
                         ymin =   mean - 3.09 * se,
                         ymax =   mean + 3.09 * se),
                     stat = "identity", width = .5, 
                     position = position_dodge(.6)) +  
        xlab("") + ylab("") + 
        scale_fill_grey(guide = guide_legend(reverse = F),
                        name = NULL, 
                        start = 0.4, end = .9) + 
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
        facet_grid(~ fct_relevel(country_code, 
                                 c_order$country_code), 
                   scales = "free") + 
        theme_bw() +
        theme(legend.position = "bottom", 
              strip.background = element_blank(),
              strip.text.x = element_blank(), 
              panel.grid.major.x = element_blank(), 
              axis.ticks.x = element_blank())

# export plot. This appears as Figure 2 in the manuscript.
ggsave(here("D-documents/figures/fig2.jpg"),
       width = 6, height = 5 )
