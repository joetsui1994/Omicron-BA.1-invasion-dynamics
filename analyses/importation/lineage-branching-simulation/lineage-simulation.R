library(tidyverse)
library(RColorBrewer)
library(lubridate)
library(ggpubr)
library(seewave)

theme_set(theme_bw()+
           theme(strip.background =element_rect(fill="black"))+
           theme(strip.text = element_text(colour = 'white', face = "bold")))

# Load data ---------------------------------------------------------------

df_cases_nat <- read_tsv("Data/ONS_modelled_incidence_BA.1.tsv") 
df_imp <- read_tsv("Data/DTA_mcc_tls_metadata.tsv")
df_seq <- read_tsv("Data/DTA_taxon_tl_lookup.tsv")

# Calculate growth rate from cases -----------------------------------------

df_cases_nat_GR <-
  df_imp %>%
  group_by(import_est_date) %>%  
  summarise(count_imp =n()) %>% 
  full_join(df_cases_nat, by = c("import_est_date" = "date")) %>% 
  rename(date = import_est_date) %>% 
  arrange(date) %>% 
  mutate(count_imp = ifelse(is.na(count_imp), 0, count_imp)) %>% 
  mutate(growth_rate = log(lead(incidence_abs.BA.1_rm, 1) - count_imp) - log(incidence_abs.BA.1_rm)) 

df_grate <- df_cases_nat_GR %>% select(date, growth_rate)

# Frequency of lineages over time (Data) ----------------------------------

freq_lin_data <- 
  df_seq %>% 
  left_join(df_imp %>% select(tl, import_est_date)) %>% 
  mutate(YearWeek = factor(str_c(year(sample_date),"/W",week(sample_date))),
         YearWeek_imp = factor(str_c(year(import_est_date),"/W",week(import_est_date))), 
         YearWeek_imp_2 = case_when(YearWeek_imp == "2021/W45"~ "2021/W45",
                                    YearWeek_imp == "2021/W46"|YearWeek_imp == "2021/W47" ~ "2021/W46-47",
                                    YearWeek_imp == "2021/W48"|YearWeek_imp == "2021/W49" ~ "2021/W48-49",
                                    YearWeek_imp == "2021/W50"|YearWeek_imp == "2021/W51" ~ "2021/W50-51",
                                    YearWeek_imp == "2021/W52"|YearWeek_imp == "2021/W53" ~ "2021/W52-53",
                                    YearWeek_imp == "2022/W1"|YearWeek_imp == "2022/W2" ~ "2022/W1-2",
                                    YearWeek_imp == "2022/W3"|YearWeek_imp == "2022/W4" ~ "2022/W3-4",
                                    YearWeek_imp == "2022/W5" ~ "2022/W5"
                                    )) %>% 
  group_by(YearWeek_imp_2, YearWeek) %>% 
  summarise(count = n()) %>% 
  group_by(YearWeek) %>% 
  mutate(freq = count/sum(count)) %>% 
  mutate(data = "Data")

# function to simulate lineages -------------------------------------------

generate_evolin <- function(g_rate, date_startGR, tl_name, date_int) {
  g_rate <- g_rate %>%
    mutate(growth_rate = ifelse(date < date_startGR,
                                g_rate$growth_rate[which(g_rate$date == date_startGR)],
                                growth_rate))
  c_lin <- rep(0, nrow(g_rate))
  c_lin_day <- 0
  for (day in 1:nrow(g_rate)) {
    c_lin[day] <-  c_lin_day
    if (date_int == g_rate$date[day]) {
      c_lin_day <- 1
      c_lin[day] <- 1
    }
    c_lin_day <- c_lin_day * exp(g_rate$growth_rate[day])
  }
  tibble(tl = tl_name,
         date = as.Date(g_rate$date),
         Cases = c_lin)
} 

# simulate lineages according to date of introduction ---------------------

start_GR <-  "2021-12-15" #Starting date to impute growth rate

list_imp <- split(df_imp %>% select(tl, import_est_date), seq(nrow(df_imp)))
list_sim_lin <- lapply(list_imp, function(x) generate_evolin(df_grate, as.Date(start_GR), x$tl, x$import_est_date))
df_sim_lin <- do.call("rbind", list_sim_lin)

# Frequency of lineages over time (Simulation) ----------------------------

df_sim_lin_W <- 
  df_sim_lin %>% 
  left_join(df_imp %>% select(tl, import_est_date)) %>% 
  mutate(YearWeek = factor(str_c(year(date), "/W", week(date))),
         YearWeek_imp = factor(str_c(year(import_est_date), "/W", week(import_est_date))), 
         YearWeek_imp_2 = case_when(YearWeek_imp == "2021/W45"~ "2021/W45",
                                    YearWeek_imp == "2021/W46"|YearWeek_imp == "2021/W47" ~ "2021/W46-47",
                                    YearWeek_imp == "2021/W48"|YearWeek_imp == "2021/W49" ~ "2021/W48-49",
                                    YearWeek_imp == "2021/W50"|YearWeek_imp == "2021/W51" ~ "2021/W50-51",
                                    YearWeek_imp == "2021/W52"|YearWeek_imp == "2021/W53" ~ "2021/W52-53",
                                    YearWeek_imp == "2022/W1"|YearWeek_imp == "2022/W2" ~ "2022/W1-2",
                                    YearWeek_imp == "2022/W3"|YearWeek_imp == "2022/W4" ~ "2022/W3-4",
                                    YearWeek_imp == "2022/W5" ~ "2022/W5"
         )) %>% 
  group_by(tl, YearWeek_imp_2, YearWeek) %>% 
  summarise(cases_W = sum(Cases)) %>% 
  ungroup()

freq_lin_sim <- 
  df_sim_lin_W %>% 
  group_by(YearWeek_imp_2, YearWeek) %>% 
  summarise(cases_W = sum(cases_W)) %>% 
  group_by(YearWeek) %>% 
  mutate(freq = cases_W/sum(cases_W)) %>% 
  mutate(data = "Model")

# Plots and KL distance ---------------------------------------------------

Palcols <- colorRampPalette(brewer.pal(9, "RdBu")) #Returning palette
coeff <- 0.0012 #coeff to rescale secondary y-axis in the plot

KLdist <- rbind(freq_lin_sim %>% select(YearWeek_imp_2, YearWeek, freq, data),
                freq_lin_data %>% select(YearWeek_imp_2, YearWeek, freq, data)) %>% 
  pivot_wider(names_from = data, values_from = freq) %>% 
  filter(!is.na(Data)) %>% 
  group_by(YearWeek) %>% 
  summarise(KL_dist = kl.dist(Model, Data)$D2,
            KL_dist_sim = kl.dist(Model, Data)$D) %>% 
  summarise(value_KL = sum(KL_dist),
            value_KL_sim = sum(KL_dist_sim))

plt_0 <- df_cases_nat_GR %>% 
  mutate(growth_rate = ifelse(date < as.Date(start_GR),
                              growth_rate[which(date == as.Date(start_GR))],
                              growth_rate)) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = incidence_abs.BA.1_rm/1e3), color = "darkred", linetype = 1, size = 1.5) +
  geom_line(aes(y = growth_rate/coeff), color = "darkblue", size= 1.5) +
  scale_y_continuous(
    "Cases (x1000)", 
    sec.axis = sec_axis(~ . * coeff, name = "Growth Rate")) +
  labs(title = paste0("initial GR as on ", as.Date(start_GR )))

plt_1 <- rbind(freq_lin_sim %>% select(YearWeek_imp_2, YearWeek, freq, data),
               freq_lin_data %>% select(YearWeek_imp_2, YearWeek, freq, data)) %>% 
  pivot_wider(names_from = data, values_from = freq) %>% 
  ggplot(aes(x = Model, y = Data, color = YearWeek_imp_2)) +
  geom_point(size = 2) +
  geom_abline(slope = 1, size = 1) +
  scale_color_manual(values =  Palcols(length(unique(freq_lin_data$YearWeek_imp_2)))) +
  scale_x_continuous(limits = c(0, 1)) + 
  scale_y_continuous(limits = c(0, 1)) + 
  annotate(geom = "text", x = 0.7, y = 0.2, label = paste("KLdist = ", round(as.numeric(KLdist$value_KL), 2))) +
  annotate(geom = "text", x = 0.7, y = 0.1, label = paste("KLdist_sim = ", round(as.numeric(KLdist$value_KL_sim), 2))) +
  theme(aspect.ratio = 1, legend.position = "none") 

plt_2 <- rbind(freq_lin_sim %>% select(YearWeek_imp_2, YearWeek, freq, data),
               freq_lin_data %>% select(YearWeek_imp_2, YearWeek, freq, data)) %>%
  ggplot(aes(x = YearWeek, y = freq, fill = YearWeek_imp_2)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values =  Palcols(length(unique(freq_lin_data$YearWeek_imp_2)))) +
  facet_wrap(~ data, nrow = 2) +
  labs(y = "Frequency", x = "Week of Sample", fill = "Week of importation") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))

#Make panel
plt_panel <- ggarrange(ggarrange(plt_0, plt_1, ncol = 2),
                       plt_2, heights=c(1,2), nrow = 2)
plot(plt_panel)
