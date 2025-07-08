## data_wrangling

## chang options to only run linear contrast for factors
options(contrasts = c("contr.treatment", "contr.treatment"))

# 1. Start from cowplot's theme with base text size 10
custom_theme <- theme_cowplot() +
  theme(
    # 2. Override the title size to 14
    plot.title    = element_text(size = 12, face = "bold"),
    # 3. (Optional) ensure subtitles, captions, axes, legend all at size 10
    plot.subtitle = element_text(size = 10),
    plot.caption  = element_text(size = 10),
    axis.title    = element_text(size = 10),
    axis.text     = element_text(size = 10),
    legend.title  = element_text(size = 10),
    legend.text   = element_text(size = 10)
  )

# Activate it globally:
theme_set(custom_theme)


## import data
data <- read_csv("data/merged_data.csv")
##
data <- data %>% 
  mutate(std_in_degree_2p = scale(in_degree_2p),
         std_in_degree_1p = scale(in_degree_1p))

edge_list <- read_csv('data/edgelist.csv')  %>% 
  filter(status == "Amistad",
         weight == 1)


facet_labels <- c("0" = "Non-Friend", "1" = "Friend")
facet_label2 <- c("nf" = "Non-Friend", "f" = "Friend")


data_app1 <- 
  data %>% 
  select(ID2, stealfriend, stealapp) %>% 
  mutate(treatment = factor(stealfriend, ordered = TRUE, levels = c(0, 1), labels = c("nf", "f")),
         rating = stealapp,
         domain = "stealing") %>% 
  select(ID2, treatment, rating, domain )

data_app2 <- 
  data %>% 
  select(ID2, phonefriend, phoneapp) %>% 
  mutate(treatment = factor(phonefriend, ordered = TRUE, levels = c(0, 1), labels = c("nf", "f")),
         rating = phoneapp,
         domain = "texting") %>% 
  select(ID2, treatment, rating, domain )

data_app3 <- 
  data %>% 
  select(ID2, interruptfriend, interruptapp) %>% 
  mutate(treatment = factor(interruptfriend, ordered = TRUE, levels = c(0, 1), labels = c("nf", "f")),
         rating = interruptapp,
         domain = "interrupt") %>% 
  select(ID2, treatment, rating, domain )

data_app4 <- 
  data %>% 
  select(ID2, insultfriend, insultapp) %>% 
  mutate(treatment = factor(insultfriend, ordered = TRUE, levels = c(0, 1), labels = c("nf", "f")),
         rating = insultapp,
         domain = "insult") %>% 
  select(ID2, treatment, rating, domain )

data_app5 <- 
  data %>% 
  select(ID2, onlinefriend, onlineapp) %>% 
  mutate(treatment = factor(onlinefriend, ordered = TRUE, levels = c(0, 1), labels = c("nf", "f")),
         rating = onlineapp,
         domain = "online")%>% 
  select(ID2, treatment, rating, domain )

data_app_all <- bind_rows(data_app1,
                          data_app2,
                          data_app3,
                          data_app4,
                          data_app5) %>% 
  filter(!is.na(rating)) %>% 
  mutate(rating_f = ordered(rating))



data_punish_steal <- 
  data %>% 
  select(ID2, stealfriend, stealnoth, stealremark, stealtalk, stealavoid, stealphyspunapp) %>% 
  pivot_longer(cols =  3:7, names_to = "punishment", values_to = "rating") %>% 
  filter(!is.na(rating)) %>% 
  mutate(rating_f = ordered(rating),
         treatment = factor(stealfriend, ordered = TRUE, levels = c(0, 1), labels = c("nf", "f")),
         domain = "stealing",    
         punishment_type = ifelse(punishment == "stealnoth", "do_nothing",
                                  ifelse(punishment == "stealremark","agry_remark", 
                                         ifelse(punishment == "stealtalk", "gossip",
                                                ifelse(punishment == "stealphyspunapp", "physical",
                                                "avoid"))))) %>% 
  filter(punishment_type != 'physical') ## put back in case we want to do something with this

data_punish_phone <- 
  data %>% 
  select(ID2, phonefriend, phonenoth, phoneremark, phonetalk, phoneavoid) %>% 
  pivot_longer(cols =  3:6, names_to = "punishment", values_to = "rating") %>% 
  filter(!is.na(rating)) %>% 
  mutate(rating_f = ordered(rating),
         treatment = factor(phonefriend, ordered = TRUE, levels = c(0, 1), labels = c("nf", "f")),
         domain = "texting",
         punishment_type = ifelse(punishment == "phonenoth", "do_nothing",
                                  ifelse(punishment == "phoneremark","agry_remark", 
                                         ifelse(punishment == "phonetalk", "gossip",
                                                "avoid"))))
data_punish_interrupt <- 
  data %>% 
  select(ID2, interruptfriend, interruptnoth, interruptremark, interrupttalk, interruptavoid) %>% 
  pivot_longer(cols =  3:6, names_to = "punishment", values_to = "rating") %>% 
  filter(!is.na(rating)) %>% 
  mutate(rating_f = ordered(rating),
         treatment = factor(interruptfriend, ordered = TRUE, levels = c(0, 1), labels = c("nf", "f")),
         domain = "interrupt",
         punishment_type = ifelse(punishment == "interruptnoth", "do_nothing",
                                  ifelse(punishment == "interruptremark","agry_remark", 
                                         ifelse(punishment == "interrupttalk", "gossip",
                                                "avoid"))))


data_punish_insult <- 
  data %>% 
  select(ID2, insultfriend, insultnoth, insultremark, insulttalk, insultavoid) %>% 
  pivot_longer(cols =  3:6, names_to = "punishment", values_to = "rating") %>% 
  filter(!is.na(rating)) %>% 
  mutate(rating_f = ordered(rating),
         treatment = factor(insultfriend, ordered = TRUE, levels = c(0, 1), labels = c("nf", "f")),
         domain = "insult",
         punishment_type = ifelse(punishment == "insultnoth", "do_nothing",
                                  ifelse(punishment == "insultremark","agry_remark", 
                                         ifelse(punishment == "insulttalk", "gossip",
                                                "avoid"))))

data_punish_online <- 
  data %>% 
  select(ID2, onlinefriend, onlinenothing, onlineremark, onlinetalk, onlineavoid) %>% 
  pivot_longer(cols =  3:6, names_to = "punishment", values_to = "rating") %>% 
  filter(!is.na(rating)) %>% 
  mutate(rating_f = ordered(rating),
         treatment = factor(onlinefriend, ordered = TRUE, levels = c(0, 1), labels = c("nf", "f")),
         domain = "online",
         punishment_type = ifelse(punishment == "onlinenothing", "do_nothing",
                                  ifelse(punishment == "onlineremark","agry_remark", 
                                         ifelse(punishment == "onlinetalk", "gossip",
                                                "avoid"))))

data_punish_all <- bind_rows(
  data_punish_insult,
  data_punish_online,
  data_punish_interrupt,
  data_punish_phone,
  data_punish_steal
) %>% 
  select(ID2, punishment, rating, rating_f, treatment, domain, punishment_type) %>% 
  mutate(rating_reverse_nothing = if_else(
    punishment_type == "do_nothing",
    6 - rating, 
    rating       
  )) 

data_punish_no_nothing <- data_punish_all %>% 
  filter(punishment_type != "do_nothing") %>% 
  select(-c("rating_reverse_nothing"))

network_info <- data %>% 
  select(ID2, in_degree_1n, in_degree_1p, in_degree_2p, in_degree_2n, `CC+I(in)`, `CC+II(in)`, `BC+I`, `BC+II`, `BC-`, `EC+I(in)`, `EC+II(in)`) %>% 
  mutate(std_in_degree_2p = scale(in_degree_2p),
         std_in_degree_1p = scale(in_degree_1p),
         std_in_degree_2n = scale(in_degree_2n),
         std_in_degree_1n = scale(in_degree_1n),
         net_indegree12 = ((in_degree_1p ) - ((in_degree_1n + in_degree_2n))),
         net_indegree22 = ((in_degree_1p + in_degree_2p) - ((in_degree_1n + in_degree_2n))),
         std_net_indegree12 = scale(net_indegree12),
         std_net_indegree22 = scale(net_indegree22),
         CC1 = `CC+I(in)`,
         CC2 = `CC+II(in)`,
         BC1 = `BC+I`,
         BC2 = `BC+II`,
         EC1 = `EC+I(in)`,
         EC2 = `EC+II(in)`
  )

net_appropriateness <- merge(network_info, data_app_all, by = "ID2")