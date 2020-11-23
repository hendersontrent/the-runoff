#--------------------------------------------
# This script sets out to produce a Bayesian 
# network of voter political behaviour
#
# NOTE: Data from:
# https://dataverse.ada.edu.au/dataset.xhtml?persistentId=doi:10.26193/KMAMMW
#--------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 23 November 2020
#------------------------------------------

#----------------------- Pre processing ----------------------------

d <- read_excel("politics/data/aes19_unrestricted.xlsx")

d1 <- as.data.frame(d) %>%
  filter(A4_1 != 999) %>%
  filter(A4_2 != 999) %>%
  filter(A4_3 != 999) %>%
  filter(A4_4 != 999) %>%
  filter(A4_5 != 999) %>%
  filter(A4_6 != 999) %>%
  filter(D11_1 != 999) %>%
  filter(D11_2 != 999) %>%
  filter(D11_3 != 999) %>%
  mutate(A4_1 = as.character(A4_1),
         A4_2 = as.character(A4_2),
         A4_3 = as.character(A4_3),
         A4_4 = as.character(A4_4),
         A4_5 = as.character(A4_5),
         A4_6 = as.character(A4_6),
         D11_1 = as.character(D11_1),
         D11_2 = as.character(D11_2),
         D11_3 = as.character(D11_3)) %>%
  rename(politics_person = A4_1,
         politics_online = A4_2,
         persuade_others = A4_3,
         public_support = A4_4,
         attend_rallies = A4_5,
         contribute_money = A4_6,
         cost_of_living = D11_1,
         school_quality = D11_2,
         health_quality = D11_3) %>%
  dplyr::select(c(politics_person, politics_online, persuade_others, 
                  public_support, attend_rallies, contribute_money)) %>%
  mutate(politics_person = as.factor(politics_person),
         politics_online = as.factor(politics_online),
         persuade_others = as.factor(persuade_others),
         public_support = as.factor(public_support),
         attend_rallies = as.factor(attend_rallies),
         contribute_money = as.factor(contribute_money)) %>%
  rename(`Discuss politics in person` = politics_person,
         `Discuss politics online` = politics_online,
         `Persuade others` = persuade_others,
         `Show public support` = public_support,
         `Attend rallies` = attend_rallies,
         `Contribute money to campaign` = contribute_money)

#----------------------- Modelling & data vis ----------------------

model <- hc(d1)

viewer(model,
       bayesianNetwork.width = "100%",
       bayesianNetwork.height = "80vh",
       bayesianNetwork.layout = "layout_components",
       bayesianNetwork.title = "Bayesian Network of Voter Behaviour",
       bayesianNetwork.subtitle = "Edges (lines) indicate directed probabilistic relationships.",
       bayesianNetwork.footer = "Source: Australian Election Study, 2019",
       node.colors = list(background = "#A09BE7",
                          border = "#2274A5",
                          highlight = list(background = "#FF686B",
                                           border = "#861657")),
       node.font = list(color = "#2274A5"),
       edges.smooth = TRUE)


bayesianNetwork.boot.strength = boot.strength(d1, R = 20, algorithm = "hc")

avg.bayesianNetwork = averaged.network(bayesianNetwork.boot.strength, threshold = 0.2)

strength.viewer(
  avg.bayesianNetwork,
  bayesianNetwork.boot.strength,
  bayesianNetwork.background = "#white",
  bayesianNetwork.arc.strength.threshold.expression = c("@threshold < 1",
                                                        "@threshold >= 1"),
  
  bayesianNetwork.arc.strength.threshold.expression.color  = c("#861657", "#93E1D8"),
  bayesianNetwork.arc.strength.threshold.alternative.color =  "white",
  
  bayesianNetwork.arc.strength.label = TRUE,
  bayesianNetwork.arc.strength.label.prefix = "",
  bayesianNetwork.arc.strength.label.color = "#2274A5",
  
  bayesianNetwork.arc.strength.tooltip = TRUE,
  
  bayesianNetwork.edge.scale.min = 1,
  bayesianNetwork.edge.scale.max = 3,
  
  bayesianNetwork.edge.scale.label.min = 14,
  bayesianNetwork.edge.scale.label.max = 14,
  
  bayesianNetwork.title = "Bayesian Network of Voter Behaviour",
  bayesianNetwork.subtitle = "Source: Australian Election Study, 2019",
  bayesianNetwork.width = "100%",
  bayesianNetwork.height = "800px",
  bayesianNetwork.layout = "layout_components",
  node.colors = list(background = "#A09BE7",
                     border = "#2274A5",
                     highlight = list(background = "#FF686B",
                                      border = "#861657")),
  node.font = list(color = "#2274A5"),
  edges.dashes = FALSE)
