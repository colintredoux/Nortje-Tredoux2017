##  This file draws graphs, after some corrections
##  for the Nortje & Tredoux chapter in the Bindemann
##  and Megreya book and ? more

# load some libraries-------------
library(pacman)
p_load(tidyverse, ggplot2, haven, cowplot, magrittr)

# define functions
dprime <- function(hit,fa) {
    qnorm(hit) - qnorm(fa)
}



# Exp 1-------------------
# load the data for exp1
exp1.data <- read_spss("Exp1.sav")

#  THIS SECTION DOESN'T WORK - NEEDS RETHINKING
# correct pair data for difference in base p
# that is, since p(random match) is greater in match pairs
# condition, we scale Hits and FPs up or down to correct this
# and then recompute dprime

# exp1.data.rev <- exp1.data  %>% 
#                 mutate(MatchHits_prop_revise = 
#                            MatchHits.Prop - (1 - MatchHits.Prop)/Setsize,
#                        MatchFas_prop_revise = 
#                            1 - MatchHits_prop_revise, 
#                 MatchHits_prop_revise =
#                     recode(MatchHits_prop_revise, '0' = 0.05, '1' = 0.95),
#                 MatchFas_prop_revise = 
#                     recode(MatchFas_prop_revise, '0' = 0.05, '1' = 0.95),
#                 Matchd = dprime(MatchHits_prop_revise, 
#                                 MatchFas_prop_revise)
#                 )
        
           
#  Draw multi-panel graph
#  but first make long format datafile


face <- exp1.data %>% 
    select(ParticipantNumber, Setsize, contains(".Prop")) %>% 
    select(ParticipantNumber, Setsize, contains("Face")) %>% 
    gather(face, face_measure, FaceHits.Prop:FaceFas.Prop)
facts <- exp1.data %>% 
    select(ParticipantNumber, Setsize, contains(".Prop")) %>% 
    select(contains("Facts")) %>% 
    gather(facts, facts_measure, FactsHits.Prop:FactsFas.Prop)
match <- exp1.data %>% 
    select(ParticipantNumber, Setsize, contains(".Prop")) %>% 
    select(contains("Match")) %>% 
    gather(match, match_measure, MatchHits.Prop:MatchFas.Prop)
gdat <- cbind(face, facts, match)


p1 <- ggplot(gdat, aes(x = Setsize, y = face_measure, linetype = face)) +
        geom_smooth(color = "black") + theme_bw(base_size = 10) + 
        theme(legend.position = "none") +
        xlab ("Set Size") + ylab("Proportion of total") +
        annotate("text", x = 20, y = 0.9, label = "Face : Hits",
                 size = 3)+
        annotate("text", x = 20, y = 0.45, label = "Face : False Alarms",
                 size = 3) +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank())+
        scale_x_continuous(breaks = c(1, 2, 3, 5, 10, 15, 30))
p1
p2 <- ggplot(gdat, aes(x = Setsize, y = facts_measure, linetype = facts)) +
    geom_smooth(color = "black") + theme_bw(base_size = 10) + 
    theme(legend.position = "none") +
    xlab ("Set Size") + ylab("Proportion of total") +
    annotate("text", x = 20, y = 1.0, label = "Facts : Hits", 
             size = 3)+
    annotate("text", x = 20, y = 0.18, label = "Facts : False Alarms",
             size = 3) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    scale_x_continuous(breaks = c(1, 2, 3, 5, 10, 15, 30))

    
p2
p3 <- ggplot(gdat, aes(x = Setsize, y = match_measure, linetype = facts)) +
    geom_smooth(color = "black") + theme_bw(base_size = 10) + 
    theme(legend.position = "none") +
    xlab ("Set Size") + ylab("Proportion of total") +
    annotate("text", x = 20, y = 0.50, label = "Match : Hits", size = 3)+
    annotate("text", x = 20, y = 1.0, label = "Match : False Alarms",
             size = 3) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    scale_x_continuous(breaks = c(1, 2, 3, 5, 10, 15, 30))

p3


p4 <- plot_grid(p1,p2,p3, nrow =1, rel_widths = c(1.2, 1, 1))
save_plot("Fig 1.pdf", p4, base_aspect_ratio = 1.9)
rm(exp1.data,face,facts,match,p1,p2,p3,p4)


# Exp 2-------------------
# load the data for exp2
exp2.data <- read_spss("eyewitness_reduced.sav")

exp2.data.fig2 <- 
    exp2.data %>% 
        filter(row_number() %in% 1:200) %>% 
        gather(measure, value, PercCorrectLineupIDs:PercCorrectPairings)


p4 <- ggplot(exp2.data.fig2, aes(x = NumberofTargets, y = value, 
                                 linetype = measure)) +
    geom_smooth(span = 1.3, , color = "black") + theme_bw(base_size = 16) + 
    theme(legend.position = "none") +
    xlab ("No. of Perpetrators") + ylab("Proportion correct") +
    annotate("text", x = 2.5, y = 0.77, label = "Roles", size = 5)+
    annotate("text", x = 2, y = 0.43, label = "Person id", size = 5) +
    annotate("text", x = 2.5, y = 0.14, label = "Role-Person pairings", size = 5) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())+
    scale_x_continuous(breaks = c(1, 2, 3, 5, 10),
                       labels = c("1", "2", "3", "5", "10"))
pdf("Fig 2.pdf")
p4
dev.off()

exp2.data.fig2b <- 
    exp2.data.fig2 %>% 
        group_by(measure, NumberofTargets) %>% 
        summarise(mean = mean(value, na.rm = T), 
                  sd = sd(value, na.rm = T), n = n(), 
                  se = sd/sqrt(n), bci = mean - se*1.96, uci = mean + 1.96*se)

ggplot(exp2.data.fig2b, aes(x = NumberofTargets, y = mean, group = measure))+
    geom_line()








