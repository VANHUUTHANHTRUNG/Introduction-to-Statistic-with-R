library(ggplot2)
library(dplyr)

load("brfss2013.RData")

# unique(brfss2013$menthlth)
# unique(brfss2013$lastsmk2)
# 
# 
# menthlth_lastsmk2 <- brfss2013 %>%
#   filter(menthlth < 100, !is.na(menthlth), !is.na(lastsmk2)) %>%
#   group_by(menthlth) %>%
#   select(menthlth,lastsmk2) %>%
#   mutate(mental_group = cut(menthlth, c(-1,0,5,10,25,30),
#                             c("normal","light","imtermediate","extreme","chronic")))
# 
# menthlth_lastsmk2_percent <- menthlth_lastsmk2 %>%
#   group_by(mental_group,lastsmk2) %>%
#   summarise(count=n()) %>%
#   mutate(perc = count/sum(count))
# 
# ggplot(menthlth_lastsmk2_percent,aes(y=factor(mental_group), x=perc*100,fill=factor(lastsmk2),label=round(perc*100,1)))+
#   geom_bar(stat="identity",width =0.9 , position = position_dodge(width=0.9)) +
#   geom_text(position = position_dodge(width = 0.9),
#             vjust = 0.6,
#             size = 3) +
#   labs(x = "Disorder level", y = "%",fill = "Smoking group")

# ggplot(menthlth_lastsmk2, aes(x = menthlth, y = n))+
#   geom_bar(stat="identity")
# # TODO: divide into groups
# menthlth_lastsmk2_bucket <- menthlth_lastsmk2 %>%
#   filter(!is.na(lastsmk2))%>%
#   select_all() %>%
#   mutate(mental_group = cut(menthlth, c(-1,0,5,10,25,30),
#                             c("normal","light","imtermediate","extreme","chronic")))
# 
# # TODO:
# 
# menthlth_lastsmk2_mean <- menthlth_lastsmk2_bucket %>%
#   select_all() %>%
#   group_by(mental_group) %>%
#   summarise(mental_disorder_by_non_smoking_time = mean(n, na.rm = TRUE))
# 
# menthlth_mean <- menthlth_lastsmk2_bucket()
# 
# lastsmk2_menthlth_mean <- menthlth_lastsmk2_bucket %>%
#   select_all() %>%
#   group_by(lastsmk2) %>%
#   summarise(mental_disorder_per_smoking_time = mean(menthlth))


smokday2_genhlth <- brfss2013 %>%
  filter(!is.na(smokday2), !is.na(genhlth)) %>%
  group_by(genhlth) %>%
  select(genhlth, smokday2) %>%
  count(smokday2) %>%
  mutate(genhlth_percent = n/sum(n))

smokday2_genhlth <- fortify(smokday2_genhlth)

ggplot(data = smokday2_genhlth,aes(x= genhlth,y= genhlth_percent)) +
  geom_bar(position = "stack",stat = "identity", color = "black") +
  geom_col(aes(fill = genhlth_percent)) + geom_text(aes(label = smokday2,
                                                        y = genhlth_percent,
                                                        group = genhlth),
                                                    position = position_dodge(width = 0.9),
                                                    vjust = 1)+
  labs(x="General health evaluation",y ="Smoking status", title = "Health estimation in relationship with smoking")
