dir()
library(ggplot2)

data<- read_xlsx("Feeding_spinosad.xlsx",col_names = TRUE) %>%
  mutate(
    colony = factor(colony)
     )
data

#### 
mo2 <- lm(cbind(Feeding) ~ Treatment + colony, data = data)
summary(mo2)

res_1 <- residuals(mo2); qqnorm(res_1); qqline(res_1)


###

mo3 <- lmer((Feeding) ~ Treatment + (1|colony), data = data)
summary(mo3)

res_1 <- residuals(mo3); qqnorm(res_1); qqline(res_1)


mo4 <- lm(Feeding~ Treatment + I(Treatment^2)+ colony, data= data)
anova(mo4, test= "F")

#####

anova(mo2, test= "F")
anova(mo3)


#######

AIC(mo2)
AIC(mo3)
AIC(mo4)


curve_mo4<- ggeffect(mo4, terms = c("Treatment[0.001:100 by= 0.25]"))
write.table(curve_mo4, file = "curve_mo4.csv", quote = FALSE, row.names = FALSE, sep = ",")
curve_mo4 <- read.csv("curve_mo4.csv") %>%  as_tibble() 
curve_mo4 %>% head()

means_mo4<- ggemmeans(mo4, terms = c("Treatment"))
write.table(means_mo4, file = "means_mo4.csv", quote = FALSE, row.names = FALSE, sep = ",")
means_mo4 <- read.csv("means_mo4.csv") %>%  as_tibble() 
means_mo4 %>% head()

ggplot(data = means_mo4, 
       aes(x = x, 
           y = predicted,
           ymin = conf.low, 
           ymax = conf.high
       )) + 
  geom_point()

p1 <- ggplot(data = curve_mo4, 
             aes(x = x, 
                 y = predicted,
                 ymin = conf.low, 
                 ymax = conf.high
             )) + 
  geom_line(size = 0.75) + 
  geom_ribbon(alpha=0.5) + 
  labs( 
    x = "Time (days)",
    y = "Number of insects")

p2 = p1 +
  scale_y_continuous(limits = c(0,75), 
                     breaks = seq(0,75, 15),
                     position = "right") +
  scale_x_continuous(limits = c(0,175), breaks = seq(0,175,25)) +
  labs(fill='Condition', linetype = 'Condition') # color = 'Condition'

p2 +
  theme(legend.position = "top") +
  coord_fixed(2.5)



