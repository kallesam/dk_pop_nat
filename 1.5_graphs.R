library(tidyverse)
library(RColorBrewer)
library(scales)
library(paletteer)
library(wesanderson)
library(ggpubr)

rm(list=ls())
gc()

#Figure 2
# read data
addr <- read.table("address/geokoor2016.txt",
                   header = T) %>%
  filter(match_bopindex != "C") %>%
  filter(match_bopindex != "D")
colnames(addr)[3] <- "index"

p_coef <- read_delim("coef/dk95_p_coef.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
p_coef$p500_c[p_coef$p500_p>0.05] <- 0
n_coef <- read_delim("coef/dk95_n_coef.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
n_coef$n250_c[n_coef$n250_p>0.05] <- 0
n_coef$n500_c[n_coef$n500_p>0.05] <- 0
n_coef$n1000_c[n_coef$n1000_p>0.05] <- 0

pop_1995 <- read_csv("pop_density/popdensity8116.csv") %>% filter(year==1995)
pop_2016 <- read_csv("pop_density/popdensity8116.csv") %>% filter(year==2016)
pop_9516 <- pop_1995[,c(1:2,5)] %>%
  inner_join(pop_2016[,c(1:2,5)], by = c("x", "y"))
pop_9516$nbhd <- "From 400 to 1200"
pop_9516$nbhd[pop_9516$count_500m.x<400] <- "Below 400"
pop_9516$nbhd[pop_9516$count_500m.x>1200] <- "Over 1200"
colnames(pop_9516)[3:4] <- c("1995", "2016")
pop_9516 <- pop_9516 %>% gather("year", "pop", -x, -y, -nbhd)
pop_9516 <- pop_9516 %>%
  inner_join(p_coef[,c(1:2,6)] , by = c("x", "y"))
pop_9516$change <- "Stable"
pop_9516$change[pop_9516$p500_c<0] <- "Decrease"
pop_9516$change[pop_9516$p500_c>0] <- "Increase"

# randomize low-end values
pop_9516$pop[pop_9516$pop==25] <- runif(length(pop_9516$pop[pop_9516$pop==25]),
                                        1, 50)
pop_9516$pop[pop_9516$pop==75] <- runif(length(pop_9516$pop[pop_9516$pop==75]),
                                        51, 100)
pop_9516$pop[pop_9516$pop==125] <- runif(length(pop_9516$pop[pop_9516$pop==125]),
                                         101, 150)
pop_9516$pop[pop_9516$pop==175] <- runif(length(pop_9516$pop[pop_9516$pop==175]),
                                         151, 200)
pop_9516$pop[pop_9516$pop==225] <- runif(length(pop_9516$pop[pop_9516$pop==225]),
                                         201, 250)
pop_9516$pop[pop_9516$pop==275] <- runif(length(pop_9516$pop[pop_9516$pop==275]),
                                         251, 300)
pop_9516$pop[pop_9516$pop==325] <- runif(length(pop_9516$pop[pop_9516$pop==325]),
                                         301, 350)
pop_9516$pop[pop_9516$pop==375] <- runif(length(pop_9516$pop[pop_9516$pop==375]),
                                         351, 400)
pop_9516$pop[pop_9516$pop==425] <- runif(length(pop_9516$pop[pop_9516$pop==425]),
                                         401, 450)

ndvi_1995 <- read_delim("geokoor_ndvi/geokoor_ndvi.txt", 
                        "\t", escape_double = FALSE, trim_ws = TRUE) %>% filter(year == 1995)
ndvi_2000 <- read_delim("geokoor_ndvi/geokoor_ndvi.txt", 
                        "\t", escape_double = FALSE, trim_ws = TRUE) %>% filter(year == 2000)
ndvi_2005 <- read_delim("geokoor_ndvi/geokoor_ndvi.txt", 
                        "\t", escape_double = FALSE, trim_ws = TRUE) %>% filter(year == 2005)
ndvi_2010 <- read_delim("geokoor_ndvi/geokoor_ndvi.txt", 
                        "\t", escape_double = FALSE, trim_ws = TRUE) %>% filter(year == 2010)
ndvi_2016 <- read_delim("geokoor_ndvi/geokoor_ndvi.txt", 
                        "\t", escape_double = FALSE, trim_ws = TRUE) %>% filter(year == 2016)
nat_9516 <- ndvi_1995[,c(1:3,6)] %>%
  right_join(ndvi_2000[,c(3,6)], by = "index") %>%
  right_join(ndvi_2005[,c(3,6)], by = "index") %>%
  right_join(ndvi_2010[,c(3,6)], by = "index") %>%
  right_join(ndvi_2016[,c(3,6)], by = "index")
colnames(nat_9516)[4:8] <- c("1995", "2000", "2005", "2010", "2016")
nat_9516 <- nat_9516 %>% gather("year", "nat", -x, -y, -index)
nat_9516 <- nat_9516 %>%
  inner_join(n_coef[,c(3,6)] , by = "index")
nat_9516$change <- "Stable"
nat_9516$change[nat_9516$n500_c<0] <- "Decrease"
nat_9516$change[nat_9516$n500_c>0] <- "Increase"

table(nat_9516$change)
length(n_coef$n250_c[n_coef$n250_c==0])/2085202
length(n_coef$n500_c[n_coef$n500_c==0])/2085202
length(n_coef$n1000_c[n_coef$n1000_c==0])/2085202

cols <- c("Decrease" = "darkred", "Stable" = "grey", "Increase" = "darkgreen")

p_lines <- pop_9516 %>%
  group_by(year) %>%
  summarize(median = median(pop, na.rm=TRUE))

p_hist <-pop_9516 %>%
  ggplot(aes(x = pop, fill = change)) +
  geom_histogram(alpha = 0.7, bins = 30) +
  geom_vline(data = p_lines, aes(xintercept = median)) +
  facet_grid(year ~ .) +
  scale_fill_manual(values = cols) +
  scale_color_manual(values = cols) +
  labs(x = "Population", y  = "Count") +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_continuous(labels = comma) +
  ggtitle("(a)") +
  guides(fill = guide_legend(title=NULL), color = F) +
  theme_light()

n_lines <- nat_9516 %>%
  group_by(year) %>%
  summarize(mean = mean(nat, na.rm = TRUE), sd = sd(nat, na.rm = T))

n_hist <-nat_9516 %>%
  ggplot(aes(x = nat, fill = change)) +
  geom_histogram(alpha = 0.7, bins = 60) +
  geom_vline(data = n_lines, aes(xintercept = mean)) +
  facet_grid(year ~ .) +
  scale_fill_manual(values = cols) +
  scale_color_manual(values = cols) +
  xlim(0.1, 0.7) +
  labs(x = "Mean NDVI", y  = "Count") +
  scale_y_continuous(labels = comma) +
  ggtitle("(b)") +
  guides(fill = guide_legend(title=NULL), color = F) +
  theme_light()

ggarrange(p_hist, n_hist, nrow = 1, common.legend = T, legend = "right")


# Figure 6A
natpop_9516 <- nat_9516 %>%
  filter(year == "1995" | year == "2016") %>%
  inner_join(pop_9516, by = c("x", "y", "year"))

natpop_9516$index <- as.(natpop_9516$index)
natpop_9516_f <- natpop_9516 %>%
  left_join(addr, by = c("x", "y")) %>%
  filter(index.x == index.y)

table(natpop_9516_f$nbhd)/4116184
centres <- natpop_9516_f %>%
  group_by(nbhd, year) %>%
  summarize(nat = median(nat, na.rm=TRUE),
            pop = median(pop, na.rm=TRUE))

A <- natpop_9516_f %>%
  ggplot(aes(x = pop, y = nat)) + 
  stat_density2d(aes(fill = ..ndensity..,
                     alpha = (..density..)^(1/5),
                     colour = NA),
                contour = F, geom = "tile"#, show.legend = F
                 ) +
  #geom_bin2d(aes(alpha = (..count..)^(1/10)),
   #          bins = 60) +
  geom_point(data = centres, aes(colour = nbhd)) +
  facet_grid(. ~ year) +
  #xlim(15, 10000) +
  #ylim(0.25, 0.65) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_fill_gradient(low = "white", high = "black") +
  scale_color_manual(values = wes_palette("Darjeeling1", n = 3)) +
  labs(title = "(a)", x = "Population (log10 scale)", y  = "NDVI",
       color = "Population", fill = "Address point density") +
  guides(alpha = F) +
  theme_light()
A

# 6B and 6C, redo the same for july

pop <- read_csv("pop_density/popdensity8116.csv") %>% .[,c(1:3,5)] %>%
  filter(year > 1994) %>% filter(year < 2017)
nat <- read_delim("geokoor_ndvi/geokoor_ndvi.txt", 
                  "\t", escape_double = FALSE, trim_ws = TRUE) %>% .[,c(1:4,6)]

ndvi_popdens_med <- NULL
for(i in seq(1995, 2016)){
  print(i)
  temp_pop <- pop %>% filter(year == i)
  temp_nat <- nat %>% filter(year == i)
  temp_val <- temp_nat %>%
    inner_join(temp_pop, by = c("x", "y"))
  ndvi_popdens_med <- rbind(ndvi_popdens_med,
                       c(i,
                         median(na.omit(temp_val$n500[temp_val$count_500m<400])),
                         median(na.omit(temp_val$n500[temp_val$count_500m>400 & temp_val$count_500m<1200])),
                         median(na.omit(temp_val$n500[temp_val$count_500m>1200]))))
}
ndvi_popdens_med <- as.data.frame(ndvi_popdens_med)
colnames(ndvi_popdens_med) <- c("year", "Below 400", "From 400 to 1200", "Over 1200")

ndvi_popdens_20 <- NULL
for(i in seq(1995, 2016)){
  print(i)
  temp_pop <- pop %>% filter(year == i)
  temp_nat <- nat %>% filter(year == i)
  temp_val <- temp_nat %>%
    inner_join(temp_pop, by = c("x", "y"))
  ndvi_popdens_20 <- rbind(ndvi_popdens_20,
                            c(i,
                              quantile(na.omit(temp_val$n500[temp_val$count_500m<400]), .20),
                              quantile(na.omit(temp_val$n500[temp_val$count_500m>400 & temp_val$count_500m<1200]), .20),
                              quantile(na.omit(temp_val$n500[temp_val$count_500m>1200]), .20)))
}
ndvi_popdens_20 <- as.data.frame(ndvi_popdens_20)
colnames(ndvi_popdens_20) <- c("year", "Below 400", "From 400 to 1200", "Over 1200")

ndvi_popdens_80 <- NULL
for(i in seq(1995, 2016)){
  print(i)
  temp_pop <- pop %>% filter(year == i)
  temp_nat <- nat %>% filter(year == i)
  temp_val <- temp_nat %>%
    inner_join(temp_pop, by = c("x", "y"))
  ndvi_popdens_80 <- rbind(ndvi_popdens_80,
                           c(i,
                             quantile(na.omit(temp_val$n500[temp_val$count_500m<400]), .80),
                             quantile(na.omit(temp_val$n500[temp_val$count_500m>400 & temp_val$count_500m<1800]), .80),
                             quantile(na.omit(temp_val$n500[temp_val$count_500m>1800]), .80)))
}
ndvi_popdens_80 <- as.data.frame(ndvi_popdens_80)
colnames(ndvi_popdens_80) <- c("year", "Below 400", "From 400 to 1200", "Over 1200")

ndvi_popdens_med <- ndvi_popdens_med %>%
  gather(Population, value, "Below 400":"Over 1200")
ndvi_popdens_20 <- ndvi_popdens_20 %>%
  gather(Population, value, "Below 400":"Over 1200")
ndvi_popdens_80 <- ndvi_popdens_80 %>%
  gather(Population, value, "Below 400":"Over 1200")

ndvi_popdens_med_w <- ndvi_popdens_med %>%
  spread(Population, value)
summary(lm(`Below 400` ~ year, data = ndvi_popdens_med_w))
summary(lm(`Below 400` ~ year + I(year^2), data = ndvi_popdens_med_w)) # better, 1.23-3.06e-4
summary(lm(`From 400 to 1200` ~ year, data = ndvi_popdens_med_w)) # better, 0.0017
summary(lm(`From 400 to 1200` ~ year + I(year^2), data = ndvi_popdens_med_w))
summary(lm(`Over 1200` ~ year, data = ndvi_popdens_med_w)) # better, 0.0016
summary(lm(`Over 1200` ~ year + I(year^2), data = ndvi_popdens_med_w))

write.table(ndvi_popdens_july_med, file = "ndvi_popdens_med", sep = "\t", row.names = F)
write.table(ndvi_popdens_20, file = "ndvi_popdens_20", sep = "\t", row.names = F)
write.table(ndvi_popdens_80, file = "ndvi_popdens_80", sep = "\t", row.names = F)
ndvi_popdens_med <- read_delim("ndvi_popdens_med", "\t", escape_double = FALSE, trim_ws = TRUE)
ndvi_popdens_20 <- read_delim("ndvi_popdens_20", "\t", escape_double = FALSE, trim_ws = TRUE)
ndvi_popdens_80 <- read_delim("ndvi_popdens_80", "\t", escape_double = FALSE, trim_ws = TRUE)


nat_july <- read_delim("geokoor_ndvi/geokoor_ndvi_july.txt", 
                  "\t", escape_double = FALSE, trim_ws = TRUE)

ndvi_popdens_july_med <- NULL
for(i in seq(1995, 2016)){
  print(i)
  temp_pop <- pop %>% filter(year == i)
  temp_nat <- nat_july %>% filter(year == i)
  temp_val <- temp_nat %>%
    inner_join(temp_pop, by = c("x", "y"))
  ndvi_popdens_july_med <- rbind(ndvi_popdens_july_med,
                            c(i,
                              median(na.omit(temp_val$n500[temp_val$count_500m<400])),
                              median(na.omit(temp_val$n500[temp_val$count_500m>400 & temp_val$count_500m<1200])),
                              median(na.omit(temp_val$n500[temp_val$count_500m>1200]))))
}
ndvi_popdens_july_med <- as.data.frame(ndvi_popdens_july_med)
colnames(ndvi_popdens_july_med) <- c("year", "Below 400", "From 400 to 1200", "Over 1200")

ndvi_popdens_july_20 <- NULL
for(i in seq(1995, 2016)){
  print(i)
  temp_pop <- pop %>% filter(year == i)
  temp_nat <- nat_july %>% filter(year == i)
  temp_val <- temp_nat %>%
    inner_join(temp_pop, by = c("x", "y"))
  ndvi_popdens_july_20 <- rbind(ndvi_popdens_july_20,
                           c(i,
                             quantile(na.omit(temp_val$n500[temp_val$count_500m<400]), .20),
                             quantile(na.omit(temp_val$n500[temp_val$count_500m>400 & temp_val$count_500m<1200]), .20),
                             quantile(na.omit(temp_val$n500[temp_val$count_500m>1200]), .20)))
}
ndvi_popdens_july_20 <- as.data.frame(ndvi_popdens_july_20)
colnames(ndvi_popdens_july_20) <- c("year", "Below 400", "From 400 to 1200", "Over 1200")

ndvi_popdens_july_80 <- NULL
for(i in seq(1995, 2016)){
  print(i)
  temp_pop <- pop %>% filter(year == i)
  temp_nat <- nat_july %>% filter(year == i)
  temp_val <- temp_nat %>%
    inner_join(temp_pop, by = c("x", "y"))
  ndvi_popdens_july_80 <- rbind(ndvi_popdens_july_80,
                           c(i,
                             quantile(na.omit(temp_val$n500[temp_val$count_500m<400]), .80),
                             quantile(na.omit(temp_val$n500[temp_val$count_500m>400 & temp_val$count_500m<1800]), .80),
                             quantile(na.omit(temp_val$n500[temp_val$count_500m>1800]), .80)))
}
ndvi_popdens_july_80 <- as.data.frame(ndvi_popdens_july_80)
colnames(ndvi_popdens_july_80) <- c("year", "Below 400", "From 400 to 1200", "Over 1200")

ndvi_popdens_july_med <- ndvi_popdens_july_med %>%
  gather(Population, value, "Below 400":"Over 1200")
ndvi_popdens_july_20 <- ndvi_popdens_july_20 %>%
  gather(Population, value, "Below 400":"Over 1200")
ndvi_popdens_july_80 <- ndvi_popdens_july_80 %>%
  gather(Population, value, "Below 400":"Over 1200")

ndvi_popdens_july_med_w <- ndvi_popdens_july_med %>%
  spread(Population, value)
summary(lm(`Below 400` ~ year, data = ndvi_popdens_july_med_w))
summary(lm(`Below 400` ~ year + I(year^2), data = ndvi_popdens_july_med_w)) # better, 1.23-3.06e-4
summary(lm(`From 400 to 1200` ~ year, data = ndvi_popdens_july_med_w)) # better, 0.0017
summary(lm(`From 400 to 1200` ~ year + I(year^2), data = ndvi_popdens_july_med_w))
summary(lm(`Over 1200` ~ year, data = ndvi_popdens_july_med_w)) # better, 0.0016
summary(lm(`Over 1200` ~ year + I(year^2), data = ndvi_popdens_july_med_w))

write.table(ndvi_popdens_july_med, file = "ndvi_popdens_july_med", sep = "\t", row.names = F)
write.table(ndvi_popdens_july_20, file = "ndvi_popdens_july_20", sep = "\t", row.names = F)
write.table(ndvi_popdens_july_80, file = "ndvi_popdens_july_80", sep = "\t", row.names = F)
ndvi_popdens_july_med <- read_delim("ndvi_popdens_july_med", "\t", escape_double = FALSE, trim_ws = TRUE)
ndvi_popdens_july_20 <- read_delim("ndvi_popdens_july_20", "\t", escape_double = FALSE, trim_ws = TRUE)
ndvi_popdens_july_80 <- read_delim("ndvi_popdens_july_80", "\t", escape_double = FALSE, trim_ws = TRUE)

?geom_smooth
B <- ndvi_popdens_med %>%
  ggplot(aes(x = year, y = value, color = Population, fill = Population)) +
  #geom_smooth(data = filter(ndvi_popdens_med, Population == "Below 400"),
  #            method = 'lm', formula = y ~ x + I(x^2), linetype = "dotted") +
  geom_smooth(method = 'lm', linetype = "dotted", alpha = 0.2) +
  geom_line(data = ndvi_popdens_20, linetype = "longdash") +
  geom_line(data = ndvi_popdens_80, linetype = "longdash") +
  geom_line() +
  ylim(0.3, 0.6) +
  #geom_line(data = ndvi_popdens_july_med, linetype = "dotdash", color = "black") +
  scale_color_manual(values = wes_palette("Darjeeling1", n = 3)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 3)) +
  labs(title = "(b)", x = "Year", y  = "NDVI", color = "Population") +
  stat_cor(p.digits = 1, r.digits = 2, show.legend = F,
           label.x.npc = 0.6, 
           label.y.npc = 0.1) +
  theme_light()
B
?stat_cor
C <- ndvi_popdens_july_med %>%
  ggplot(aes(x = year, y = value, color = Population, fill = Population)) +
  #geom_smooth(data = filter(ndvi_popdens_med, Population == "Below 400"),
  #            method = 'lm', formula = y ~ x + I(x^2), linetype = "dotted") +
  geom_smooth(method = 'lm', linetype = "dotted", alpha = 0.2) +
  geom_line(data = ndvi_popdens_july_20, linetype = "longdash") +
  geom_line(data = ndvi_popdens_july_80, linetype = "longdash") +
  geom_line() +
  ylim(0.3, 0.6) +
  scale_color_manual(values = wes_palette("Darjeeling1", n = 3)) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 3)) +
  labs(title = "(c)", x = "Year", y  = "NDVI", color = "Population") +
  stat_cor(p.digits = 2, r.digits = 2, show.legend = F,
           label.x.npc = 0.6, 
           label.y.npc = 0.1) +
  theme_light()
C
?ggarrange
BC <- ggarrange(B, C, ncol = 2, nrow = 1, common.legend = T, legend = "right")
BC
AC <- ggarrange(A, BC, ncol = 1)
AC

# Figure S2
# S2A and S2B
ndvi_all_lan <- read_delim("",
                           "\t", escape_double = FALSE, trim_ws = TRUE)
ndvi_mid_lan <- read_delim("",
                           "\t", escape_double = FALSE, trim_ws = TRUE)
ndvi_all_mod <- read_delim("",
                           "\t", escape_double = FALSE, trim_ws = TRUE)
ndvi_mid_mod <- read_delim("",
                           "\t", escape_double = FALSE, trim_ws = TRUE)

A_plotdata <- cbind(ndvi_all_lan[,1:2], ndvi_mid_lan[,2],
                    ndvi_all_mod[,2], ndvi_mid_mod[,2])
colnames(A_plotdata)[2:5] <- c("All summer, Landsat", "Midsummer, Landsat",
                               "All summer, MODIS", "Midsummer, MODIS")
A_plotdata <- A_plotdata %>% gather(dataset, value, "All summer, Landsat":"Midsummer, MODIS")

B_plotdata <- as.data.frame(cbind(ndvi_all_lan[,1],
                                  ndvi_all_lan[["mean"]]/ndvi_all_lan[[1,2]],
                                  ndvi_mid_lan[["mean"]]/ndvi_mid_lan[[1,2]],
                                  ndvi_all_mod[["mean"]]/ndvi_all_mod[[1,2]],
                                  ndvi_mid_mod[["mean"]]/ndvi_mid_mod[[1,2]]))
colnames(B_plotdata)[1:5] <- c("year", "All summer, Landsat", "Midsummer, Landsat",
                               "All summer, MODIS", "Midsummer, MODIS")
B_plotdata <- B_plotdata %>% gather(dataset, value, "All summer, Landsat":"Midsummer, MODIS")

S2A <- A_plotdata %>%
  ggplot(aes(x = year, y = value, color = dataset)) +
  geom_smooth(linetype = "dashed", method='lm', se = F) +
  geom_line() +
  scale_color_manual(values = brewer.pal(n = 4, name = "Paired")) +
  labs(title = "A", x = "Year", y  = "Mean NDVI", color = "Dataset") +
  theme_light()

S2B <- B_plotdata %>%
  ggplot(aes(x = year, y = value, color = dataset)) +
  geom_smooth(linetype = "dashed", method='lm', se = F) +
  geom_line() +
  scale_color_manual(values = brewer.pal(n = 4, name = "Paired")) +
  labs(title = "B", x = "Year", y  = "Index (compared to 2000)", color = "Dataset") +
  theme(plot.title = element_text(size=12)) +
  theme_light()

S2AB <- ggarrange(S2A, S2B, ncol = 2, common.legend = T, legend = "right")
S2AB
