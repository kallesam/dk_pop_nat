library(tidyverse)
library(RColorBrewer)
library(paletteer)
library(wesanderson)
library(ggpubr)

gc()

#Figure 2 and 4
# read data
p_coef <- read_delim("", "\t", escape_double = FALSE, trim_ws = TRUE)
n_coef <- read_delim("", "\t", escape_double = FALSE, trim_ws = TRUE)
np_coef <- read_delim("", "\t", escape_double = FALSE, trim_ws = TRUE)

pop_1995 <- read_csv("") %>% filter(year==1995)
pop_2016 <- read_csv("") %>% filter(year==2016)

ndvi_1995 <- read_delim("", 
                        "\t", escape_double = FALSE, trim_ws = TRUE) %>% filter(year == 1995)
ndvi_2000 <- read_delim("", 
                        "\t", escape_double = FALSE, trim_ws = TRUE) %>% filter(year == 2000)
ndvi_2005 <- read_delim("", 
                        "\t", escape_double = FALSE, trim_ws = TRUE) %>% filter(year == 2005)
ndvi_2010 <- read_delim("", 
                        "\t", escape_double = FALSE, trim_ws = TRUE) %>% filter(year == 2010)
ndvi_2016 <- read_delim("", 
                        "\t", escape_double = FALSE, trim_ws = TRUE) %>% filter(year == 2016)

# Figure 2
pop_9516 <- pop_1995[,c(1:2,5)] %>%
  inner_join(pop_2016[,c(1:2,5)], by = c("x", "y"))
colnames(pop_9516)[3:4] <- c("1995", "2016")
pop_9516 <- pop_9516 %>% gather("year", "pop", -x, -y)
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

nat_9516 <- nat_1995[,c(1:2,6)] %>%
  right_join(nat_2000[,c(1:2,6)], by = c("x", "y")) %>%
  right_join(nat_2005[,c(1:2,6)], by = c("x", "y")) %>%
  right_join(nat_2010[,c(1:2,6)], by = c("x", "y")) %>%
  right_join(nat_2016[,c(1:2,6)], by = c("x", "y"))
colnames(nat_9516)[3:7] <- c("1995", "2000", "2005", "2010", "2016")
nat_9516 <- nat_9516 %>% gather("year", "nat", -x, -y)
nat_9516 <- nat_9516 %>%
  inner_join(n_coef[,c(1:2,6)] , by = c("x", "y"))
nat_9516$change <- "Stable"
nat_9516$change[nat_9516$n500_c<0] <- "Decrease"
nat_9516$change[nat_9516$n500_c>0] <- "Increase"

cols <- c("Decrease" = "darkred", "Stable" = "grey", "Increase" = "darkgreen")

p_lines <- pop_9516 %>%
  group_by(year) %>%
  summarize(median = median(pop, na.rm=TRUE))

p_hist <-pop_9516 %>%
  ggplot(aes(x = log10(pop), fill = change)) +
  geom_histogram(alpha = 0.7, bins = 30) +
  geom_vline(data = p_lines, aes(xintercept = log10(median))) +
  facet_grid(year ~ .) +
  scale_fill_manual(values = cols) +
  scale_color_manual(values = cols) +
  labs(x = "Population (log10 scale)", y  = "Count") +
  ggtitle("A") +
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
  ggtitle("B") +
  guides(fill = guide_legend(title=NULL), color = F) +
  theme_light()

ggarrange(p_hist, n_hist, nrow = 1, common.legend = T, legend = "right")

# Figure 4
natpop_9516 <- nat_9516[,c(1:4)] %>%
  filter(year == "1995" | year == "2016") %>%
  inner_join(pop_9516[,c(1:4)], by = c("x", "y", "year")) %>%
  inner_join(np_coef[,c(1:2,6)], by = c("x", "y"))
natpop_9516$change <- "Stable"
natpop_9516$change[natpop_9516$np500_c<0] <- "Decrease"
natpop_9516$change[natpop_9516$np500_c>0] <- "Increase"
natpop_9516$np <- natpop_9516$nat*900*861/natpop_9516$pop

centres <- natpop_9516 %>%
  group_by(change, year) %>%
  summarize(nat = mean(nat, na.rm=TRUE),
            pop = median(pop, na.rm=TRUE))

natpop_9516 %>%
  ggplot(aes(x = log10(pop), y = nat, z = np500_c)) + 
  stat_density2d(aes(fill = ..density..,
                     alpha = (..density..)^(1/5)),
                 contour = F, geom = "tile") +
  geom_point(data = centres, aes(z = 1), color = "black") +
  facet_wrap(year ~ change) +
  xlim(1.5, 3.75) +
  ylim(0.3, 0.6) +
  scale_fill_paletteer_c("gameofthrones::targaryen2", name ="Density") +
  labs(x = "Population (log10 scale)", y  = "Mean NDVI") +
  guides(alpha = F, fill = F) +
  theme_light()

# Figure 6
# 6A and 6B
ndvi_all_lan <- read_delim("", ";", escape_double = FALSE, trim_ws = TRUE)
ndvi_mid_lan <- read_delim("", ";", escape_double = FALSE, trim_ws = TRUE)
ndvi_all_mod <- read_delim("", ";", escape_double = FALSE, trim_ws = TRUE)
ndvi_mid_mod <- read_delim("", ";", escape_double = FALSE, trim_ws = TRUE)

A_plotdata <- cbind(ndvi_all_lan[,1:2], ndvi_mid_lan[,2],
                  ndvi_all_mod[,2], ndvi_mid_mod[,2])
colnames(A_plotdata)[2:5] <- c("All summer, Landsat", "Midsummer, Landsat",
                             "All summer, MODIS", "Midsummer, MODIS")
A_plotdata <- A_plotdata %>% gather(dataset, value, "All summer, Landsat":"Midsummer, MODIS")

B_plotdata <- as.data.frame(cbind(ndvi_all_lan[,1],
                                  ndvi_all_lan[,2]/ndvi_all_lan[1,2],
                                  ndvi_mid_lan[,2]/ndvi_mid_lan[1,2],
                                  ndvi_all_mod[,2]/ndvi_all_mod[1,2],
                                  ndvi_mid_mod[,2]/ndvi_mid_mod[1,2]))
colnames(B_plotdata)[1:5] <- c("year", "All summer, Landsat", "Midsummer, Landsat",
                               "All summer, MODIS", "Midsummer, MODIS")
B_plotdata <- B_plotdata %>% gather(dataset, value, "All summer, Landsat":"Midsummer, MODIS")

A <- A_plotdata %>%
  ggplot(aes(x = year, y = value, color = dataset)) +
  geom_smooth(linetype = "dashed", method='lm', se = F) +
  geom_line() +
  scale_color_manual(values = wes_palette("Moonrise2", n = 4)) +
  labs(title = "A", x = "Year", y  = "Mean NDVI", color = "Dataset") +
  theme_light()

B <- B_plotdata %>%
  ggplot(aes(x = year, y = value, color = dataset)) +
  geom_smooth(linetype = "dashed", method='lm', se = F) +
  geom_line() +
  scale_color_manual(values = wes_palette("Moonrise2", n = 4)) +
  labs(title = "B", x = "Year", y  = "Index (compared to 2000)", color = "Dataset") +
  theme(plot.title = element_text(size=12)) +
  theme_light()

AB <- ggarrange(A, B, ncol = 1, common.legend = T, legend = "right")

# 6C and 6D
ndvi_popdens <- read_delim("", ";", escape_double = FALSE, trim_ws = TRUE) %>%
  .[,c(1,6,8,10)]
colnames(ndvi_popdens)[2:4] <- c("Below 400", "From 400 to 1200",
                             "Over 1200")
ndvi_popdens <- ndvi_popdens %>%
  gather(Population, value, "Below 400":"Over 1200")

ndvi_popdens_s <- as.data.frame(cbind(ndvi_popdens[,1],
                                      ndvi_popdens[,2]/ndvi_popdens[1,2],
                                      ndvi_popdens[,3]/ndvi_popdens[1,3],
                                      ndvi_popdens[,4]/ndvi_popdens[1,4]))
colnames(ndvi_popdens_s)[1:4] <- c("year", "Below 400", "From 400 to 1200",
                                   "Over 1200")
ndvi_popdens_s <- ndvi_popdens_s %>%
  gather(Population, value, "Below 400":"Over 1200")

C <- ndvi_popdens %>%
  ggplot(aes(x = year, y = value, color = Population)) +
  geom_smooth(linetype = "dashed", method='lm', se = F) +
  geom_line() +
  scale_color_manual(values = wes_palette("Darjeeling1", n = 3)) +
  labs(title = "C", x = "Year", y  = "Mean NDVI", color = "Population") +
  theme_light()

D <- ndvi_popdens_s %>%
  ggplot(aes(x = year, y = value, color = Population)) +
  geom_smooth(linetype = "dashed", method='lm', se = F) +
  geom_line() +
  scale_color_manual(values = wes_palette("Darjeeling1", n = 3)) +
  labs(title = "D", x = "Year", y  = "Index (compared to 1995)", color = "Population") +
  theme_light()

CD <- ggarrange(C, D, ncol = 2, common.legend = T, legend = "right")

# 6E
temp_prec <- read_delim("", ";", escape_double = FALSE, trim_ws = TRUE)

E_plotdata <- as.data.frame(cbind(ndvi_all_lan[,1:2], temp_prec[,c(10,17)]))
colnames(E_plotdata) <- c("year", "NDVI", "Temperature", "Precipitation")
E_plotdata <- E_plotdata %>% gather(Variable, value, "NDVI":"Precipitation")
E_plotdata$Variable <- as.factor(E_plotdata$Variable)

E <- E_plotdata %>%
  ggplot(aes(x = year, y = value, color = Variable)) +
  geom_smooth(linetype = "dashed", method='lm', se = F) +
  geom_line() +
  facet_wrap(~Variable, ncol = 1, scales = "free",
             labeller = as_labeller(c("NDVI" = "NDVI",
                                      "Temperature" = "Temperature (avg. degrees C°)",
                                      "Precipitation" = "Precipitation (total mm)"))) +
  scale_color_manual(values = wes_palette("GrandBudapest1", n = 3)) +
  labs(title = "E", x = "Year", y = "") +
  theme_light()

#complete graph
AD <- ggarrange(AB, CD, ncol = 1)
ggarrange(AD, E, nrow = 1, widths = c(1.5,1))