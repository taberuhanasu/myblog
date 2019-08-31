library(tidyverse)

url <- "https://github.com/mitti1210/myblog/blob/master/fim.csv?raw=true"
dat <- read.csv(url)
names(dat) <- str_remove(names(dat), "FIM_")
dat <- 
  dat %>% 
  select(-運動合計:-合計)

head(dat)

dat_long <-
  dat %>% 
  gather(key = key, value = value, 食事:記憶, factor_key = TRUE)
dat_summarize <-
  dat_long %>% 
  group_by(key, sheet) %>% 
  summarize(mean = mean(value))
dat_count <- 
  dat_long %>% 
  group_by(key, sheet, value) %>% 
  summarize(n = n())

ggplot() +
  theme_classic(base_family = "HiraKakuPro-W3") +
  geom_line(data = dat_long, aes(x = sheet, y = value, group = 氏名), color = "gray", alpha = 0.2) +
  geom_line(data = dat_summarize, aes(x = sheet, y = mean, group = key), color = "red", size = 1.5) +
  geom_text(data = dat_summarize, aes(x = sheet, y = mean, label = mean), vjust = -0.5) +
  facet_wrap(~ key) + 
  labs(x = "", y = "")

ggplot() +
  theme_gray(base_family = "HiraKakuPro-W3") +
  geom_tile(data = dat_count, aes(x = sheet, y = value, fill = n)) +
  scale_fill_gradientn(colours = c("yellow", "red")) +
  geom_text(data = dat_count, aes(x = sheet, y = value, label = n)) +
  facet_wrap(~ key)

ggplot() +
  theme_gray(base_family = "HiraKakuPro-W3") +
  geom_tile(data = dat_count, aes(x = sheet, y = value, fill = n)) +
  scale_fill_gradientn(colours = c("yellow", "red")) +
  geom_line(data = dat_long, aes(x = sheet, y = value, group = 氏名), color = "gray", alpha = 0.2) +
  geom_line(data = dat_summarize, aes(x = sheet, y = mean, group = key), color = "red", size = 1.5) +
  geom_text(data = dat_count, aes(x = sheet, y = value, label = n)) +
  facet_wrap(~ key)

