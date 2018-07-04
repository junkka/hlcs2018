# psm-life-course-graph.R

library(tidyverse)
library(forcats)
library(gridExtra)



tf_theme <- function(){
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 12, hjust = 0),
    legend.key = element_rect(fill = NA),
    plot.background = element_rect(fill = "white", color = "white"), panel.background = element_rect(fill = "white"), legend.background = element_rect(fill = "white"), legend.position = "right",legend.box = "vertical")
}

d <- read_csv("data/fake3.csv") %>% 
  mutate(
    id = factor(id),
    id= fct_reorder(id, order) %>% fct_rev()
  )

d2 <- read_csv("data/fake4.csv") %>% 
  mutate(
    id = factor(id),
    id= fct_reorder(id, order) %>% fct_rev(),
    time = time - 6
  )

d11 <- d %>% filter(!event %in% c("Start", "Fert"))
p1 <- d %>% 
  ggplot(aes(time, id, group = id)) +
  geom_vline(aes(xintercept = 6), color = "gray50") +
  geom_line(aes(color = risk), size = 1.5) +
  geom_point(data = d11, aes(shape = event), size = 4) +
  scale_x_continuous(expand = c(0,0), breaks = c(0, 6, 10, 20, 30)) +
  coord_cartesian(xlim = c(0, 31)) +
  scale_color_manual(values = c("gray80", "red")) +
  labs(
    x = "t = Years since start of relationship", 
    y = "ID", 
    color = "Episode state", 
    shape = "Event type", 
    title = "A. Individual life episodes under risk of joining an association"
  ) +
  tf_theme()

d22 <- d2 %>% filter(!event %in% c("Start", "Fert"))
p2 <- d2 %>% 
  ggplot(aes(time, id, group = id)) +
  geom_vline(aes(xintercept = 0), color = "gray50") +
  geom_line(aes(color = risk), size = 1.5) +
  geom_point(data = d22, aes(shape = event), size = 4) +
  scale_x_continuous(expand = c(0,0), breaks = c(-6, 0, 10, 20)) +
  coord_cartesian(xlim = c(-6, 25)) +
  scale_color_manual(values = c("gray80", "red", "darkgreen")) +
  labs(
    x = "t = Years since matching", 
    y = "ID", 
    color = "Episode state", 
    shape = "Event type", 
    title = "B. Individual life episodes under risk of having a child after matching at t(6)"
  ) +
  tf_theme()

# grid.arrange(p1,p2,nrow=2)
gr <- marrangeGrob(list(p1,p2), nrow = 2, ncol = 1, top = "")

h = 5
w = 8
ggsave("figures/psm-lifecourse.png", gr, width = w, height = h)
ggsave("figures/psm-lifecourse.pdf", gr, width = w, height = h)


# d <- read_csv("data/fake.csv") %>% 
#   mutate(
#     id = factor(id),
#     id= fct_reorder(id, order) %>% fct_rev()
#   )
# 
# 
# p1 <- ggplot(d, aes(time, id, group = id)) +
#   geom_line(aes(color = group)) +
#   geom_point(aes(shape = event), size = 4) +
#   labs(x = "t", y = "ID", shape = "Event type", color = "Sample",title = "Years (t) from start to end of observation for 5 individuals") +
#   tf_theme()
# 
# 
# p2 <- d %>% 
#   mutate(status = ifelse(id == "o1", "Potential", ifelse(id == "t1", "Joiner", "Not potential"))) %>% 
#   ggplot(aes(time, id, group = id)) +
#   geom_vline(aes(xintercept = 20), color = "gray") +
#   geom_line(aes(color = status)) +
#   geom_point(aes(shape = event), size = 4) +
#   scale_color_manual(values = c("red", "gray5", "blue")) +
#   labs(x = "t", y = "ID", color = "Control status", shape = "Event type", title = "Control status of indivuals for joiner T1") +
#   tf_theme()
# 
# p3 <- d %>% 
#   mutate(status = ifelse(id == "o3", "Not potential", ifelse(id == "t2", "Joiner", "Potential"))) %>% 
#   ggplot(aes(time, id, group = id)) +
#   geom_vline(aes(xintercept = 13), color = "gray") +
#   geom_line(aes(color = status)) +
#   geom_point(aes(shape = event), size = 4) +
#   scale_color_manual(values = c("red", "gray5", "blue")) +
#   labs(x = "t", y = "ID", color = "Control status", shape = "Event type", title = "Control status of indivuals for joiner T2") +
#   tf_theme()



# second phase

# 
# d2 <- read_csv("data/fake2.csv") %>% 
#   mutate(
#     id = factor(id)
#   )
# 
# ggplot(d2, aes(time, paste(id, group), group = paste(id, group))) +
#   geom_line(aes(color = group)) +
#   geom_point(aes(shape = event), size = 4) +
#   labs(
#     x = "t", y = "ID", 
#     shape = "Event type", color = NULL,
#     title = "Years (t) from start to births to end of observation for matched sample") +
#   tf_theme()
