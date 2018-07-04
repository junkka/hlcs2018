



plot_haz <- function(dd, lims = c(-3,3)){
	d <- dd %>% 
		mutate(
			year = round(period),
			x = n_haz10 %>% log()
		) %>% 
		filter(!is.na(x), !is.infinite(x), x < 6)
	
	d3 <- d %>% 
		group_by(year) %>% 
		summarise(mean = mean(x), sd = sd(x), min = min(x), max = max(x)) %>% 
		ungroup() %>% 
		filter(year < 1951)
	
	mean1950 <- d3$mean[d3$year == 1950]
	sd11950 <- mean1950 + d3$sd[d3$year == 1950]
	sd21950 <- mean1950 + (d3$sd[d3$year == 1950]*2)
	min1950 <- d3$min[d3$year == 1950]
	max1950 <- d3$max[d3$year == 1950]
	
	text_dat <- data_frame(
		x = rep(1950.6, 4),
		y = c(mean1950, sd11950, sd21950, max1950 - 0.2),
		label = c("Mean", "+1 SD", "+2 SD", "Maximum")
	)
	
	ggplot(d3, aes(year, mean, group = 1)) + 
		geom_hline(aes(yintercept = 0), color = "gray") +
		geom_line() + 
		geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2, color = NA) +
		geom_ribbon(aes(ymin = mean - (sd*2), ymax = mean + (sd*2)), alpha = 0.2, color = NA) +
		geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.2, color = NA) +
		xlim(1850,1955) +
		geom_text(data = text_dat, aes(x, y, label = label), hjust = 0) +
		coord_cartesian(ylim = lims) +
		labs(
			x = "Year", 
			y = "Hazard ratio") +
		theme_bw()
}

plot_10 <- plot_haz(n_haz, c(-2,2))
plot_10
