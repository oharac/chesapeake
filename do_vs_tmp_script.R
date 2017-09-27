ggtheme_plot <- function(base_size = 9) {
  theme(axis.ticks = element_blank(),
        text             = element_text(family = 'Helvetica', color = 'gray30', size = base_size),
        plot.title       = element_text(size = rel(1.25), hjust = 0, face = 'bold'),
        panel.background = element_blank(),
        legend.position  = 'right',
        panel.border     = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = 'grey90', size = .25),
        # panel.grid.major = element_blank(),
        legend.key       = element_rect(colour = NA, fill = NA),
        axis.line        = element_blank()) # element_line(colour = "grey30", size = .5))
}

raw_data <- read_csv('data/water_qual_student.csv')

do_tmp <- raw_data %>% 
  filter(Parameter %in% c('DO', 'WTEMP'))
do_tmp1 <- do_tmp %>% 
  select(place = place_name, fips = FIPS, 
         date = SampleDate, time = SampleTime, 
         param = Parameter, 
         value = MeasureValue, unit = Unit) %>% 
  distinct()

do_tmp2 <- do_tmp1 %>% 
  group_by(place, fips, date, param, unit) %>% 
  summarize(value = mean(value, na.rm = TRUE))

do_tmp3 <- do_tmp2 %>% 
  select(-unit) %>% 
  spread(param, value)
do_tmp4 <- do_tmp3 %>% 
  rename(dissolved_o2 = DO, water_temp = WTEMP)

write_csv(do_tmp4, 'data/do_vs_tmp.csv')

ggplot(do_tmp4, aes(x = date, y = dissolved_o2)) + 
  ggtheme_plot() +
  geom_point(color = 'blue', alpha = .5) +
  labs(title = 'Dissolved Oxygen vs time')

ggplot(do_tmp4, aes(x = date, y = water_temp)) + 
  ggtheme_plot() +
  geom_point(color = 'red', alpha = .5) +
  labs(title = 'Water temp vs time')

ggplot(do_tmp4, aes(x = water_temp, y = dissolved_o2)) + 
  ggtheme_plot() +
  geom_point(color = 'purple4') +
  labs(title = 'Dissolved O2 vs water temp')

