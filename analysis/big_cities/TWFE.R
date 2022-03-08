# здесь TWFE -> лучше потом в отдельном скрипте и с декомпозицией
# взято из домашки про panel matching
fm_task1 <- cpi_w ~ SMSA_central + AFQT2 + self_conf + education + years + woman + black + hispanic + 
  fam_size + married + union + promotion + risk


OLS <- lm(fm_task1, wage_gap)
ind_fe <- plm(fm_task1, wage_gap, index = c("n","t"), model="within", effect="individual")
twoways_fe <- plm(fm_task1, wage_gap, index = c("n","t"), model="within", effect="twoways")
random_eff <- plm(fm_task1, wage_gap, index = c("n","t"), model="random")


stargazer(OLS, ind_fe, twoways_fe, random_eff, type = "text")