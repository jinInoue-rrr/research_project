library(pacman)

p_load(tidyverse, haven, naniar, ggfocus, ggthemes, broom, ivreg, gt, parameters, DIDmultiplegt, TwoWayFEWeights, wooldridge, panelView, bacondecomp, lmtest, multiwayvcov, plm,lfe,miceadds,estimatr, clubSandwich)

df <- wooldridge::wagepan
df_selected <- df %>% 
  dplyr::select(lwage, nr, year, union, hours)

converter <- function(df){
  dat <- data.frame(df)
  for (i in c(1980,1981,1982,1983,1984,1985)){
    j <- i + 1
    k <- i + 2
    if ((dat[dat$year==i,]$union == 0)&&(dat[dat$year==j,]$union == 1)&&(dat[dat$year==k,]$union == 0)){
      dat[dat$year==j,]$union <- 0
      # print("TRUE")
    }
    else if((dat[dat$year==i,]$union == 1)&&(dat[dat$year==j,]$union == 0)&&(dat[dat$year==k,]$union == 1)){
      dat[dat$year==j,]$union <- 1
      # print("TRUE")
    }
    else{
      next
    }
  }
  return(dat)
}


# 
# df13 <- nested %>% 
#   filter(nr == 13) %>% 
#   unnest()
# df13

# group_byしてnestしてgroup(nr)ごとにconverterを適用してunnest

nested <- df_selected %>% 
  group_by(nr) %>% 
  nest()

mod_df <- nested %>%
  mutate(new = map(.x = data,~converter(df = .x))) %>% 
  unnest() %>% 
  dplyr::select(nr, year, union,union1,hours,lwage)

output_mod <- did_multiplegt(df = mod_df, Y = "lwage", G = "nr", T = "year", D = "union1",placebo = 3,brep = 2,cluster = "nr")
output_mod
