
library(data.table)
library(tidyverse)
library(foreach)
library(doParallel)
library(purrr)
library(patchwork)

test = fread("./01original-data/test.csv")

mat = matrix(0, nrow = (38872*2), ncol = 15)

colnames(mat) = c("game_id", "player", "species", 
                  "ability", "addToG", "camera", "getToG", "rightC", "selection", "setToG", 
                  "attack", "build", "patrol", "upgrade", 
                  "win")

mat = as.data.frame(mat)

game_id_vec = vector()
for(i in 1:38872) {
  g = rep(i,2)
  game_id_vec = c(game_id_vec, g) 
}

mat$game_id = game_id_vec

mat$player = rep(c(0,1),38872) 






## 병렬 컴퓨팅
n_core = detectCores()
cl = makeCluster(10)
cl

attack_vec = vector()
build_vec = vector()
patrol_vec = vector()
upgrade_vec = vector()
species_vec = vector()
win_vec = vector()

#(n_distinct(train$game_id)-1)

test
foreach(i = 0:(n_distinct(test$game_id)-1)) %dopar% {
  foreach(j = c(0,1)) %dopar% {
    sel = test %>% filter(game_id == (i+38872) & player == j)
    a = grep("Attack", sel$event_contents) %>% length()
    b = grep("Build", sel$event_contents) %>% length()
    p = grep("Patrol", sel$event_contents) %>% length()
    u = grep("Upgrade", sel$event_contents) %>% length()
    s = sel %>% select(species)
    s = s[1,]
    
    attack_vec = c(attack_vec, a)
    build_vec = c(build_vec, b)
    patrol_vec = c(patrol_vec, p)
    upgrade_vec = c(upgrade_vec, u)
    species_vec = c(species_vec, s)

  }
  print(i)
  if(i%%5 == 0){
    print(attack_vec)
    print(build_vec)
    print(upgrade_vec)
    print(species_vec)
    
  }
}






mat$attack = attack_vec
mat$build = build_vec
mat$patrol = patrol_vec
mat$upgrade = upgrade_vec
mat$species = species_vec
mat$win = win_vec

stopCluster(cl)



tt = train %>% filter(game_id == 0 & player == 0) %>% select(species) 
tt[1,]


train



































