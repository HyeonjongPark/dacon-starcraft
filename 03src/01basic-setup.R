library(data.table)
library(tidyverse)
library(foreach)
library(doParallel)
library(purrr)

getwd()

train = fread("./01original-data/train.csv")
test = fread("./01original-data/test.csv")
sub = fread("./01original-data/sample_submission.csv")

train$win = ifelse(train$winner == train$player, 1, 0)

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










library(data.table)
library(tidyverse)
library(foreach)
library(doParallel)
library(purrr)



train = fread("./01original-data/train.csv")

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

foreach(i = 4001:8000) %dopar% {
  foreach(j = c(0,1)) %dopar% {
    sel = train %>% filter(game_id == i & player == j)
    a = grep("Attack", sel$event_contents) %>% length()
    b = grep("Build", sel$event_contents) %>% length()
    p = grep("Patrol", sel$event_contents) %>% length()
    u = grep("Upgrade", sel$event_contents) %>% length()
    s = sel %>% select(species)
    s = s[1,]
    w = sel %>% select(win)
    w = w[1,]
    
    attack_vec = c(attack_vec, a)
    build_vec = c(build_vec, b)
    patrol_vec = c(patrol_vec, p)
    upgrade_vec = c(upgrade_vec, u)
    species_vec = c(species_vec, s)
    win_vec = c(win_vec, w)
  }
  print(i)
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






































train$attack = grepl("Attack", train$event_contents)

train = ifelse(train$attack == FALSE, train$attack = 0, train$attack = 1)


new = data.frame(game_id = NA, player = NA, Species = NA, Ability = NA, AddToControlGroup, Camera,
                 GetControlGroup, RightClick, Selection, SetControlGroup)
new

n_dist


#(n_distinct(train$game_id)-1)
new_df = data.frame()





event = data.frame()
colnames(event) = c("Ability", "AddToControlGroup", "Camera", "GetControlGroup", "Right Click", "Selection", "SetControlGroup")
event

a = train %>% filter(game_id == 0 & player == 0) %>% select(event) %>% table() %>% as.matrix() %>% t() 
a = a[,c("Ability", "AddToControlGroup", "Camera", "GetControlGroup", "Right Click", "Selection", "SetControlGroup")]
b = train %>% filter(game_id == 0 & player == 1) %>% select(event) %>% table() %>% as.matrix() %>% t()
b = b[,c("Ability", "AddToControlGroup", "Camera", "GetControlGroup", "Right Click", "Selection", "SetControlGroup")]
train %>% filter(game_id == 1 & player == 0) %>% select(event) %>% table() %>% as.matrix() %>% t()
train %>% filter(game_id == 1 & player == 1) %>% select(event) %>% table() %>% as.matrix() %>% t()








