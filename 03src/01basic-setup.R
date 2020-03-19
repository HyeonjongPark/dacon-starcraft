
library(data.table)
library(tidyverse)
library(foreach)
library(doParallel)
library(purrr)
library(patchwork)

getwd()

train = fread("./01original-data/train.csv")
test = fread("./01original-data/test.csv")
sub = fread("./01original-data/sample_submission.csv")

train %>% filter(game_id == 0)

train$win = ifelse(train$winner == train$player, 1, 0)

win_train = train %>% filter(win == 1)
lose_train = train %>% filter(win == 0)

win_train %>% head

par(mfrow= c(1,2))
table(win_train$event) %>% plot() 
table(lose_train$event) %>% plot()
# 컨트롤 횟수와 우승과의 상관관계가 있어 보임.
###




###
sample0_0 = train %>% filter(game_id == 0 & player == 0)
sample0_1 = train %>% filter(game_id == 0 & player == 1)
sample1_0 = train %>% filter(game_id == 1 & player == 0)
sample1_1 = train %>% filter(game_id == 3 & player == 1)

train %>% filter(game_id == 0 & player == 1) %>% group_by(event) %>% summarise(count = n())

tt1 = sample0_0 %>% group_by(event) %>% summarise(count = n())
tt2 = sample0_1 %>% group_by(event) %>% summarise(count = n()) 
tt1
event_df = data.frame(Ability = NA, AddToControlGroup = NA, Camera = NA, GetControlGroup = NA, RightClick = NA, Selection = NA, SetControlGroup = NA)

event_df
event_df[1,1] = tt1$count[tt1$event == "Ability"]
event_df[1,2] = tt1$count[tt1$event == "AddToControlGroup"]
event_df[1,3] = tt1$count[tt1$event == "Camera"]
event_df[1,4] = tt1$count[tt1$event == "GetControlGroup"]
event_df[1,5] = tt1$count[tt1$event == "Right Click"]
event_df[1,6] = tt1$count[tt1$event == "Selection"]
event_df[1,7] = tt1$count[tt1$event == "SetControlGroup"]

event_df
event_df[2,1] = tt2$count[tt2$event == "Ability"]
event_df[2,2] = tt2$count[tt2$event == "AddToControlGroup"]
event_df[2,3] = tt2$count[tt2$event == "Camera"]
event_df[2,4] = tt2$count[tt2$event == "GetControlGroup"]
event_df[2,5] = tt2$count[tt2$event == "Right Click"]
event_df[2,6] = tt2$count[tt2$event == "Selection"]
event_df[2,7] = tt2$count[tt2$event == "SetControlGroup"]

if("Ability" %in% tt2$event) {
  event_df[2,1] = tt2$count[tt2$event == "Ability"]
}
if("AddToControlGroup" %in% tt2$event) {
  event_df[2,2] = tt2$count[tt2$event == "AddToControlGroup"]
}
if("Camera" %in% tt2$event) {
  event_df[2,3] = tt2$count[tt2$event == "Camera"]
}
if("GetControlGroup" %in% tt2$event) {
  event_df[2,4] = tt2$count[tt2$event == "GetControlGroup"]
}
if("Right Click" %in% tt2$event) {
  event_df[2,5] = tt2$count[tt2$event == "Right Click"]
}
if("Selection" %in% tt2$event) {
  event_df[2,6] = tt2$count[tt2$event == "Selection"]
}
if("SetControlGroup" %in% tt2$event) {
  event_df[2,7] = tt2$count[tt2$event == "SetControlGroup"]
}


event_df = data.frame(Ability = NA, AddToControlGroup = NA, Camera = NA, GetControlGroup = NA, RightClick = NA, Selection = NA, SetControlGroup = NA)

event_df 
train = as.data.frame(train)

#(n_distinct(train$game_id)-1)
for(i in 0:3) {
  for(j in 0:1) {
    sel = train %>% filter(game_id == i & player == j)
    
    sel1 = sel %>% group_by(event) %>% summarise(count = n())
    
    
    if("Ability" %in% sel1$event) {
      event_df[(i+1),1] = sel1$count[sel1$event == "Ability"]
    }
    if("AddToControlGroup" %in% sel1$event) {
      event_df[(i+1),2] = sel1$count[sel1$event == "AddToControlGroup"]
    }
    if("Camera" %in% sel1$event) {
      event_df[(i+1),3] = sel1$count[sel1$event == "Camera"]
    }
    if("GetControlGroup" %in% sel1$event) {
      event_df[(i+1),4] = sel1$count[sel1$event == "GetControlGroup"]
    }
    if("Right Click" %in% sel1$event) {
      event_df[(i+1),5] = sel1$count[sel1$event == "Right Click"]
    }
    if("Selection" %in% sel1$event) {
      event_df[(i+1),6] = sel1$count[sel1$event == "Selection"]
    }
    if("SetControlGroup" %in% sel1$event) {
      event_df[(i+1),7] = sel1$count[sel1$event == "SetControlGroup"]
    }
  }
  print(i)
}

event_df




t1 = table(sample0_0$event) %>% as.matrix()
t2 = table(sample0_1$event) %>% as.matrix()
cbind(t1,t2)

table(sample1_0$event)
table(sample1_1$event)


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

foreach(i = 0:(n_distinct(train$game_id)-1)) %dopar% {
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
  if(i%%5 == 0){
    print(attack_vec)
    print(build_vec)
    print(upgrade_vec)
    print(species_vec)
    print(win_vec)
    
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








