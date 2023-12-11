##########################################################################
# MOJA Vegetation Analysis Project #######################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 2: LinePoint Analyses ###########################################
# -----------------------------------------------------------------------------

# PART 0: Import Line Point Intercept Data ####

LinePoint <- read_csv("data/processed/LinePoint.csv") %>% 
  mutate(distance_rescaled = distance/100)

# PART 1: Percent Cover of Litter ---------------------------------------------

# Question: Does the percent cover of leaf litter vary with distance to well (water source)?

#First, take a look at the distribution of the data and check for normality
ggplot(LinePoint, aes(x=litter_cover))+
  geom_histogram()
shapiro.test(LinePoint$litter_cover) #Yes, it is just within the threshold of normality


#Create a linear mixed effects model (normal distribution)
f1 <- lme(litter_cover ~ distance, 
          random = ~1|well/point, #Random effects of well, point (nested)
          data = LinePoint)
summary(f1)

#Try with poisson
fx <- glmer(litter_cover ~ distance_rescaled +
              (1|well/point), #Random effects of well, point)
            family = poisson(link = "log"),
            data = LinePoint)

summary(fx)


# Check f3 assumptions with DHARMa package
fx_res = simulateResiduals(fx, n=1000)
plot(fx_res, rank = T) 
testDispersion(fx_res)
plotResiduals(fx, form = LinePoint$distance, main=NULL)



#Check assumptions
plot(fx)
qqnorm(resid(fx))
qqnorm(fx, ~ranef(., level=1))
qqnorm(f1, ~ranef(., level=2))

# PART 2: Percent Cover of woody plants ----------------------------------------











# PART 3: Percent Cover of B. rubens & Schizmus --------------------------------

#First, take a look at the distribution of the data
ggplot(LinePoint, aes(x=selected_plant_cover))+
  geom_histogram()


#Create model f3. 
f3 <- glmmTMB(selected_plant_cover ~ distance + 
                (1|well/point), #Random effects of well, point, transect (nested)
              ziformula=~1,
              family=nbinom2,
              data = LinePoint)
summary(f3)


# Check f3 assumptions with DHARMa package
f3_res = simulateResiduals(f3, n=1000)
plot(f3_res, rank = T) 
testDispersion(f3_res)
plotResiduals(f3, form = LinePoint$distance, main=NULL)


# PART 4: Percent Cover of all invasive plants --------------------------------

#First, take a look at the distribution of the data and check for normality
ggplot(LinePoint, aes(x=invasive_plant_cover))+
  geom_histogram()


#Create model f4.
f4 <- glmmTMB(invasive_plant_cover ~ distance + 
                (1|well/point), #Random effects of well, point (nested)
              ziformula=~1,
              family=nbinom2, #binomial distribution (0/1) data
              data = LinePoint)
summary(f4)


# Check f4 assumptions with DHARMa package
f4_res = simulateResiduals(f4)
plot(f4_res, rank = T) 
testDispersion(f4_res)
plotResiduals(f4, form = LinePoint$distance, main=NULL)


# PART 5: % cover of native grasses ---------------------------------------









#PART 6: Percent cover of canopy vegetation ------------------------------------

#First, take a look at the distribution of the data
ggplot(LinePoint, aes(x=canopy_cover))+
  geom_histogram()
shapiro.test(LinePoint$canopy_cover) #No, it is not normal

#Create model f6.

f6 <- glmmTMB(canopy_cover ~ distance + 
                (1|well/point), #Random effects of well, point (nested)
              family=nbinom1, #binomial distribution (0/1) data
              data = LinePoint)
summary(f6)

# Check f6 assumptions with DHARMa package
f6_res = simulateResiduals(f6, n=1000)
plot(f6_res, rank = T) 
testDispersion(f6_res)
plotResiduals(f6, form = LinePoint$distance, main=NULL)
#NOTE: DHARMa throws an error that there are may be problems with model fit.  


# PART 7: Total percent cover of vegetation (dead or alive) --------------------

ggplot(LinePoint, aes(x=all_cover))+
  geom_histogram()

ggplot(LinePoint, aes(y=all_cover, x=distance))+
  geom_jitter(width = 5)+
  stat_smooth(method = "glm")


#Create model f7. Need to check distribution assumptions, double check, etc.
f7 <- glmmTMB(all_cover ~ distance, #+ 
#              (1|well/point), #Random effects of well, point, transect (nested)
            family = nbinom2, 
            data = LinePoint)
summary(f7)

# Check f7 assumptions with DHARMa package
f7_res = simulateResiduals(f7, n=1000)
plot(f7_res, rank = T) 
testDispersion(f7_res)
plotResiduals(f7, form = LinePoint$distance, main=NULL)
#NOTE: DHARMa throws an error that there are may be problems with model fit.  
