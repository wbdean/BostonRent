# model

library(lme4)
library(splines)


fit <- glm(data = BostonListings, 
           log(rent) ~ ns(closest_T, df = 3))
plot(BostonListings$closest_T, predict(fit, type = 'response'))

fit <- glm(data = BostonListings, 
             log(rent) ~ ns(closest_T, df = degf) + ns(closest_Bar, df = degf) + ns(closest_Groc, df = degf) + 
             ns(closest_food, df = degf) + ns(closest_hist, df = degf) +  ns(closest_land, df = degf) + 
             ns(closest_park, df = degf) + ns(closest_dog, df = degf) + ns(closest_bike, df = degf) + 
             rooms + num_land + num_food + num_Bar + num_T + num_Groc + studio + num_dog +
             neighbourhood_cleansed, 
           family = Gamma(link = 'log'))

fit <- lm(data = BostonListings, 
          log(rent) ~ ns(num_land, df = 1, knots = 20) + num_food + ns(num_Bar, df = 1, knots = 125) + 
            num_T + num_Groc + num_dog + ns(num_hist, df = 1, knots = 13) + num_lux + num_Cof +
            closest_bike + factor(num_pSport) + ns(closest_bc, df = 1, knots = 2) + 
            ns(rooms, df = 1, knots = 2.5) + neighbourhood_cleansed*traffic)

# Find Spline knots 
ggplot(BostonListings, aes(rooms, log(rent))) + geom_point()

T_df <- 1:4; lux_df <- 1:4; Cof_df <- 1:3; food_df <- 1:2;
fit <- lm(data = BostonListings, 
          log(rent) ~ num_T + traffic + 
            num_Groc + num_dog + num_lux + num_Cof + poly(num_hist, degree = 2, raw = TRUE) +
            num_food+ num_Bar + num_land +
            closest_bike * neighbourhood_cleansed + factor(num_bc) + factor(num_pSport) + 
            poly(rooms, degree = 2, raw = TRUE) * neighbourhood_cleansed+ studio)


plot(BostonListings$rooms, log(BostonListings$rent))
points(BostonListings$rooms, predict(fit, type = 'response'), col = 'red')

plot(BostonListings$rent %>% log, predict(fit, type = 'response'))

ggplot(BostonListings, aes(log(rent), predict(fit, type = 'response'))) + geom_point()

AIC(fit)
summary(fit)
plot(fit)

r <- resid(fit, type = 'response')^2
r2 <- r ^ 2

ggplot(BostonListings, aes(num_T, r2)) + geom_point(aes(color = downtown)) 

fit2 <- glm(data = BostonListings, 
            r2 ~ num_T + traffic * neighbourhood_cleansed + 
              num_Groc + num_dog + num_lux + num_Cof + num_hist +
              num_food+ num_Bar + num_land + studio + 
              closest_bike + factor(num_bc) + factor(num_pSport) + poly(rooms, degree = 2, raw = TRUE),
            family = Gamma(link = 'log'))
w = 1 / predict(fit2, type = 'response') 

fit <- lm(data = BostonListings, 
          log(rent) ~ num_T + traffic + 
            num_Groc + num_dog + num_lux + num_Cof + poly(num_hist, degree = 2, raw = TRUE) +
            num_food+ num_Bar + num_land +
            closest_bike * neighbourhood_cleansed + factor(num_bc) + factor(num_pSport) + poly(rooms, degree = 2, raw = TRUE) * neighbourhood_cleansed + studio, 
            weights = w)

plot(BostonListings$traffic, log(BostonListings$rent))
points(BostonListings$traffic, predict(fit, type = 'response'), col = 'red')
plot(BostonListings$rent %>% log, predict(fit, type = 'response'),
     col = BostonListings$downtown)
abline(0, 1)




summary(fit)

AIC(fit)



ggplot(BostonListings, aes(longitude, latitude, color = (r2))) + geom_point()
boston + geom_point(data = BostonListings %>% filter(log(rent) > 8.4), aes(longitude, latitude))

Bad <- BostonListings %>% filter(log(rent) > 8.3)
library(leaflet)
leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(lng = Bad$longitude, lat = Bad$latitude)
leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(lng = apt$longitude, lat = apt$latitude)

