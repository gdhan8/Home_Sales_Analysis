

library(dplyr)

area = 1842
rentZestimate = 2700
beds = 3
baths = 3.5
lotSize = 1498
zipcode = 22025

DumfriesVaZillowOct9 <- read.csv("~/Documents/Data Science/Data/DumfriesVaZillowOct9.csv")

homes <- DumfriesVaZillowOct9[0:26]

home_price_estimator = function(data, area, rentZestimate, beds, baths, lotSize, zipcode){

homes2 <- data %>% filter(statusText == 'Sold') %>% filter(lotSize <= 3500) %>% filter(addressZipcode == zipcode)

df = data.frame(homes2$price, homes2$area, homes2$rentZestimate, homes2$beds, homes2$baths, homes2$lotSize)
df = na.omit(df)

mod = lm(df$homes2.price~. ,data=df)

new = data.frame(homes2.area = area, homes2.rentZestimate = rentZestimate, homes2.beds = beds, homes2.baths = baths, homes2.lotSize = lotSize)
predict(mod, newdata = new)[1]
}

home_price_estimator(data = homes,
                     area = 1842,
                     rentZestimate = 2700,
                     beds = 3,
                     baths = 3.5,
                     lotSize = 1498,
                     zipcode = 22025)


