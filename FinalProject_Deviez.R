library(tidyverse)
library(memisc)
library(pander)
library(Hmisc)
library(writexl)

hp <- read_csv("R1housingprices.csv")

describe(hp[,c("SalePrice", "LotArea", "GrLivArea", "YearBuilt", "OverallCond")])

hp %>% count(BldgType)

hpoutliers <- hp %>% 
  filter(LotArea < 17500, SalePrice < 350000, GrLivArea < 3000, YearBuilt > 1907, between(OverallCond,4,7))

write_xlsx(hpoutliers,"hpoutliers.xlsx")

ggplot(data = hpoutliers) + 
  geom_bar(mapping = aes(x = YearBuilt), binwidth = 0.6) +
  labs(x = "Original Construction Date")

ggplot(data = hpoutliers) + 
  geom_bar(mapping = aes(x = BldgType), binwidth = 0.5) +
  labs(x = "Type of Dwelling")
#      1Fam	Single-family Detached	
#       2FmCon	Two-family Conversion; originally built as one-family dwelling
#       Duplx	Duplex
#       TwnhsE	Townhouse End Unit
#       TwnhsI	Townhouse Inside Unit

ggplot(data = hpoutliers) + 
  geom_bar(mapping = aes(x = OverallCond), binwidth = 0.5) +
  labs(x = "Overall Condition of The House")

ggplot(data = hpoutliers) + 
  geom_point(mapping = aes(x = GrLivArea, y = LotArea)) +
  geom_smooth(mapping = aes(x = GrLivArea, y = LotArea)) +
  labs(x = "Above Grade Living Area (sqft)", y = "Lot Size (sqft)")

ggplot(data = hpoutliers) + 
  geom_bar(mapping = aes(x = KitchenQual), binwidth = 0.6) +
  labs(x = "Kitchen Quality")
#       Ex	Excellent
#       Gd	Good
#       TA Typical/Average
#       Fa	Fair
#       Po	Poor

ggplot(data = hpoutliers, mapping = aes(x = YearBuilt, y = SalePrice)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(labels = function(y) format(y, scientific = FALSE)) +
  labs(title = "Sale Price Based on Construction Date", x = "Original Construction Date", y = "House Sale Price ($)")

ggplot(data = hpoutliers, mapping = aes(x = LotArea, y = SalePrice)) +
  geom_point(mapping = aes(color = BldgType)) +
  geom_smooth() +
  scale_y_continuous(labels = function(y) format(y, scientific = FALSE)) +
  labs(title = "Sale Price Based on Lot Size",x = "Lot Size (sqft)", y = "House Sale Price ($)" )
#      1Fam	Single-family Detached	
#       2FmCon	Two-family Conversion; originally built as one-family dwelling
#       Duplx	Duplex
#       TwnhsE	Townhouse End Unit
#       TwnhsI	Townhouse Inside Unit

ggplot(data = hpoutliers, mapping = aes(x = GrLivArea, y = SalePrice)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~OverallCond, ncol = 3) +
  scale_y_continuous(labels = function(y) format(y, scientific = FALSE)) +
  labs(title = "Sale price Based on Above Grade Living Area per Overall Condition Grade of the House", x = "Above Grade Living Area (sqft)", y = "House Sale Price ($)")

model1 <- lm( formula = SalePrice ~ YearBuilt + OverallCond + BldgType, data = hpoutliers)
model2 <- lm( formula = SalePrice ~ YearBuilt + LotArea + OverallCond, data = hpoutliers)
model3 <- lm( formula = SalePrice ~ YearBuilt + OverallCond + LotArea + GrLivArea, data = hpoutliers)


require(memisc)
m123table <- mtable('Model1' = model1,
                    'Model 2' = model2,
                    'Model 3' = model3,
                    summary.stats = c('R-Squared','F','p','N'))
pander(m123table)

summary(model3)

par(mfrow=c(2,2))
plot(model1, which = c(1,2,3,4))

par(mfrow=c(2,2))
plot(model2, which = c(1,2,3,4))

par(mfrow=c(2,2))
plot(model3, which = c(1,2,3,4))