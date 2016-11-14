# Example 1

# Find R^2 for a model


data(CPS85, package = "mosaicData")
mod1 <- lm(wage ~ exper + sector, data = CPS85)
summary(mod1)
