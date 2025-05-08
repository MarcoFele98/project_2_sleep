library("dagitty")
library("ggdag")

dag <- dagify(response ~ 
                # individual
                dominance + id + 
                # group
                avg_dominance_rest + n_resting +
                # confounders
                external
                ,
              # individual
              dominance ~ id,
              # group
              avg_dominance_rest ~ close_resting,
              n_resting ~ group_configuration,
              close_resting ~ group_configuration,
              # confounders,
              n_resting ~ external,
              latent = c("external", "close_resting", "group_configuration")
              )
ggdag(dag, text_col = "red")
# Check what I should condition on to find the effect of dominance on response
# individual
adjustmentSets(dag, exposure = "dominance", outcome = "response")
# group
adjustmentSets(dag, exposure = "n_resting", outcome = "response")
adjustmentSets(dag, exposure = "close_resting", outcome = "response")

# instrumental variables
dag <- dagify(response ~ n_resting + close_resting + id + dominance,
              n_resting ~ group_configuration,
              close_resting ~ group_configuration + spatial_positions,
              avg_dominance_resting ~ close_resting,
              dominance ~ id,
              latent = c("close_resting", "group_configuration", "spatial_positions"))
ggdag(dag, text_col = "red")
instrumentalVariables(dag, 
                      exposure = "close_resting", 
                      outcome = "response")

# Testable implications: if the causal model is correct, the conditional independencies have to be seen in the data (by having coefficents != 0)
impliedConditionalIndependencies(dag)


# Extra: check if there is a causal path between two variables
dseparated(dag, X = "dominance", Y = "response")
dseparated(dag, X = "close_resting", Y = "response")

