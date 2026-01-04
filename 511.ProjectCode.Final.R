#########
# Setup #
#########
# Set up our environment, clear prior clutter
# update.packages(ask = FALSE)
options(scipen = 999)
rm(list = ls())
graphics.off()
cat("\014")
linebreak <- "------------------------\n"

# Install packages via pacman
pacman::p_load( car, lars, data.table, caret, MASS, broom, leaps,
                lmtest, knitr, tidyverse, BeSS, equatiomatic, here)

#################
# Preprocessing #
#################
getwd()
# setwd("C:/OneDrive/School Files/GRAD FALL/Econometrics/DATA") # Windows
setwd('/Users/andrewholland/Library/CloudStorage/OneDrive-Personal/School Files/GRAD FALL/Econometrics/DATA') # Mac
df.passport <- read.csv('passportcosts.csv')
summary(df.passport)

# factor variables
factor.variables <- c(
  'region',
  'restrict',
  'commonwealth',
  'language')
for (var_name in factor.variables) { # call the variables one at a time
  df.passport[[var_name]] <- factor(df.passport[[var_name]]) # factor each variable in the list
}
summary(df.passport)

# Remove var flowuscanz
# Per project doc, not necessary
df.passport <- df.passport %>% dplyr::select(-flowuscanz)
summary(df.passport)

# Data Frame audit
observations.n <- nrow(df.passport)
variables.total <- length(df.passport)
str(df.passport)

# Lists of variable names by type, stored in environment
numeric.var <- names(df.passport)[sapply(df.passport, is.numeric)]
identifier.var <- names(df.passport)[sapply(df.passport, is.character)]
factor.var <- names(df.passport)[sapply(df.passport, is.factor)]
variable.list <- list(numeric.var = numeric.var,
                      identifier.var = identifier.var,
                      factor.var = factor.var)

variable.list

#########################
# Predictor Exploration #
#########################

# Explore Y Response Variable (cost)
summary(df.passport$cost)

# Histogram Plot
ggplot(df.passport, aes( x = cost)) +
  geom_histogram(bins = 20, fill = 'darkgreen', color = 'white', alpha = 0.7) +
  geom_vline(aes(xintercept = mean(cost)),
             color = 'red', linetype = 'dashed', linewidth = 1) +
  labs(title = "Distribution of Passport Costs",
       subtitle = paste("Mean =", round(mean(df.passport$cost), 2)),
       x = "Cost",
       y = "Frequency") + 
  theme_minimal() + 
  theme(panel.grid = element_blank())

# Outliers and more
sd(df.passport$cost)
boxplot(df.passport$cost, main = "Passport Cost - Boxplot", ylab = "Cost")
qqnorm(df.passport$cost)
qqline(df.passport$cost, col = 'red')

########################
# Correlation Analysis #
########################
# Create the matrix of untransformed variables
cor.mtrx.orig <- cor(df.passport[numeric.var]) 

# Visualize the matrix in a heat map
pacman::p_load(reshape2)
cor.mtrx.melted <- melt(cor.mtrx.orig)
ggplot(cor.mtrx.melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1),
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()) +
  labs(title = "Correlation Matrix: Numerical Variables", x = "", y = "")

# Pull highly correlated values
high.corr.thresh <- 0.7
high.corr.orig <- cor.mtrx.melted %>%
  filter(abs(value) > high.corr.thresh & value != 1) %>%
  arrange(desc(abs(value)))

## Remove duplicate pairs
high.corr.orig <- high.corr.orig %>%
  filter(as.character(Var1) < as.character(Var2))

kable(high.corr.orig, digits = 2)


###########################
# Variable transformation #
###########################
# Remove some highly correlated variables from the list for consideration
# Removing ruleoflaw, regulatory, govteffectiveness
# These variables all measure the same underlying governance qualiy construct
numeric.var <- setdiff(numeric.var, c('ruleoflaw', 'regulatory', 'govteffectiveness'))

# Create transformed variables
df.passport$log_popn2004 <- log(df.passport$popn2004)
df.passport$log_migrants <- log(df.passport$migrants + 1)  # +1 for zeros
df.passport$log_gdp <- log(df.passport$gdp)

# Update numeric vars list
## Remove un-transformed variables
numeric.var <- setdiff(numeric.var, c('popn2004', 'gdp', 'migrants'))

## Add transformed variables
numeric.var <- c(numeric.var, 'log_popn2004', 'log_migrants', 'log_gdp')

numeric.var

## Reset master variable list
variable.list <- list(numeric.var = numeric.var,
                      identifier.var = identifier.var,
                      factor.var = factor.var)

# All variables remain in the data frame for robustness

############################
# Explanatory Scatterplots #
############################
# Create plots for every variable against predictor Y

plot_all_v_predictor <- function(data, predictor.var, vars_to_include = NULL) {
  
  # Get all variable names except the predictor and identifier variables
  if (!is.null(vars_to_include)) {
    vars_to_plot <- setdiff(vars_to_include, predictor.var)
  } else {
    vars_to_plot <- setdiff(names(data), c(predictor.var, identifier.var))
  }
  
  # Check if predictor exists
  if (!predictor.var %in% names(data)) {
    stop("Predictor variable not found in dataset")
  }
  
  # Create plots for each variable
  plots <- lapply(vars_to_plot, function(var) {
    
    # Determine if variable is categorical (factor) or continuous (numeric)
    is_categorical <- is.factor(data[[var]])
    
    if (is_categorical) {
      # Box plot for categorical variables
      p <- ggplot(data, aes(x = .data[[var]], y = .data[[predictor.var]])) +
        geom_boxplot(fill = "darkgreen", alpha = 0.9, color = "darkgreen") +
        labs(title = paste("Distribution of", predictor.var, "by", var),
             x = var, 
             y = predictor.var) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              panel.grid = element_blank(),
              plot.title = element_text(hjust = 0.5))
      
    } else {
      # Scatter plot for continuous variables
      p <- ggplot(data, aes(x = .data[[var]], y = .data[[predictor.var]])) +
        geom_point(alpha = 0.9, color = "black") +
        geom_smooth(method = "lm", color = "red", se = TRUE, linewidth = 1) +
        labs(title = paste(predictor.var, "vs", var),
             x = var, 
             y = predictor.var) +
        theme_minimal() +
        theme(panel.grid = element_blank(),
              plot.title = element_text(hjust = 0.5))
    }
    
    return(p)
  })
  
  # Name the plots
  names(plots) <- vars_to_plot
  
  return(plots)
}

# Generate plots for variables only in the curated list
vars_to_plot <- c(numeric.var, factor.var)
predictor.plots <- plot_all_v_predictor(df.passport, "cost", vars_to_include = vars_to_plot)

# Print all plots sequentially
for (plot_name in names(predictor.plots)) {
  print(predictor.plots[[plot_name]])
}

#####################
# Model Development #
#####################
# Regression Violation Check Function
check_regression_conditions <- function(model, model.name, data) {
  
  cat("\n")
  cat("================================================================\n")
  cat("REGRESSION DIAGNOSTICS FOR:", model.name, "\n")
  cat("================================================================\n")
  cat("Formula:", deparse(formula(model)), "\n")
  cat("R-squared:", round(summary(model)$r.squared, 4), "\n")
  cat("Adj R-squared:", round(summary(model)$adj.r.squared, 4), "\n")
  cat("================================================================\n\n")
  
  model.terms <- attr(terms(model), "term.labels")
  contin.pred <- intersect(model.terms, numeric.var)
  contin.pred <- setdiff(contin.pred, c('cost', 'code'))
  
  ## Linearity
  cat("Linearity Check\n")
  cat(linebreak)
  
  # Extract plot data
  plotdata <- data.frame(
    Fitted = fitted(model),
    Residuals = residuals(model)
  )
  
  # Residuals v Fitted
  print(
    ggplot(plotdata, aes(x = Fitted, y = Residuals)) + 
    geom_point(shape = 16) + 
    geom_hline(yintercept = 0, linetype = 'dashed', color = 'red', linewidth = 0.75) +
    labs (
      title = paste(model.name, '\nResiduals v. Fitted Values'),
      x = "Fitted Values",
      y = "Residuals"
    ) +
    theme_minimal()
  )
  
  # Residuals v each cont. predictor
  if (length(contin.pred) > 0) {
    n_plots <- length(contin.pred)
    n_cols <- min(3, n_plots)
    n_rows <- ceiling(n_plots / n_cols)
    
    par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 2, 1))
    for (var in contin.pred) {
      plot(data[[var]], residuals(model),
           xlab = var, ylab = "Residuals",
           main = paste("Residuals vs", var),
           pch = 16)
      abline(h = 0, col = "red", lty = 2, lwd = 2)
    }
    par(mfrow = c(1, 1))
  }
  
  cat("✓ Check complete. Look for random scatter with no curved patterns.\n\n")
  
  cat(linebreak)
  
  ## Multicollinearity
  cat("Multicollinearity Check\n")
  cat(linebreak)
  
  # Check if model has predictors
  if (length(model.terms) > 0) {
    
    # Correlation martix for continuous predictors in mdoel
    if (length(contin.pred) > 1) {
      cat("\nCorrelation Matrix (continuous predictors in model):\n")
      cor.model <- cor(data[, contin.pred], use = "complete.obs")
      print(round(cor.model, 3))
      
      # Flag high correlations
      high.cor <- which(abs(cor.model) > 0.8 & abs(cor.model) < 1, arr.ind = TRUE)
      if (nrow(high.cor) > 0) {
        cat("\n⚠ HIGH CORRELATIONS (|r| > 0.8) FOUND:\n")
        for (i in 1:nrow(high.cor)) {
          var1 <- rownames(cor.model)[high.cor[i, 1]]
          var2 <- colnames(cor.model)[high.cor[i, 2]]
          if (var1 < var2) {  # Avoid duplicates
            cat(sprintf("  %s <-> %s: %.3f\n", var1, var2, 
                        cor.model[high.cor[i, 1], high.cor[i, 2]]))
          }
        }
      } else {
        cat("\n✓ No high correlations (|r| > 0.8) detected.\n")
      }
    }
    
    # VIF calculation
    cat("\nVariance Inflation Factors (VIF):\n")
    tryCatch({
      vif.values <- vif(model)
      
      # Check if GVIF is present (indicates factor variables)
      if (is.matrix(vif.values)) {
        print(round(vif.values, 3))
        cat("\nInterpretation Guide:\n")
        cat("  For continuous variables (VIF column):\n")
        cat("    4-8 = Some concern\n")
        cat("    9+ = High concern\n")
        cat("  For factor variables (GVIF^(1/(2*Df)) column):\n")
        cat("    2.0-2.9 = Some concern\n")
        cat("    3.0+ = High concern\n\n")
        
        # Flag problematic VIFs
        if ("GVIF" %in% colnames(vif.values)) {
          # Has factors - use adjusted GVIF
          gvif.adj <- vif.values[, "GVIF^(1/(2*Df))"]
          problems <- gvif.adj[gvif.adj >= 2.0]
          if (length(problems) > 0) {
            cat("⚠ Variables with elevated VIF:\n")
            print(round(problems, 3))
          } else {
            cat("✓ All VIF values acceptable.\n")
          }
        }
      } else {
        print(round(vif.values, 3))
        cat("\nInterpretation: 4-8 = Some concern, 9+ = High concern\n")
        problems <- vif.values[vif.values >= 4]
        if (length(problems) > 0) {
          cat("⚠ Variables with elevated VIF:\n")
          print(round(problems, 3))
        } else {
          cat("✓ All VIF values acceptable.\n")
        }
      }
    }, error = function(e) {
      cat("✗ Cannot calculate VIF:", e$message, "\n")
    })
  } else {
    cat("Intercept-only model - no multicollinearity to check.\n")
  }
  
  cat(linebreak)
  
  ## Constant/Non-constant Variance
  cat("Heteroskedasticity Check\n")
  cat(linebreak)
  # Scale-Location plot
  sqrt.std.resid <- sqrt(abs(rstandard(model)))
  plot(fitted(model), sqrt.std.resid,
       xlab = "Fitted Values", 
       ylab = expression(sqrt("|Standardized Residuals|")),
       main = paste(model.name, "\nStudentized vs Fitted Plot"),
       pch = 16)
  lowess.fit <- lowess(fitted(model), sqrt.std.resid)
  
  # Breusch-Pagan test
  if (length(model.terms) > 0) {
    bp.test <- bptest(model)
    cat("\nBreusch-Pagan Test for Heteroskedasticity:\n")
    cat(sprintf("  BP = %.4f, p-value = %.4f\n", bp.test$statistic, bp.test$p.value))
    if (bp.test$p.value < 0.05) {
      cat("  ⚠ Evidence of non-constant variance (p < 0.05)\n")
    } else {
      cat("  ✓ No strong evidence of non-constant variance\n")
    }
  }
  
  cat("Look for: Constant spread across fitted values (no funnel/horn shape)\n\n")
  cat(linebreak)
  
  ## Normally Distributed Errors
  cat("Normally Distributed Errors Check\n")
  cat(linebreak)
  #Q-Q Plot
  qqPlot(rstudent(model), 
         distribution = "t",
         df = df.residual(model) - 1,
         main = paste(model.name, "\nNormal Q-Q Plot"),
         ylab = "Studentized Residuals",
         pch = 16)
  
  cat("Look for: Points falling on the 45-degree line within confidence bands\n\n")
  
  cat(linebreak)
  
  ## Outliers
  cat("Outliers Check\n")
  cat(linebreak)
  # Studentized residuals
  stud.resid <- rstudent(model)
  outlier.threshold <- 3
  regression.outliers <- which(abs(stud.resid) > outlier.threshold)
  
  if (length(regression.outliers) > 0) {
    cat("⚠ REGRESSION OUTLIERS (|studentized residual| > 3):\n")
    for (idx in regression.outliers) {
      cat(sprintf("  Obs %d: %s (studentized residual = %.3f)\n\n", 
                  idx, 
                  data$country_name[idx],
                  stud.resid[idx]))
    }
  } else {
    cat("✓ No regression outliers detected (|studentized residual| > 3)\n\n")
  }
  cat("================================================================\n")
  cat("END OF DIAGNOSTICS FOR:", model.name, "\n")
  cat("================================================================\n\n")
  
}

#####################
# Create a full and intercept model based on the remaining variables
variable.list

model.full <- lm(cost ~ log_gdp + log_popn2004 + log_migrants + migprop
                 + voice + politicalstability + corruption + region + restrict
                 + commonwealth + language, data = df.passport)
model.int <- lm(cost ~ 1, data = df.passport)

# Create the models.check list and add the full model to it
models.check <- list(
  "Full Model" = model.full
)

# Run the regression checks function
for (model.name in names(models.check)) {
  check_regression_conditions(models.check[[model.name]],
                              model.name, df.passport)
}

# Comparison Table based on the check function
comparison.table <- data.frame(
  N_Predictors = sapply(models.check, function(m) length(attr(terms(m), "term.labels"))),
  R_Squared = sapply(models.check, function(m) summary(m)$r.squared),
  Adj_R_Squared = sapply(models.check, function(m) summary(m)$adj.r.squared),
  AIC = sapply(models.check, AIC),
  BIC = sapply(models.check, BIC),
  RMSE = sapply(models.check, function(m) sqrt(mean(residuals(m)^2)))
)
kable(comparison.table, digits = 4)

# Corruption continues to create collinearity issues, dropped from the variable list
# from here on out
numeric.var <- setdiff(numeric.var, c('corruption'))

# Funnel shape -> heteroskedasticity in the residuals/fitted plot
# Transform y -> log(y) for a log-log model
# Armenia (2004-5) had zero passport cost due to 1:1 replacement of Soviet passports
# This is a qualitatively different policy (free replacement program) vs. low-cost passports
# Excluding this observation for log-log model (n = [original n] - 1 = [new n])
df.passport.analysis <- df.passport[df.passport$cost > 0, ]
df.passport.analysis$log_cost <- log(df.passport.analysis$cost)

# Preference towards the asymptotically consistent BIC
BIC.passport <- log(nrow(df.passport.analysis))

#####################
# Create some step-wise selection models
# Create the fixed full and intercept models
model.full <- lm(log_cost ~ log_gdp + log_popn2004 + log_migrants + migprop
                 + voice + politicalstability + region + restrict
                 + commonwealth + language, data = df.passport.analysis)
model.int <- lm(log_cost ~ 1, data = df.passport.analysis)

# Forward
step.forward <- step(model.int,
                     formula(model.full),
                     direction = 'forward',
                     k = BIC.passport
)
summary(step.forward)
Anova(step.forward, type = 'III')

# Backward
step.backward <- step(model.full,
                      formula(model.full),
                      direction = 'backward',
                      k = BIC.passport
)
summary(step.backward)
Anova(step.backward, type = 'III')

# Bidirectional
step.both <- step(model.int,
                  formula(model.full),
                  direction = 'both',
                  k = BIC.passport
)
summary(step.both)
Anova(step.both, type = 'III')

# Run the regression validation check function on the stepwise models
models.check <- list("Backward Selection" = step.backward)
for (model.name in names(models.check)) {
  check_regression_conditions(models.check[[model.name]],
                              model.name, df.passport.analysis)
}

# Both the forward and bidirectional selected models have significant issues
# from only having 1 variable in their models, thus are not included in the
# further diagnostic analysis

# Comparison Table based on the check function
comparison.table <- data.frame(
  N_Predictors = sapply(models.check, function(m) length(attr(terms(m), "term.labels"))),
  R_Squared = sapply(models.check, function(m) summary(m)$r.squared),
  Adj_R_Squared = sapply(models.check, function(m) summary(m)$adj.r.squared),
  AIC = sapply(models.check, AIC),
  BIC = sapply(models.check, BIC),
  RMSE = sapply(models.check, function(m) sqrt(mean(residuals(m)^2)))
)
kable(comparison.table, digits = 4)

#####################
# Create a BESS model
# Create a dataset for BeSS to work in
df.passport.bess <- df.passport.analysis[, -1]
df.passport.bess <- df.passport.bess[, -1]

# Remove the variables excluded from var lists
df.passport.bess <- df.passport.bess %>% dplyr::select(-cost, -corruption,
                                                       -ruleoflaw, -regulatory,
                                                       -govteffectiveness, -gdp,
                                                       -popn2004, -migrants)
# Fit the variable space and BeSS model
x.bess <- model.matrix(log_cost ~ ., data = df.passport.bess)[, -1] # remove intercept
y.bess <- df.passport.analysis$log_cost
model.bess <- bess(x.bess, y.bess, family = 'gaussian')

# Pull the best BeSS model
model.best.bess <- model.bess$bestmodel
summary(model.best.bess)

# Run model diagnostic test on the BeSS
# WARNING: REQUIRES MANUAL EXAMINATION SINCE THE FUNCTION CANNOT PULL 
# THE VARIABLES IN THE SAME WAY IT CAN FOR OTHER MODELS
# DISREGARD vif() FOR THIS MODEL
models.check <- list("BeSS Model" = model.best.bess)
for (model.name in names(models.check)) {
  check_regression_conditions(models.check[[model.name]],
                              model.name, df.passport.analysis)
}

#####################
# Develop models based on logic, intuition, and economic/political theory
# Iteratively refined based on economic theory and diagnostics
model.theory <- lm(log_cost ~ log_gdp # The GDP of a country should influence costs
                   + log_popn2004 # The population should influence relative costs
                   + region # Regional variation in administrative costs
                   + commonwealth # British institutions should influence costs
                   + language # English speaking countries have higher access to western Institutions
                   , data = df.passport.analysis)

model.theory2 <- lm(log_cost ~ log_gdp # The GDP of a country should influence costs
                    + log_popn2004 # The population should influence relative costs
                    + log_migrants # Migrant influx likely increases costs, especially in a post 9/11 world
                    + region # Regional variation in administrative costs
                    + commonwealth # British institutions should influence costs
                    + language # English speaking countries have higher access to western Institutions
                    + politicalstability # A valuable proxy score for citizens' ability to get passports
                    , data = df.passport.analysis)

model.theory3 <- lm(log_cost ~ log_gdp # The GDP of a country should influence costs
                    + log_popn2004 # The population should influence relative costs
                    + log_migrants # Migrant influx likely increases costs, especially in a post 9/11 world
                    + region # Regional variation in administrative costs
                    + voice # Replace commonwealth with voice for more country-by-country impacts, regardless of Empire
                    + politicalstability # A valuable proxy score for citizens' ability to get passports
                    , data = df.passport.analysis)

model.theory4 <- lm(log_cost ~ log_gdp # The GDP of a country should influence costs
                    + log_popn2004 # The population should influence relative costs
                    + log_migrants # Migrant influx likely increases costs, especially in a post 9/11 world
                    + region # Regional variation in administrative costs
                    + voice 
                    + commonwealth # reintroduce commonwealth for possible strength?
                    + politicalstability # A valuable proxy score for citizens' ability to get passports
                    , data = df.passport.analysis)
model.theory5 <- lm(log_cost ~ log_gdp # The GDP of a country should influence costs
                    + region # Regional variation in administrative costs
                    + voice # Replace commonwealth with voice for more country-by-country impacts, regardless of Empire
                    + politicalstability # A valuable proxy score for citizens' ability to get passports
                    , data = df.passport.analysis)

models.check <- list(
  "Theory Model 1" = model.theory,
  "Theory Model 2" = model.theory2,
  "Theory Model 3" = model.theory3,
  "Theory Model 4" = model.theory4
)
for (model.name in names(models.check)) {
  check_regression_conditions(models.check[[model.name]],
                              model.name, df.passport.analysis)
}
comparison.table <- data.frame(
  N_Predictors = sapply(models.check, function(m) length(attr(terms(m), "term.labels"))),
  R_Squared = sapply(models.check, function(m) summary(m)$r.squared),
  Adj_R_Squared = sapply(models.check, function(m) summary(m)$adj.r.squared),
  AIC = sapply(models.check, AIC),
  BIC = sapply(models.check, BIC),
  RMSE = sapply(models.check, function(m) sqrt(mean(residuals(m)^2)))
)
kable(comparison.table, digits = 3)

# Group consensus: 
# Theory Model 3 is the best by BIC and regression condition satisfaction

########################
# Compare Final Models #
########################
# Check the best of each model group against regression violations one last time and performance
models.check <- list(
  "Backward Model" = step.backward,
  "BeSS Model" = model.best.bess,
  "Theory Model 3" = model.theory3
)
for (model.name in names(models.check)) {
  check_regression_conditions(models.check[[model.name]],
                              model.name, df.passport.analysis)
}
comparison.table <- data.frame(
  N_Predictors = sapply(models.check, function(m) length(attr(terms(m), "term.labels"))),
  R_Squared = sapply(models.check, function(m) summary(m)$r.squared),
  Adj_R_Squared = sapply(models.check, function(m) summary(m)$adj.r.squared),
  AIC = sapply(models.check, AIC),
  BIC = sapply(models.check, BIC),
  RMSE = sapply(models.check, function(m) sqrt(mean(residuals(m)^2)))
)
kable(comparison.table, digits = 4)

# While the BeSS model may be the most robust statisically,
# Theory model 3 holds sway in both satisfying regression consitions and maintains
# important explainability
# Theory model 3 also has several independent variables of case importance,
# Something the backwards selection algorithm did without. 

###########################
# Selected Model Equation #
###########################
model.select <- model.theory3
summary(model.select)
eq <- as.character(extract_eq(model.select, use_coefs = TRUE, wrap = FALSE))
eq_clean <- gsub("\\\\operatorname\\{([^}]+)\\}", "\\1", eq)  # Remove \operatorname
eq_clean <- gsub("\\$\\$|\\n", "", eq_clean)  # Remove $$ and newlines
eq_clean <- trimws(eq_clean)
cat(eq_clean)




#################
# Var Selection #
#################
model.region <- lm(log_cost ~ region, data = df.passport.analysis)
summary(model.region)

model.gdp <- lm(log_cost ~ log_gdp, data = df.passport.analysis)
summary(model.gdp)

model.migrants <- lm(log_cost ~ log_migrants, data = df.passport.analysis)
summary(model.migrants)

model.pop <- lm(log_cost ~ log_popn2004, data = df.passport.analysis)
summary(model.pop)

model.political <- lm(log_cost ~ politicalstability, data = df.passport.analysis)
summary(model.political)

model.voice <- lm(log_cost ~ voice, data = df.passport.analysis)
summary(model.voice)

model.commonwealth <- lm(log_cost ~ commonwealth, data = df.passport.analysis)
summary(model.commonwealth)

model.migprop <- lm(log_cost ~ migprop, data = df.passport.analysis)
summary(model.migprop)

model.corruption <- lm(log_cost ~ corruption, data = df.passport.analysis)
summary(model.corruption)