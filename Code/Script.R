# Load Packages
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

# Load data
fedfunds  <- read_csv("./FEDFUNDSPROJ/FEDFUNDS (2).csv") |> rename(date = 1, fedfunds = 2)
inflation <- read_csv("./FEDFUNDSPROJ/PCEPILFE_PC1.csv") |> rename(date = 1, inflation = 2)
unemp     <- read_csv("./FEDFUNDSPROJ/UNRATE.csv")       |> rename(date = 1, unemp = 2)
natural   <- read_csv("./FEDFUNDSPROJ/NROU.csv")         |> rename(date = 1, natural = 2)

# Merge datasets
full_data <- fedfunds |>
  inner_join(inflation, by = "date") |>
  inner_join(unemp,     by = "date") |>
  inner_join(natural,   by = "date") |>
  mutate(gap = unemp - natural) |>
  filter(date >= as.Date("1960-01-01"))


# ----------------------- PART 1 - CURRENT TRENDS ----------------------------

mydata <- full_data |> filter(date >= as.Date("1990-01-01"))

# Run regression
reg_full <- lm(fedfunds ~ inflation + gap, data = mydata)
summary(reg_full)

# Graph 1 - raw data over time series
long_data <- mydata |>
  select(date, fedfunds, inflation, gap) |>
  pivot_longer(cols = -date, names_to = "series", values_to = "value")

ggplot(long_data, aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Fed Funds Rate, Inflation, and Unemployment Gap",
       x = NULL, y = "Percent / percentage points", color = NULL,
       caption = "Source: FRED, Federal Reserve Bank of St. Louis") +
  theme_bw()

# Save predicted FFR from regression results
mydata$predict_full <- predict(reg_full, newdata = mydata)

# Graph 2 - actual vs. predicted
ggplot(mydata, aes(x = date)) +
  geom_line(aes(y = fedfunds, color = "Actual"), linewidth = 0.75) +
  geom_line(aes(y = predict_full, color = "Predicted by model"), linewidth = 0.75) +
  scale_color_manual(values = c("Actual" = "black",
                                "Predicted by model" = "firebrick")) +
  labs(title = "Actual vs. Predicted Federal Funds Rate",
       subtitle = "Where the lines split apart, the Fed behaved differently than the model",
       x = NULL, y = "Federal funds rate (%)", color = NULL,
       caption = "Source: FRED, Federal Reserve Bank of St. Louis") +
  theme_bw()

# Taylor rule prediction, using the unemployment gap in place of the output gap
mydata$taylor <- 2 + mydata$inflation + 0.5 * (mydata$inflation - 2) - 1.0 * mydata$gap

# Graph 3 - actual vs. predicted vs. Taylor prediction
ggplot(mydata, aes(x = date)) +
  geom_line(aes(y = fedfunds, color = "Actual"), linewidth = 0.75) +
  geom_line(aes(y = predict_full, color = "Predicted by model"), linewidth = 0.75) +
  geom_line(aes(y = taylor, color = "Taylor Rule"), linewidth = 0.75) +
  scale_color_manual(values = c("Actual" = "black", 
                                 "Predicted by model" = "firebrick", 
                                 "Taylor Rule" = "navy")) +
  labs(title = "Actual vs. Predicted vs. Taylor Rule Federal Funds Rate",
       subtitle = "Where the lines split apart, the Fed behaved differently than the model",
       x = NULL, y = "Federal funds rate (%)", color = NULL,
       caption = "Source: FRED, Federal Reserve Bank of St. Louis") +
  theme_bw()

# TRUE if the rate was at or near zero, FALSE otherwise
mydata$zlb <- mydata$fedfunds <= 0.25

# Run regression removing quarters in which the Fed hit the ZLB
reg_ex <- lm(fedfunds ~ inflation + gap, data = filter(mydata, !zlb))
summary(reg_ex)

# Save predicted FFR - zlb from regression results
mydata$predict_ex <- predict(reg_ex, newdata = mydata)

# Set zlb periods for comparison 
zlb_periods <- mydata |>
  mutate(run = cumsum(zlb != lag(zlb, default = first(zlb)))) |>
  filter(zlb) |>                                                
  group_by(run) |>
  summarise(start = min(date),
            end   = max(date) + 90)  

# Graph 4 - excluding-ZLB regression, ZLB shaded
y_range <- coord_cartesian(ylim = c(-7, 12))  

ggplot(mydata, aes(x = date)) +
  geom_rect(data = zlb_periods,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = "gray70", alpha = 0.35, inherit.aes = FALSE) +
  geom_line(aes(y = fedfunds, color = "Actual"), linewidth = 0.75) +
  geom_line(aes(y = taylor,   color = "Taylor Rule"), linewidth = 0.75) +
  geom_line(aes(y = predict_ex,  color = "Predicted by model"), linewidth = 0.75) +
  scale_color_manual(values = c("Actual" = "black",
                                "Predicted by model" = "firebrick",
                                "Taylor Rule" = "navy")) +
  y_range +
  labs(title = "Actual vs. Taylor Rule vs. Estimated Rule (excluding ZLB)",
       subtitle = "Gray bands mark zero-lower-bound periods; the regression excludes them",
       x = NULL, y = "Federal funds rate (%)", color = NULL,
       caption = "Source: FRED, Federal Reserve Bank of St. Louis") +
  theme_bw()

# Graph 5 - full-sample regression, ZLB shaded
ggplot(mydata, aes(x = date)) +
  geom_rect(data = zlb_periods,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = "gray70", alpha = 0.35, inherit.aes = FALSE) +
  geom_line(aes(y = fedfunds, color = "Actual"), linewidth = 0.75) +
  geom_line(aes(y = taylor,   color = "Taylor Rule"), linewidth = 0.75) +
  geom_line(aes(y = predict_full, color = "Predicted by model"), linewidth = 0.75) +
  scale_color_manual(values = c("Actual" = "black",
                                "Predicted by model" = "firebrick",
                                "Taylor Rule" = "navy")) +
  y_range +
  labs(title = "Actual vs. Taylor Rule vs. Estimated Rule (full sample)",
       subtitle = "Gray bands mark zero-lower-bound periods; the regression includes them",
       x = NULL, y = "Federal funds rate (%)", color = NULL,
       caption = "Source: FRED, Federal Reserve Bank of St. Louis") +
  theme_bw()


# ----------------------- PART 2 - HISTORICAL CONTEXT ----------------------------

# TRUE if the rate was at or near zero, FALSE otherwise
full_data$zlb <- full_data$fedfunds <= 0.25

# Define the eras from full_data
pre_volcker <- full_data |> filter(date < as.Date("1979-07-01"))
greenspan   <- full_data |> filter(date >= as.Date("1987-07-01"), date < as.Date("2008-01-01"))
post_crisis <- full_data |> filter(date >= as.Date("2008-01-01"), !zlb)

m1 <- lm(fedfunds ~ inflation + gap, data = pre_volcker)
m2 <- lm(fedfunds ~ inflation + gap, data = greenspan)
m3 <- lm(fedfunds ~ inflation + gap, data = post_crisis)

# Coefficients by era, next to Taylor's
era_table <- rbind(
  "1960-1979"      = coef(m1),
  "1987-2007"      = coef(m2),
  "2008+ (no ZLB)" = coef(m3),
  "Taylor rule"    = c(1, 1.5, -1.0)
)
era_table

# Taylor rule prediction, using the unemployment gap in place of the output gap
full_data$taylor <- 2 + full_data$inflation + 0.5 * (full_data$inflation - 2) - 1.0 * full_data$gap

# Graph 6 - Eras graph fedfund vs. Taylor
ggplot(full_data, aes(x = date)) +
  geom_rect(data = zlb_periods,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = "gray70", alpha = 0.35, inherit.aes = FALSE) +
  geom_line(aes(y = fedfunds, color = "Actual"), linewidth = 0.75) +
  geom_line(aes(y = taylor,   color = "Taylor Rule"), linewidth = 0.75) +
  scale_color_manual(values = c("Actual" = "black", "Taylor Rule" = "navy")) +
  labs(title = "Fed Funds Rate vs. Taylor Rule, 1960 to Present",
       x = NULL, y = "Percent", color = NULL,
       caption = "Source: FRED, Federal Reserve Bank of St. Louis") +
  theme_bw()

# Post-crisis era, ZLB quarters excluded (main version)
post_ex  <- full_data |> filter(date >= as.Date("2008-01-01"), !zlb)
m3_ex    <- lm(fedfunds ~ inflation + gap, data = post_ex)

# Post-crisis era, ZLB quarters included (for comparison)
post_all <- full_data |> filter(date >= as.Date("2008-01-01"))
m3_all   <- lm(fedfunds ~ inflation + gap, data = post_all)

era_table <- rbind(
  "1960-1979"                = coef(m1),
  "1987-2007"                = coef(m2),
  "2008+ (excluding ZLB)"    = coef(m3_ex),
  "2008+ (including ZLB)"    = coef(m3_all),
  "Taylor rule"              = c(1, 1.5, -1.0)
)
era_table


# ----------------------- PART 3 - NEWEY WEST SE ----------------------------

library(sandwich)
library(lmtest)

# Regular OLS output, for comparison
summary(reg_full)

# Same model with Newey-West standard errors
coeftest(reg_full, vcov = NeweyWest(reg_full, lag = 4, prewhite = FALSE))

# Do the same for each model you want to report
coeftest(reg_ex, vcov = NeweyWest(reg_ex, lag = 4, prewhite = FALSE))
coeftest(m1,      vcov = NeweyWest(m1,      lag = 4, prewhite = FALSE))
coeftest(m2,      vcov = NeweyWest(m2,      lag = 4, prewhite = FALSE))
coeftest(m3_ex,   vcov = NeweyWest(m3_ex,   lag = 4, prewhite = FALSE))

# Pull the inflation and gap coefficients plus Newey-West SEs from a model
get_coefs <- function(model, era) {
  ct <- coeftest(model, vcov = NeweyWest(model, lag = 4, prewhite = FALSE))
  data.frame(
    era       = era,
    variable  = c("inflation", "gap"),
    estimate  = ct[c("inflation", "gap"), "Estimate"],
    se        = ct[c("inflation", "gap"), "Std. Error"],
    row.names = NULL
  )
}

# Stack one set of results per era model
coef_data <- bind_rows(
  get_coefs(m1,     "1960-1979"),
  get_coefs(m2,     "1987-2007"),
  get_coefs(m3_ex,  "2008+ (excl. ZLB)"),
  get_coefs(m3_all, "2008+ (incl. ZLB)")
) |>
  mutate(
    lower = estimate - 1.96 * se,
    upper = estimate + 1.96 * se,
    era   = factor(era, levels = c("1960-1979", "1987-2007",
                                   "2008+ (excl. ZLB)", "2008+ (incl. ZLB)")),
    variable = factor(variable, levels = c("inflation", "gap"),
                      labels = c("Inflation coefficient", "Gap coefficient"))
  )

coef_data   # check: 8 rows, estimates should match your coeftest() output

# Taylor rule reference values for each panel
taylor_df <- data.frame(
  variable = factor(c("Inflation coefficient", "Gap coefficient"),
                    levels = c("Inflation coefficient", "Gap coefficient")),
  taylor   = c(1.5, -1.0)
)

# Graph 7 - Dot chart of coefficients with Newey-West intervals
ggplot(coef_data, aes(x = era, y = estimate)) +
  geom_hline(data = taylor_df, aes(yintercept = taylor),
             linetype = "dashed", color = "navy") +
  geom_pointrange(aes(ymin = lower, ymax = upper), color = "firebrick") +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "Estimated Policy Response by Era",
       subtitle = "Dots are estimates with 95% intervals (Newey-West, 4 lags); dashed line is the Taylor rule value",
       x = NULL, y = "Coefficient",
       caption = "Source: FRED, Federal Reserve Bank of St. Louis") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# ---------------------------- Saving Results ------------------------------------
results_table <- coef_data |>
  mutate(across(c(estimate, se, lower, upper), ~ round(.x, 3)))

results_table                          
write.csv(results_table, "results_table.csv", row.names = FALSE)

get_results <- function(model, label) {
  ct <- coeftest(model, vcov = NeweyWest(model, lag = 4, prewhite = FALSE))
  data.frame(
    model    = label,
    term     = rownames(ct),        
    estimate = ct[, "Estimate"],
    nw_se    = ct[, "Std. Error"],
    n        = nobs(model),         
    row.names = NULL
  )
}

all_results <- bind_rows(
  get_results(reg_full, "1990-present, full sample"),
  get_results(reg_ex,   "1990-present, excl. ZLB"),
  get_results(m1,       "1960-1979"),
  get_results(m2,       "1987-2007"),
  get_results(m3_ex,    "2008+, excl. ZLB"),
  get_results(m3_all,   "2008+, incl. ZLB")
) |>
  mutate(across(c(estimate, nw_se), ~ round(.x, 3)))

all_results

write.csv(all_results, "all_results.csv", row.names = FALSE)

coefci(reg_full, vcov. = NeweyWest(reg_full, lag = 4, prewhite = FALSE))
coefci(reg_ex,   vcov. = NeweyWest(reg_ex,   lag = 4, prewhite = FALSE))
coefci(m1,   vcov. = NeweyWest(m1,   lag = 4, prewhite = FALSE))
coefci(m2,   vcov. = NeweyWest(m2,   lag = 4, prewhite = FALSE))
coefci(m3_all,   vcov. = NeweyWest(m3_all,   lag = 4, prewhite = FALSE))
coefci(m3_ex,   vcov. = NeweyWest(m3_ex,   lag = 4, prewhite = FALSE))