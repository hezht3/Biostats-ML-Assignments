require(tidyverse)
require(data.table)
require(tidymodels)
require(probably)

setwd("D:/OneDrive - Johns Hopkins/Course/140.644.01 - Statistical Machine Learning Methods, Theory, and Applications/Assignments/biostats644-Assignments/Final project")

load("./DATA/nhanes2003-2004.Rda")


# Preparation


## Data cleaning


data <- nhanes2003_2004 %>% 
    filter(as.numeric(as.character(RIDAGEYR)) >= 50) %>% 
    select(mortstat, RIDAGEYR, RIAGENDR, BPQ010, BPQ060, DIQ010, DIQ050, DIQ090, MCQ010, MCQ053,
           MCQ160A, MCQ160B, MCQ160K, MCQ160L, BMXWAIST, MCQ160M, MCQ220, MCQ245A, MCQ250A, MCQ250B,
           MCQ250C, MCQ250E, MCQ250F, MCQ250G, MCQ265, SSQ011, SSQ051, WHQ030, WHQ040, LBXRDW, HSD010,
           BPXPULS, BPXML1, VIQ200, BMXBMI, BPXSY1, BPXDI1)   # select predictors in instructions

data <- data %>% 
    mutate(
        # outcome
        mortstat = factor(mortstat, levels = c(0, 1),
                                    labels = c("Assumed alive", "Assumed Deceased")),
        
        # demographic variables
        RIDAGEYR = as.numeric(as.character(RIDAGEYR)),
        RIAGENDR = factor(RIAGENDR, levels = c(1, 2), labels = c("Male", "Female")),
        
        # examination
        BMXWAIST = as.numeric(as.character(BMXWAIST)),
        BPXPULS = factor(BPXPULS, levels = c(1, 2), labels = c("Regular", "Irregular")),
        BPXML1 = as.numeric(as.character(BPXML1)),
        BPXSY1 = as.numeric(as.character(BPXSY1)),
        BPXDI1 = as.numeric(as.character(BPXDI1)),
        VIQ200 = ifelse(VIQ200 == 9, NA, VIQ200),
        VIQ200 = factor(VIQ200, levels = c(1, 2), labels = c("Yes", "No")),
        BMXBMI = as.numeric(as.character(BMXBMI)),
        
        # questionnaire
        across(c(BPQ010, BPQ060, DIQ010, DIQ050, DIQ090, MCQ010, MCQ053, MCQ160A, MCQ160B,
                 MCQ160K, MCQ160L, MCQ160M, MCQ220, MCQ245A, MCQ250A, MCQ250B, MCQ250C,
                 MCQ250E, MCQ250F, MCQ250G, MCQ265, SSQ011, SSQ051, WHQ030, WHQ040, HSD010),
               ~ ifelse(.x == 7 | .x == 9, as.factor(NA), .x)),
        across(c(BPQ060, DIQ050, DIQ090, MCQ010, MCQ053, MCQ160A, MCQ160B, MCQ160K, MCQ160L,
                 MCQ160M, MCQ220, MCQ245A, MCQ250A, MCQ250B, MCQ250C, MCQ250E, MCQ250F,
                 MCQ250G, MCQ265),
               ~ factor(.x, levels = c(1, 2),
                        labels = c("Yes", "No"))),
        BPQ010 = factor(BPQ010, levels = c(1, 2, 3, 4, 5),
                                labels = c("Less than 6 months ago", "6 months to 1 year ago",
                                           "More than 1 year to 2 years ago",
                                           "More than 2 years ago", "Never")),
        DIQ010 = factor(DIQ010, levels = c(1, 2, 3), labels = c("Yes", "No", "Borderline")),
        SSQ011 = factor(SSQ011, levels = c(1, 2, 3),
                                labels = c("Yes", "No", "SP doesn't need help")),
        SSQ051 = factor(SSQ051, levels = c(1, 2, 3),
                                labels = c("Yes", "No", "Offered help but wouldn't accept it")),
        WHQ030 = factor(WHQ030, levels = c(1, 2, 3),
                                labels = c("Overweight", "Underweight", "About the right weight")),
        WHQ040 = factor(WHQ040, levels = c(1, 2, 3),
                                labels = c("More", "Less", "Stay about the same")),
        HSD010 = factor(HSD010, levels = c(1, 2, 3, 4, 5),
                                labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
        
        # laboratory
        LBXRDW = as.numeric(as.character(LBXRDW))
        ) %>% 
    # demographic variables
    mutate_at(vars(RIDAGEYR), funs(setattr(., "label", "Age at Screening Adjudicated"))) %>% 
    mutate_at(vars(RIAGENDR), funs(setattr(., "label", "Gender"))) %>% 
    
    # examination
    mutate_at(vars(BMXWAIST), funs(setattr(., "label", "Waist Circumference (cm)"))) %>% 
    mutate_at(vars(BPXPULS), funs(setattr(., "label", "Pulse regular or irregular?"))) %>% 
    mutate_at(vars(BPXML1), funs(setattr(., "label", "MIL: maximum inflation levels (mm Hg)"))) %>% 
    mutate_at(vars(BPXSY1), funs(setattr(., "label", "Systolic: Blood pres (1st rdg) mm Hg"))) %>% 
    mutate_at(vars(BPXDI1), funs(setattr(., "label", "Diastolic: Blood pres (1st rdg) mm Hg"))) %>% 
    mutate_at(vars(VIQ200), funs(setattr(., "label", "Eye surgery for cataracts?"))) %>% 
    mutate_at(vars(BMXBMI), funs(setattr(., "label", "Body Mass Index (kg/m**2)"))) %>% 
    
    # questionnaire
    mutate_at(vars(BPQ010), funs(setattr(., "label", "Last blood pressure reading by doctor"))) %>% 
    mutate_at(vars(BPQ060), funs(setattr(., "label", "Ever had blood cholesterol checked"))) %>% 
    mutate_at(vars(DIQ010), funs(setattr(., "label", "Doctor told you have diabetes"))) %>% 
    mutate_at(vars(DIQ050), funs(setattr(., "label", "Taking insulin now"))) %>% 
    mutate_at(vars(DIQ090), funs(setattr(., "label", "Ulcer/sore not healed within 4 weeks"))) %>% 
    mutate_at(vars(MCQ010), funs(setattr(., "label", "Ever been told you have asthma"))) %>% 
    mutate_at(vars(MCQ053), funs(setattr(., "label", "Taking treatment for anemia/past 3 mos"))) %>% 
    mutate_at(vars(MCQ160A), funs(setattr(., "label", "Doctor ever said you had arthritis"))) %>% 
    mutate_at(vars(MCQ160B), funs(setattr(., "label", "Ever told had congestive heart failure"))) %>% 
    mutate_at(vars(MCQ160K), funs(setattr(., "label", "Ever told you had chronic bronchitis"))) %>% 
    mutate_at(vars(MCQ160L), funs(setattr(., "label", "Ever told you had any liver condition"))) %>% 
    mutate_at(vars(MCQ160M), funs(setattr(., "label", "Ever told you had a thyroid problem"))) %>% 
    mutate_at(vars(MCQ220), funs(setattr(., "label", "Ever told you had cancer or malignancy"))) %>% 
    mutate_at(vars(MCQ245A), funs(setattr(., "label", "Work days missed for illness/maternity"))) %>%
    mutate_at(vars(MCQ250A), funs(setattr(., "label", "Blood relatives have diabetes"))) %>%
    mutate_at(vars(MCQ250B), funs(setattr(., "label", "Blood relatives have Alzheimer's"))) %>%
    mutate_at(vars(MCQ250C), funs(setattr(., "label", "Blood relatives have asthma"))) %>%
    mutate_at(vars(MCQ250E), funs(setattr(., "label", "Blood relatives have osteoporosis"))) %>%
    mutate_at(vars(MCQ250F), funs(setattr(., "label", "Blood relatives w/hypertension/stroke"))) %>%
    mutate_at(vars(MCQ250G), funs(setattr(., "label", "Blood relatives w/hypertension/stroke"))) %>%
    mutate_at(vars(MCQ265), funs(setattr(., "label", "Blood relative have/had prostate cancer"))) %>%
    mutate_at(vars(SSQ011), funs(setattr(., "label", "Anyone to help with emotional support"))) %>%
    mutate_at(vars(SSQ051), funs(setattr(., "label", "Anyone to help with financial support"))) %>%
    mutate_at(vars(WHQ030), funs(setattr(., "label", "How do you consider your weight"))) %>%
    mutate_at(vars(WHQ040), funs(setattr(., "label", "Like to weigh more, less or same"))) %>%
    mutate_at(vars(HSD010), funs(setattr(., "label", "General health condition"))) %>%

    # laboratory
    mutate_at(vars(LBXRDW), funs(setattr(., "label", "Red cell distribution width (%)")))

data <- data %>% 
    mutate(BPXDI1 = ifelse(BPXDI1 <= 20, NA, BPXDI1))   # drop outliers in `BPXDI1`

data <- data %>% drop_na()   # drop missing values


## Exploratory analysis


data %>% 
    select(mortstat, RIDAGEYR, BMXWAIST, BPXML1, BPXSY1, BPXDI1, BMXBMI, LBXRDW) %>% 
    mutate(mortstat = factor(mortstat, labels = c("Alive", "Deceased"))) %>% 
    GGally::ggpairs(aes(color = mortstat, alpha = 0.5)) +
    theme_minimal()   # continuous variable

data %>% 
    select(- RIDAGEYR, - BMXWAIST, - BPXML1, - BPXSY1, - BPXDI1, - BMXBMI, - LBXRDW) %>% 
    pivot_longer(cols = - mortstat) %>%
    ggplot(aes(y = value, fill = mortstat)) +
    geom_bar(position = "fill") +
    facet_wrap(vars(name), scales = "free", ncol = 2) +
    labs(x = NULL, y = NULL, fill = NULL) +
    theme_minimal()   # categorical variable

data %>% 
    select(-mortstat) %>% 
    mutate(across(everything(),
                  ~ as.numeric(.x))) %>% 
    corrr::correlate() %>% 
    corrr::rplot(colours = c("indianred2", "black", "skyblue1")) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))   # pair-wise correlation


## Re-sampling

set.seed(123)
data_split <- initial_split(data, strata = "mortstat", prop = 8/10)

data_train <- training(data_split)
data_test <- testing(data_split)

data_fold <- vfold_cv(data_train, strata = "mortstat", v = 10)


# Linear model


## Logistic regression


### Fit logistic regression model


logistic_recipe <- recipe(mortstat ~ ., data = data_train) %>% 
    step_dummy(all_nominal(), - all_outcomes()) %>%
    step_zv(all_numeric()) %>% 
    step_corr(threshold = 0.8)   # specify recipe, exclude variables closely correlated (r = 0.8)

logistic_spec <- logistic_reg() %>%
    set_engine("glm") %>% 
    set_mode("classification")   # specify logistic model

logistic_workflow <- 
    workflow() %>% 
    add_model(logistic_spec) %>% 
    add_recipe(logistic_recipe)   # build logistic model workflow

logistic_fit <- logistic_workflow %>%
    fit(data = data_train)   # fit logistic model in training set


### Evaluate model performance


augment(logistic_fit, new_data = data_test) %>% 
    conf_mat(truth = mortstat, estimate = .pred_class)   # confusion matrix

augment(logistic_fit, new_data = data_test) %>% 
    accuracy(truth = mortstat, estimate = .pred_class) %>% 
    bind_rows(augment(logistic_fit, new_data = data_test) %>% 
                  sens(truth = mortstat, estimate = .pred_class)) %>% 
    bind_rows(augment(logistic_fit, new_data = data_test) %>% 
                  spec(truth = mortstat, estimate = .pred_class)) %>% 
    bind_rows(augment(logistic_fit, new_data = data_test) %>% 
                  roc_auc(truth = mortstat, `.pred_Assumed alive`))   # accuracy, sensitivity, specificity, auc


### Model post-processing


data_threshold <- logistic_fit %>%
    augment(new_data = data_test) %>% 
    threshold_perf(mortstat, `.pred_Assumed alive`, thresholds = seq(0.5, 1, by = 0.0025)) %>%
    filter(.metric != "distance") %>%
    mutate(group = case_when(.metric == "sens" | .metric == "spec" ~ "1",
                             TRUE ~ "2"))

max_j_index_threshold <- data_threshold %>%
    filter(.metric == "j_index") %>%
    filter(.estimate == max(.estimate)) %>%
    pull(.threshold)

ggplot(data_threshold, aes(x = .threshold, y = .estimate, color = .metric, alpha = group)) +
    geom_line() +
    scale_color_viridis_d(end = 0.9) +
    scale_alpha_manual(values = c(.4, 1), guide = "none") +
    geom_vline(xintercept = max_j_index_threshold, alpha = .6, color = "grey30") +
    labs(x = "'Good' Threshold\n(above this value is considered 'good')",
         y = "Metric Estimate",
         title = "Balancing performance by varying the threshold",
         subtitle = "Vertical line = Max J-Index") +
    theme_minimal()

data_threshold %>% 
    filter(.threshold %in% max_j_index_threshold) %>% 
    arrange(.threshold)

best_threshold <- data_threshold %>% 
    filter(.threshold %in% max_j_index_threshold) %>%
    filter(.metric == "j_index") %>% 
    pull(.threshold)

data_preds_final <- logistic_fit %>%
    augment(new_data = data_test) %>% 
    mutate(.pred_class = make_two_class_pred(`.pred_Assumed alive`, levels(mortstat), 
                                             threshold = best_threshold))

data_preds_final %>% 
    conf_mat(truth = mortstat, estimate = .pred_class)   # confusion matrix

data_preds_final %>% 
    accuracy(truth = mortstat, estimate = .pred_class) %>% 
    bind_rows(data_preds_final %>% sens(truth = mortstat, estimate = .pred_class)) %>% 
    bind_rows(data_preds_final %>% spec(truth = mortstat, estimate = .pred_class)) %>% 
    bind_rows(data_preds_final %>% roc_auc(truth = mortstat, `.pred_Assumed alive`))   # accuracy, sensitivity, specificity, auc


## Lasso


### Fit lasso regression model


lasso_recipe <- recipe(mortstat ~ ., data = data_train) %>% 
    step_dummy(all_nominal(), - all_outcomes()) %>%
    step_zv(all_numeric()) %>% 
    step_normalize(all_predictors())   # specify recipe

lasso_spec <- logistic_reg(penalty = tune(), mixture = 1) %>% 
    set_engine("glmnet") %>% 
    set_mode("classification")   # specify lasso model

lasso_workflow <- 
    workflow() %>% 
    add_model(lasso_spec) %>% 
    add_recipe(lasso_recipe)   # build lasso model workflow

penalty_grid <- grid_regular(penalty(range = c(-4, 2)), levels = 50)   # tuning parameter

tune_res <- lasso_workflow %>% 
    tune_grid(
    resamples = data_fold, 
    grid = penalty_grid,
    control = control_grid(save_pred = TRUE)
)

autoplot(tune_res) + theme_minimal()

best_penalty <- select_best(tune_res, metric = "roc_auc")

lasso_final <- finalize_workflow(lasso_workflow, best_penalty)

lasso_final_fit <- fit(lasso_final, data = data_train)


### Evaluate model performance


data_laaso_preds <- augment(lasso_final_fit, new_data = data_test)

data_laaso_preds %>% 
    conf_mat(truth = mortstat, estimate = .pred_class)

data_laaso_preds %>% 
    accuracy(truth = mortstat, estimate = .pred_class) %>% 
    bind_rows(data_laaso_preds %>% sens(truth = mortstat, estimate = .pred_class)) %>% 
    bind_rows(data_laaso_preds %>% spec(truth = mortstat, estimate = .pred_class)) %>% 
    bind_rows(data_laaso_preds %>% roc_auc(truth = mortstat, `.pred_Assumed alive`))


### Model post-processing


data_threshold <- lasso_final_fit %>%
    augment(new_data = data_test) %>% 
    threshold_perf(mortstat, `.pred_Assumed alive`, thresholds = seq(0.5, 1, by = 0.0025)) %>%
    filter(.metric != "distance") %>%
    mutate(group = case_when(.metric == "sens" | .metric == "spec" ~ "1",
                             TRUE ~ "2"))

max_j_index_threshold <- data_threshold %>%
    filter(.metric == "j_index") %>%
    filter(.estimate == max(.estimate)) %>%
    pull(.threshold)

ggplot(data_threshold, aes(x = .threshold, y = .estimate, color = .metric, alpha = group)) +
    geom_line() +
    scale_color_viridis_d(end = 0.9) +
    scale_alpha_manual(values = c(.4, 1), guide = "none") +
    geom_vline(xintercept = max_j_index_threshold, alpha = .6, color = "grey30") +
    labs(x = "'Good' Threshold\n(above this value is considered 'good')",
         y = "Metric Estimate",
         title = "Balancing performance by varying the threshold",
         subtitle = "Vertical line = Max J-Index") +
    theme_minimal()

data_threshold %>% 
    filter(.threshold %in% max_j_index_threshold) %>% 
    arrange(.threshold)

best_threshold <- data_threshold %>% 
    filter(.threshold %in% max_j_index_threshold) %>%
    filter(.metric == "j_index") %>% 
    pull(.threshold)

data_preds_final <- lasso_final_fit %>%
    augment(new_data = data_test) %>% 
    mutate(.pred_class = make_two_class_pred(`.pred_Assumed alive`, levels(mortstat), 
                                             threshold = best_threshold))

data_preds_final %>% 
    conf_mat(truth = mortstat, estimate = .pred_class)   # confusion matrix

data_preds_final %>% 
    accuracy(truth = mortstat, estimate = .pred_class) %>% 
    bind_rows(data_preds_final %>% sens(truth = mortstat, estimate = .pred_class)) %>% 
    bind_rows(data_preds_final %>% spec(truth = mortstat, estimate = .pred_class)) %>% 
    bind_rows(data_preds_final %>% roc_auc(truth = mortstat, `.pred_Assumed alive`))   # accuracy, sensitivity, specificity, auc


# Tree-Based Methods


## RandomForest


rf_recipe <- recipe(mortstat ~ ., data = data_train) %>% 
    step_dummy(all_nominal(), - all_outcomes()) %>%
    step_zv(all_numeric())   # specify recipe

rf_spec <- rand_forest(mtry = 6) %>%
    set_engine("randomForest", importance = TRUE) %>%
    set_mode("classification")   # specify random forests model

rf_workflow <- 
    workflow() %>% 
    add_model(rf_spec) %>% 
    add_recipe(rf_recipe)   # build random forests model workflow

rf_fit <- rf_workflow %>%
    fit(data = data_train)

data_preds <- rf_fit %>%
    augment(new_data = data_test)

data_preds %>% 
    conf_mat(truth = mortstat, estimate = .pred_class)   # confusion matrix

data_preds %>% 
    accuracy(truth = mortstat, estimate = .pred_class) %>% 
    bind_rows(data_preds %>% sens(truth = mortstat, estimate = .pred_class)) %>% 
    bind_rows(data_preds %>% spec(truth = mortstat, estimate = .pred_class)) %>% 
    bind_rows(data_preds %>% roc_auc(truth = mortstat, `.pred_Assumed alive`))   # accuracy, sensitivity, specificity, auc

data_threshold <- rf_fit %>%
    augment(new_data = data_test) %>% 
    threshold_perf(mortstat, `.pred_Assumed alive`, thresholds = seq(0.5, 1, by = 0.0025)) %>%
    filter(.metric != "distance") %>%
    mutate(group = case_when(.metric == "sens" | .metric == "spec" ~ "1",
                             TRUE ~ "2"))

max_j_index_threshold <- data_threshold %>%
    filter(.metric == "j_index") %>%
    filter(.estimate == max(.estimate)) %>%
    pull(.threshold)

ggplot(data_threshold, aes(x = .threshold, y = .estimate, color = .metric, alpha = group)) +
    geom_line() +
    scale_color_viridis_d(end = 0.9) +
    scale_alpha_manual(values = c(.4, 1), guide = "none") +
    geom_vline(xintercept = max_j_index_threshold, alpha = .6, color = "grey30") +
    labs(x = "'Good' Threshold\n(above this value is considered 'good')",
         y = "Metric Estimate",
         title = "Balancing performance by varying the threshold",
         subtitle = "Vertical line = Max J-Index") +
    theme_minimal()

data_threshold %>% 
    filter(.threshold %in% max_j_index_threshold) %>% 
    arrange(.threshold)

best_threshold <- data_threshold %>% 
    filter(.threshold %in% max_j_index_threshold) %>%
    filter(.metric == "j_index") %>% 
    pull(.threshold)

data_preds_final <- rf_fit %>%
    augment(new_data = data_test) %>% 
    mutate(.pred_class = make_two_class_pred(`.pred_Assumed alive`, levels(mortstat), 
                                             threshold = best_threshold))

data_preds_final %>% 
    conf_mat(truth = mortstat, estimate = .pred_class)   # confusion matrix

data_preds_final %>% 
    accuracy(truth = mortstat, estimate = .pred_class) %>% 
    bind_rows(data_preds_final %>% sens(truth = mortstat, estimate = .pred_class)) %>% 
    bind_rows(data_preds_final %>% spec(truth = mortstat, estimate = .pred_class)) %>% 
    bind_rows(data_preds_final %>% roc_auc(truth = mortstat, `.pred_Assumed alive`))   # accuracy, sensitivity, specificity, auc

## Boosting trees

### XGBoost model specification

xgb_spec <- boost_tree(
    trees = 1000, 
    tree_depth = tune(), min_n = tune(), 
    loss_reduction = tune(),                     ## first three: model complexity
    sample_size = tune(), mtry = tune(),         ## randomness
    learn_rate = tune()                        ## step size
) %>% 
    set_engine("xgboost") %>% 
    set_mode("classification")

xgb_grid <- grid_latin_hypercube(
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), data_train),
    learn_rate(),
    size = 30
)

xgb_wf <- workflow() %>%
    add_formula(mortstat ~ .) %>%
    add_model(xgb_spec)

doParallel::registerDoParallel()

set.seed(234)
xgb_res <- tune_grid(
    xgb_wf,
    resamples = data_fold,
    grid = xgb_grid,
    control = control_grid(save_pred = TRUE)
)

xgb_res

best_auc <- select_best(xgb_res, "roc_auc")
best_auc

final_xgb <- finalize_workflow(
    xgb_wf,
    best_auc
)

final_xgb

final_xgb_fit <- fit(final_xgb, data = data_train)

data_preds <- final_xgb_fit %>%
    augment(new_data = data_test)

data_preds %>% 
    conf_mat(truth = mortstat, estimate = .pred_class)   # confusion matrix

data_preds %>% 
    accuracy(truth = mortstat, estimate = .pred_class) %>% 
    bind_rows(data_preds %>% sens(truth = mortstat, estimate = .pred_class)) %>% 
    bind_rows(data_preds %>% spec(truth = mortstat, estimate = .pred_class)) %>% 
    bind_rows(data_preds %>% roc_auc(truth = mortstat, `.pred_Assumed alive`))   # accuracy, sensitivity, specificity, auc

data_threshold <- final_xgb_fit %>%
    augment(new_data = data_test) %>% 
    threshold_perf(mortstat, `.pred_Assumed alive`, thresholds = seq(0.5, 1, by = 0.0025)) %>%
    filter(.metric != "distance") %>%
    mutate(group = case_when(.metric == "sens" | .metric == "spec" ~ "1",
                             TRUE ~ "2"))

max_j_index_threshold <- data_threshold %>%
    filter(.metric == "j_index") %>%
    filter(.estimate == max(.estimate)) %>%
    pull(.threshold)

ggplot(data_threshold, aes(x = .threshold, y = .estimate, color = .metric, alpha = group)) +
    geom_line() +
    scale_color_viridis_d(end = 0.9) +
    scale_alpha_manual(values = c(.4, 1), guide = "none") +
    geom_vline(xintercept = max_j_index_threshold, alpha = .6, color = "grey30") +
    labs(x = "'Good' Threshold\n(above this value is considered 'good')",
         y = "Metric Estimate",
         title = "Balancing performance by varying the threshold",
         subtitle = "Vertical line = Max J-Index") +
    theme_minimal()

data_threshold %>% 
    filter(.threshold %in% max_j_index_threshold) %>% 
    arrange(.threshold)

best_threshold <- data_threshold %>% 
    filter(.threshold %in% max_j_index_threshold) %>%
    filter(.metric == "j_index") %>% 
    pull(.threshold)

data_preds_final <- final_xgb_fit %>%
    augment(new_data = data_test) %>% 
    mutate(.pred_class = make_two_class_pred(`.pred_Assumed alive`, levels(mortstat), 
                                             threshold = best_threshold))

data_preds_final %>% 
    conf_mat(truth = mortstat, estimate = .pred_class)   # confusion matrix

data_preds_final %>% 
    accuracy(truth = mortstat, estimate = .pred_class) %>% 
    bind_rows(data_preds_final %>% sens(truth = mortstat, estimate = .pred_class)) %>% 
    bind_rows(data_preds_final %>% spec(truth = mortstat, estimate = .pred_class)) %>% 
    bind_rows(data_preds_final %>% roc_auc(truth = mortstat, `.pred_Assumed alive`))   # accuracy, sensitivity, specificity, auc
