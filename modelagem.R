library(tigris) #ok
library(tidyverse) # ok
library(glue) #ok
library(readxl) #ok
library(ggrepel) #ok
library(caret) # ok
library(glmmTMB) #ok
library(lmtest) #ok
library(tidycensus) #ok
library(zipcodeR) #ok
library(buildmer) #ok
library(mgcv)
library(pROC) #ok
library(randomForest)
library(rpart)
library(rpart.plot)
library(fastDummies)
library(lme4)
options(tigris_use_cache = T)
theme_set(theme_light())
setwd("/home/lmonteiro/Documents/mba/tcc/new_telco_churn")
NEW_MODELS_DIR <- 'new_modelagem/new_modelos'

read_xlsx_custom <- function(file_name) {
    dados <- read_xlsx(file_name) %>% 
        rename_with(~ str_to_lower(str_replace_all(.x, ' ', '_')))
    
    return(dados)
}


renomear_variaveis_censo <- function(tmp, geografia='condado') {
    tmp <- tmp %>% 
        mutate(variable = case_when(variable == 'B01001_001' ~ glue('{geografia}_qtd_habitantes'),
                                    variable == 'B01001_002' ~ glue('{geografia}_qtd_habitantes_homens'),
                                    variable == 'B01002_001' ~ glue('{geografia}_idade_mediana_habitantes'),
                                    variable == 'B09001_001' ~ glue('{geografia}_qtd_habitantes_menor_18_anos'),
                                    variable == 'B19083_001' ~ glue('{geografia}_indice_gini_desigualdade_renda'),
                                    variable == 'B19113_001' ~ glue('{geografia}_renda_familiar_mediana')))
    return(tmp)
}

descricao <- read_delim('csvs/descricao_variaveis.csv', '|')


# American Community Survey
about_variaveis_censo_acs <- load_variables(year = 2017, dataset = 'acs5', cache = T) %>%
    mutate(across(all_of(c('label', 'concept')), str_to_lower),
           label = str_replace_all(label, '!!', ', ')) %>%
    filter(!str_detect(concept, 'american indian|asian|black or african american|hispanic or latino|native hawaiian and other pacific islander|other race|two or more races|white alone'))

variaveis_censo_condado <- get_acs(geography = "county",
                                   variables = c('B01001_001', 'B01001_002', 'B01002_001', 
                                                 'B09001_001', 'B19083_001', 'B19113_001'),
                                   state = "CA",
                                   year = 2017,
                                   geometry = F) %>% 
    left_join(about_variaveis_censo_acs, by = c('variable' = 'name')) %>% 
    select(NAME, variable, estimate) %>% 
    renomear_variaveis_censo(geografia = 'condado') %>% 
    spread(variable, estimate) %>% 
    mutate(NAME = str_remove(NAME, ', California'),
           condado_tx_habitantes_homens = condado_qtd_habitantes_homens / condado_qtd_habitantes,
           condado_tx_habitantes_menor_18_anos = condado_qtd_habitantes_menor_18_anos / condado_qtd_habitantes) %>% 
    select(-c(condado_qtd_habitantes_homens, condado_qtd_habitantes_menor_18_anos))


variaveis_censo_zip_code <- get_acs(geography = "zip code tabulation area",
                                    variables = c('B01001_001', 'B01001_002', 'B01002_001', 
                                                  'B09001_001', 'B19083_001', 'B19113_001'),
                                    state = "CA",
                                    year = 2017,
                                    geometry = F) %>% 
    select(NAME, variable, estimate) %>% 
    renomear_variaveis_censo(geografia = 'zip_code') %>% 
    spread(variable, estimate) %>% 
    mutate(NAME = str_remove(NAME, 'ZCTA5 '),
           zip_code_tx_habitantes_homens = zip_code_qtd_habitantes_homens / zip_code_qtd_habitantes,
           zip_code_tx_habitantes_menor_18_anos = zip_code_qtd_habitantes_menor_18_anos / zip_code_qtd_habitantes) %>% 
    select(-c(zip_code_qtd_habitantes_homens, zip_code_qtd_habitantes_menor_18_anos))


tc_demographics <- read_xlsx_custom('csvs/Telco_customer_churn_demographics.xlsx') %>% 
    select(customer_id, gender, age, married, number_of_dependents)

tc_location <- read_xlsx_custom('csvs/Telco_customer_churn_location.xlsx') %>% 
    select(customer_id, city, zip_code, latitude, longitude) %>% 
    mutate(zip_code = as.character(zip_code))

tc_population <- read_xlsx_custom('csvs/Telco_customer_churn_population.xlsx') %>% 
    select(zip_code) %>% 
    mutate(zip_code = as.character(zip_code))

tc_services <- read_xlsx_custom('csvs/Telco_customer_churn_services.xlsx') %>% 
    select(customer_id, number_of_referrals, tenure_in_months, offer, phone_service, avg_monthly_long_distance_charges, multiple_lines, internet_service, internet_type, avg_monthly_gb_download, online_security, online_backup, device_protection_plan, premium_tech_support, streaming_tv, streaming_movies, streaming_music, unlimited_data, contract, paperless_billing, payment_method, monthly_charge, total_charges, total_refunds, total_extra_data_charges, total_long_distance_charges)

tc_status <- read_xlsx_custom('csvs/Telco_customer_churn_status.xlsx') %>% 
    select(customer_id, satisfaction_score, customer_status, churn_value, cltv, churn_category, churn_reason)

zip_county <- zipcodeR::zip_code_db %>%
    filter(state == 'CA') %>% 
    select(zipcode, county)

counties_stats <- tigris::counties(state = 'CA', year = 2010) %>% 
    sf::st_drop_geometry() %>%
    select(NAMELSAD10, ALAND10) %>% 
    rename(county = 1,
           condado_area_terra_m2 = 2)

zip_code_stats <- tigris::zctas(state = 'CA', year = 2010) %>% 
    sf::st_drop_geometry() %>%
    select(ZCTA5CE10, ALAND10) %>% 
    rename(zip_code = 1,
           zip_code_area_terra_m2 = 2)

tc <- tc_demographics %>% 
    left_join(tc_location, by = 'customer_id') %>% 
    left_join(tc_population, by = 'zip_code') %>% 
    left_join(tc_services, by = 'customer_id') %>% 
    left_join(tc_status, by = 'customer_id') %>% 
    left_join(zip_county, by = c('zip_code' = 'zipcode')) %>% 
    left_join(variaveis_censo_condado, by = c('county' = 'NAME')) %>% 
    left_join(variaveis_censo_zip_code, by = c('zip_code' = 'NAME')) %>% 
    left_join(counties_stats, by = 'county') %>% 
    left_join(zip_code_stats, by = 'zip_code') %>% 
    rename(flg_device_protection_plan = device_protection_plan,
           flg_internet_service = internet_service,
           flg_online_backup = online_backup,
           flg_online_security = online_security,
           flg_phone_service = phone_service,
           flg_premium_tech_support = premium_tech_support,
           flg_multiple_lines = multiple_lines,
           flg_streaming_tv = streaming_tv,
           flg_streaming_movies = streaming_movies,
           flg_streaming_music = streaming_music,
           flg_unlimited_data = unlimited_data,
           flg_paperless_billing = paperless_billing,
           flg_married = married) %>% 
    mutate(across(matches('flg_'), ~ ifelse(.x == 'Yes', 1, 0))) %>%
    rename(flg_churn = churn_value) %>% 
    mutate(condado_densidade_populacional = condado_qtd_habitantes / condado_area_terra_m2,
           zip_code_densidade_populacional = zip_code_qtd_habitantes / zip_code_area_terra_m2,
           valor_cobranca_geral = total_charges + total_long_distance_charges + total_extra_data_charges,
           tx_valores_reembolsados = total_refunds / valor_cobranca_geral,
           tx_concentracao_cobranca_mes_q3 = monthly_charge / total_charges, # possivel indicador da quantidade de meses que o cliente esta com a companhia, e se o valor da mensalidade atual eh superior ao das mensalidades anteriores
           valor_cobrancas_extras = total_long_distance_charges + total_extra_data_charges,
           tx_contrib_cobrancas_extras_cobranca_geral = valor_cobrancas_extras / valor_cobranca_geral,
           qtd_servicos_principais = flg_internet_service + flg_phone_service,
           qtd_servicos_adicionais = flg_unlimited_data + flg_device_protection_plan + flg_online_backup + flg_online_security + flg_premium_tech_support,
           qtd_streamings = flg_streaming_movies + flg_streaming_music + flg_streaming_tv,
           across(matches('flg_'), as.factor),
           across(where(is.character), as.factor),
           satisfaction_score = as.factor(satisfaction_score),
           flg_churn_numeric = ifelse(flg_churn == '1', 1, 0))

# colnames(tc) %>% paste(collapse = '\n') %>% cat()

tc %>% select(matches('tx_')) %>% summary()

rm(list = c('tc_demographics', 'tc_location', 'tc_population', 'tc_services', 'tc_status'))

# Funcoes
get_indicadores_modelo <- function(modelo, conjunto_teste, cutoff = 0.5, descricao) {
    
    if(inherits(modelo, 'glm') | inherits(modelo, 'glmerMod')) {
        tmp_model_stats <- confusionMatrix(as.factor(ifelse(predict(modelo, conjunto_teste, type = 'response') >= cutoff, 1, 0)),
                                           reference = conjunto_teste$flg_churn,
                                           positive = '1',
                                           mode = 'sens_spec')
        
        tmp_previsoes_rocr <- ROCR::prediction(predict(modelo, conjunto_teste, type = 'response'),
                                               conjunto_teste$flg_churn)
        
    } else {
        
        if(inherits(modelo, 'train')) {
            tmp_model_stats <- confusionMatrix(as.factor(ifelse(predict(modelo, conjunto_teste, type = 'prob')[,2] >= cutoff, 'Sim', 'Não')),
                                               reference = conjunto_teste$flg_churn,
                                               positive = 'Sim',
                                               mode = 'sens_spec')
        } else {
            tmp_model_stats <- confusionMatrix(as.factor(ifelse(predict(modelo, conjunto_teste, type = 'prob')[,2] >= cutoff, 1, 0)),
                                               reference = conjunto_teste$flg_churn,
                                               positive = '1',
                                               mode = 'sens_spec')
        }
        
        
        
        tmp_previsoes_rocr <- ROCR::prediction(predict(modelo, conjunto_teste, type = 'prob')[,2],
                                               conjunto_teste$flg_churn)
    }
    
    model_stats <- tibble(`Ponto de corte` = cutoff,
                          `Acurácia` = tmp_model_stats$overall[['Accuracy']],
                          Sensitividade = tmp_model_stats$byClass[['Sensitivity']],
                          Especificidade = tmp_model_stats$byClass[['Specificity']])
    
    area_sob_curva <- ROCR::performance(tmp_previsoes_rocr, 'auc')@y.values %>%
        unlist()
    
    model_stats <- model_stats %>%
        mutate(AUC = area_sob_curva,
               descricao = descricao) %>% 
        select(descricao, everything())
    
    return(model_stats)
}


# Modelagem
tc_mod <- tc %>% 
    select(-c(customer_id, latitude, longitude,
              customer_status, churn_category, churn_reason, flg_churn_numeric)) %>% 
    select(-matches('zip_code')) %>% 
    select(-c(city))

# verifica colunas com valores faltantes
tc_mod %>%
    summarise(across(everything(), ~ sum(is.na(.x)))) %>% 
    gather(variavel, qtd_nulos, everything()) %>% 
    arrange(desc(qtd_nulos))

tc_scaled <- tc_mod %>% 
    mutate(across(where(is.double) & !matches('tx_'), ~ .x - mean(.x, na.rm = T)))

variaveis_dummizar <- tc_mod %>%
    select(!where(is.double)) %>%
    select(-matches('flg_')) %>%
    colnames()

tc_dummies <- dummy_columns(.data = tc_mod,
                            select_columns = variaveis_dummizar,
                            remove_selected_columns = TRUE,
                            remove_first_dummy = TRUE) %>%
    rename_with(~ str_replace_all(.x, ' ', '_'), matches(' '))


rf_tc_dummies <- dummy_columns(.data = tc_mod,
                               select_columns = 'county',
                               remove_selected_columns = TRUE,
                               remove_first_dummy = TRUE) %>%
    rename_with(~ str_replace_all(.x, ' ', '_'), matches(' '))

## Amostragem
set.seed(3)
train_idx <- createDataPartition(tc_mod$flg_churn, p = .7, list = F)
tc_train <- tc_mod[train_idx,]
tc_test <- tc_mod[-train_idx,]

dim(tc_train)

set.seed(3)
dummies_train_idx <- createDataPartition(tc_dummies$flg_churn, p = .7, list = F)
dummies_tc_train <- tc_dummies[dummies_train_idx,]
dummies_tc_test <- tc_dummies[-dummies_train_idx,]

dim(dummies_tc_train)

set.seed(3)
rf_dummies_train_idx <- createDataPartition(rf_tc_dummies$flg_churn, p = .7, list = F)
rf_dummies_tc_train <- rf_tc_dummies[rf_dummies_train_idx,]
rf_dummies_tc_test <- rf_tc_dummies[-rf_dummies_train_idx,]

dim(dummies_tc_train)


## Analise de correlacao entre variaveis numericas
corr_m <- tc_mod %>%
    select(where(is.double)) %>%
    cor()

corr_m[upper.tri(corr_m, diag = T)] <- NA
corr_m <- corr_m %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    rename(v1 = 1)

to_plot_corr_m <- corr_m %>% 
    pivot_longer(-v1, names_to = 'v2', values_to = 'correlacao') %>% 
    mutate(across(c(v1, v2), as_factor))

ggplot(to_plot_corr_m, aes(v1, v2, fill = correlacao)) +
    geom_tile() +
    # scale_fill_viridis_c(na.value = "#FF000000") +
    scale_fill_viridis_c(option = 'D', na.value = "#FF000000") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    labs(x = NULL, y = NULL, fill = 'Correlação')

ggsave("plots/correlacao_variaveis_preditoras_numericas.png", width = 12, height = 9)


tabela_correlacao <- to_plot_corr_m %>% 
    filter(!is.na(correlacao)) %>% 
    mutate(correlacao_absoluta = abs(correlacao),
           across(c(correlacao, correlacao_absoluta), ~round(.x, 3))) %>% 
    arrange(desc(correlacao_absoluta))

write_delim(tabela_correlacao, 'csvs_redacao/tabela_correlacao_variaveis_preditoras_numericas.csv', delim = ';')

## Regressao logistica

modelo_rl_vazio <- glm(flg_churn ~ 1,
                       data = dummies_tc_train,
                       na.action = na.omit,
                       family = binomial)

modelo_rl_tudo <- glm(flg_churn ~ .,
                      data = dummies_tc_train,
                      na.action = na.omit,
                      family = binomial)

AIC(modelo_rl_tudo)

# modelo_rl_forward <- MASS::stepAIC(modelo_rl_vazio,
#                                    direction = 'forward',
#                                    scope = list(lower = modelo_rl_vazio, upper = modelo_rl_tudo),
#                                    trace = 1)
# 
# modelo_rl_backward <- MASS::stepAIC(modelo_rl_tudo,
#                                     direction = 'backward',
#                                     scope = list(lower = modelo_rl_vazio, upper = modelo_rl_tudo),
#                                     trace = 1)
# 
# modelo_rl_both <- MASS::stepAIC(modelo_rl_vazio,
#                                 direction = 'both',
#                                 scope = list(lower = modelo_rl_vazio, upper = modelo_rl_tudo),
#                                 trace = 1)

aic_modelo_rl_forward <- read_delim('csvs_redacao/modelo_rl_forward.csv', delim = ';') %>% 
    mutate(criterio = 'Progressiva',
           step = row_number())

aic_modelo_rl_backward <- read_delim('csvs_redacao/modelo_rl_backward.csv', delim = ';') %>% 
    mutate(criterio = 'Regressiva',
           step = row_number())

aic_modelo_rl_both <- read_delim('csvs_redacao/modelo_rl_both.csv', delim = ';') %>% 
    mutate(criterio = 'Ambas',
           step = row_number())

aic_modelos_rl <- aic_modelo_rl_forward %>% 
    rbind(aic_modelo_rl_backward) %>% 
    rbind(aic_modelo_rl_both) %>% 
    mutate(criterio = as_factor(criterio),
           qtd_variaveis = ifelse(formula_modelo == 'flg_churn ~ 1',
                                  0,
                                  str_count(formula_modelo, '[+]') + 1))

to_plot_aic_modelos_rl <- aic_modelos_rl %>% 
    pivot_longer(c(aic, qtd_variaveis), names_to = 'var', values_to = 'value') %>% 
    mutate(var = case_when(var == 'aic' ~ 'AIC',
                           var == 'qtd_variaveis' ~ 'Quantidade de variáveis',
                           T ~ 'Outro'))

ggplot(to_plot_aic_modelos_rl %>% 
           filter(var == 'AIC'), aes(step, value, color = criterio)) +
    geom_line() +
    scale_color_viridis_d() +
    labs(x = 'Iteração', y = 'AIC', color = 'Direção')

ggsave("plots/modelos_rl_stepwise_reducao_dos_aics.png", width = 9, height = 5)


ggplot(to_plot_aic_modelos_rl %>% 
           filter(var == 'Quantidade de variáveis'), aes(step, value, color = criterio)) +
    geom_line() +
    scale_color_viridis_d() +
    labs(x = 'Iteração', y = 'Quantidade de variáveis', color = 'Direção')

ggsave("plots/modelos_rl_stepwise_qtd_variaveis.png", width = 9, height = 5)


resumo_modelos_rl <- aic_modelos_rl %>% 
    select(-descricao) %>% 
    group_by(criterio) %>% 
    summarise(across(everything(), list(first = first, last = last), .names = '{.fn}_{.col}')) %>% 
    pivot_longer(matches('first_|last_'),
                 names_to = c('descricao', '.value'),
                 names_pattern = '(first|last)_(.+)') %>% 
    arrange(criterio, descricao) %>% 
    select(criterio, descricao, step, aic, qtd_variaveis) %>% 
    mutate(descricao = ifelse(descricao == 'first', 'primeira', 'última')) %>% 
    rename(iteracao = step)

write_delim(resumo_modelos_rl, 'csvs_redacao/resumo_modelos_rl_stepwise.csv', delim = ';')


### Melhor modelo obtido, com AIC = 836.78. criterio both

modelo_rl_both <- glm(flg_churn ~ satisfaction_score_4 + satisfaction_score_5 + satisfaction_score_3 + flg_online_security + number_of_referrals + monthly_charge + tx_concentracao_cobranca_mes_q3 + contract_Two_Year + flg_married + number_of_dependents + county_San_Diego_County + flg_premium_tech_support + flg_phone_service + contract_One_Year + offer_Offer_E + county_Mendocino_County + offer_Offer_A + county_Lake_County + age + county_Nevada_County + tx_contrib_cobrancas_extras_cobranca_geral + county_Fresno_County + county_El_Dorado_County + county_Tulare_County + total_charges + county_San_Mateo_County + avg_monthly_long_distance_charges + valor_cobrancas_extras + internet_type_Fiber_Optic + flg_online_backup,
                      data = dummies_tc_train,
                      na.action = na.omit,
                      family = binomial)

coef_modelo_rl_both <- summary(modelo_rl_both)$coefficients %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    mutate(across(!c(rowname), ~ round(.x, 3))) %>%
    rename(`Variável preditora` = rowname,
           Coeficiente = Estimate,
           `Erro padrão` = `Std. Error`,
           `Valor z` = `z value`)

write_delim(coef_modelo_rl_both, 'csvs_redacao/coeficientes_estimados_modelo_rl_both.csv', delim = ';')

indicadores_modelo_rl_both <- seq(0.1, 0.9, 0.05) %>% 
    map_dfr(~ get_indicadores_modelo(modelo_rl_both, dummies_tc_test, cutoff = .x, 'Regressão logística stepwise')) %>% 
    mutate(AIC = AIC(modelo_rl_both))

to_plot_indicadores_modelo_rl_both <- indicadores_modelo_rl_both %>% 
    pivot_longer(c(`Acurácia`, Sensitividade, Especificidade), names_to = 'nome_indicador', values_to = 'indicador')

ggplot(to_plot_indicadores_modelo_rl_both,
       aes(`Ponto de corte`, indicador, color = nome_indicador)) +
    geom_line() +
    geom_point() +
    scale_color_viridis_d() +
    labs(y = NULL, color = 'Indicador')

# plotly::ggplotly()

ggsave("plots/indicadores_modelo_rl_both.png", width = 9, height = 5)

write_delim(indicadores_modelo_rl_both %>% 
                mutate(across(where(is.double), ~ round(.x, 3))), 'csvs_redacao/indicadores_modelo_rl_both.csv', delim = ';')


## Arvore de decisao

modelo_arvore <- rpart(flg_churn ~ ., # -county
                       data = tc_train,
                       control = rpart.control(cp = 0))

printcp(modelo_arvore)
plotcp(modelo_arvore)

modelo_arvore_podada <- prune(modelo_arvore,
                              cp = modelo_arvore$cptable[which.min(modelo_arvore$cptable[,"xerror"]), "CP"])

printcp(modelo_arvore_podada)
plotcp(modelo_arvore_podada)

mean(tc_test$flg_churn == predict(modelo_arvore_podada, tc_test, type = "class"))

indicadores_arvore <- seq(0.1, 0.9, 0.05) %>%
    map_dfr(~ get_indicadores_modelo(modelo_arvore_podada, tc_test, cutoff = .x, descricao = 'Árvore de decisão'))


write_delim(indicadores_arvore %>% 
                mutate(across(where(is.double), ~ round(.x, 3))), 'csvs_redacao/indicadores_arvore_decisao.csv', ';')


to_plot_indicadores_arvore <- indicadores_arvore %>% 
    pivot_longer(c(`Acurácia`, Sensitividade, Especificidade), names_to = 'nome_indicador', values_to = 'indicador')

ggplot(to_plot_indicadores_arvore,
       aes(`Ponto de corte`, indicador, color = nome_indicador)) +
    geom_line() +
    geom_point() +
    scale_color_viridis_d() +
    labs(y = NULL, color = 'Indicador')

ggsave("plots/indicadores_arvore_decisao.png", width = 9, height = 5)

tmp_regras_arvore <- rpart.rules(modelo_arvore_podada, cover = T)

colnames(tmp_regras_arvore) <- c('flg_churn', paste0('V', 2:ncol(tmp_regras_arvore)))

regras_arvore <- tmp_regras_arvore %>% 
    unite('regra', -c(flg_churn, last_col()), sep = ' ', remove = TRUE, na.rm = TRUE) %>% 
    mutate(regra = str_squish(regra)) %>% 
    rename(cover = last_col())

write_delim(regras_arvore, 'csvs_redacao/regras_arvore_decisao.csv', ';')

rpart.plot(modelo_arvore, type = 3, clip.right.labs = FALSE, branch = .3, under = TRUE)

## Random forest
set.seed(3)
modelo_rf_sem_county <- randomForest(flg_churn ~ . -county,
                                     data = tc_train,
                                     ntree = 1000,
                                     importance = TRUE)

plot(modelo_rf_sem_county)

indicadores_rf_sem_county <- seq(0.1, 0.9, 0.05) %>%
    map_dfr(~ get_indicadores_modelo(modelo_rf_sem_county, tc_test, cutoff = .x, descricao = 'Random forest sem county'))

to_plot_indicadores_rf_sem_county <- indicadores_rf_sem_county %>% 
    pivot_longer(c(`Acurácia`, Sensitividade, Especificidade), names_to = 'nome_indicador', values_to = 'indicador')

ggplot(to_plot_indicadores_rf_sem_county,
       aes(`Ponto de corte`, indicador, color = nome_indicador)) +
    geom_line() +
    geom_point() +
    scale_color_viridis_d() +
    labs(y = NULL, color = 'Indicador')

ggsave("plots/indicadores_rf_sem_county.png", width = 9, height = 5)

write_delim(indicadores_rf_sem_county %>% 
                mutate(across(where(is.double), ~ round(.x, 3))), 'csvs_redacao/indicadores_rf_sem_county.csv', delim = ';')

# sqrt(ncol(select(tc_train, -county)))

set.seed(3)
modelo_rf_dummy <- randomForest(flg_churn ~ .,
                                data = rf_dummies_tc_train,
                                ntree = 1000,
                                importance = TRUE)

indicadores_rf_dummy <- seq(0.1, 0.9, 0.05) %>%
    map_dfr(~ get_indicadores_modelo(modelo_rf_dummy, rf_dummies_tc_test, cutoff = .x, descricao = 'Random forest dummy county'))

to_plot_indicadores_rf_dummy <- indicadores_rf_dummy %>% 
    pivot_longer(c(`Acurácia`, Sensitividade, Especificidade), names_to = 'nome_indicador', values_to = 'indicador')

ggplot(to_plot_indicadores_rf_dummy,
       aes(`Ponto de corte`, indicador, color = nome_indicador)) +
    geom_line() +
    geom_point() +
    scale_color_viridis_d() +
    labs(y = NULL, color = 'Indicador')

ggsave("plots/indicadores_rf_dummy_county.png", width = 9, height = 5)

write_delim(indicadores_rf_dummy %>% 
                mutate(across(where(is.double), ~ round(.x, 3))), 'csvs_redacao/indicadores_rf_dummy_county.csv', delim = ';')


library(caret)
tc_train_rf_caret <- tc_train %>%
    mutate(across(starts_with('flg_'), ~ as.factor(ifelse(.x == '1', 'Sim', 'Não'))))
# mutate(across(flg_churn, ~ as.factor(ifelse(.x == '1', 'Sim', 'Não')), .names = "new_{.col}"))

tc_test_rf_caret <- tc_test %>%
    mutate(across(starts_with('flg_'), ~ as.factor(ifelse(.x == '1', 'Sim', 'Não'))))


# table(tc_train_rf_caret$flg_churn, tc_train_rf_caret$new_flg_churn)
set.seed(3)
modelo_rf_caret <- caret::train(flg_churn ~ .,
                                data = tc_train_rf_caret,
                                method = 'rf',
                                trControl = trainControl(method = 'cv',
                                                         number = 10,
                                                         classProbs = TRUE,
                                                         summaryFunction = twoClassSummary),
                                na.action = na.omit,
                                metric = 'ROC')


indicadores_rf_caret <- seq(0.1, 0.9, 0.05) %>%
    map_dfr(~ get_indicadores_modelo(modelo_rf_caret, tc_test_rf_caret, cutoff = .x, descricao = 'Random forest com validação cruzada'))

to_plot_indicadores_rf_caret <- indicadores_rf_caret %>% 
    pivot_longer(c(`Acurácia`, Sensitividade, Especificidade), names_to = 'nome_indicador', values_to = 'indicador')

ggplot(to_plot_indicadores_rf_caret,
       aes(`Ponto de corte`, indicador, color = nome_indicador)) +
    geom_line() +
    geom_point() +
    scale_color_viridis_d() +
    labs(y = NULL, color = 'Indicador')


ggsave("plots/indicadores_rf_com_validacao_cruzada.png", width = 9, height = 5)

write_delim(indicadores_rf_caret %>% 
                mutate(across(where(is.double), ~ round(.x, 3))), 'csvs_redacao/indicadores_rf_com_validacao_cruzada.csv', delim = ';')


## Regressao logistica multinivel

# efeitos aleatorios para testar
# calcular tambem coeficiente de correlacao intra-classe (icc)
efeitos_aleatorios <- select(tc_scaled, !where(is.double) & !flg_churn) %>% colnames()

# Coeficientes de correlacao intraclasse
ccis <- tibble()

for(ea in efeitos_aleatorios) {
    print(ea)
    
    glmm_m0 <- glmer(as.formula(glue("flg_churn ~ 1 + (1 | {ea})")),
                     data = tc_scaled,
                     family = binomial)
    
    cci <- performance::icc(glmm_m0)
    
    tmp_ccis <- cci %>%
        as.data.frame() %>% 
        mutate(efeito_aleatorio = ea)
    
    if(ncol(tmp_ccis) == 4) {
        ccis <- rbind(ccis, tmp_ccis)
    }
}

todos_ccis <- ccis %>% 
    rename(icc_ajustado = ICC_adjusted) %>% 
    mutate(icc_ajustado = round(icc_ajustado, 3)) %>% 
    select(efeito_aleatorio, icc_ajustado) %>% 
    arrange(desc(icc_ajustado))

write_delim(todos_ccis, 'csvs_redacao/todos_ccis.csv', delim = ';')


get_dados_dummizados_para_multinivel <- function(efeito_aleatorio) {
    variaveis_dummizar <- tc_scaled %>%
        select(!where(is.double)) %>%
        select(-matches('flg_')) %>%
        select(-any_of(c(efeito_aleatorio))) %>%
        colnames()
    
    tc_scaled_dummies <- dummy_columns(.data = tc_scaled,
                                       select_columns = variaveis_dummizar,
                                       remove_selected_columns = TRUE,
                                       remove_first_dummy = TRUE) %>%
        rename_with(~ str_replace_all(.x, ' ', '_'), matches(' '))
    
    set.seed(3)
    train_idx <- createDataPartition(tc_scaled_dummies$flg_churn, p = .7, list = F)
    tc_scaled_dummies_train <- tc_scaled_dummies[train_idx,]
    tc_scaled_dummies_test <- tc_scaled_dummies[-train_idx,]
    
    return(list(tc_scaled_dummies_train = tc_scaled_dummies_train,
                tc_scaled_dummies_test = tc_scaled_dummies_test))
}

## flg_internet_service
rlm_flg_internet_service <- glmer(flg_churn ~ 1 + satisfaction_score_4 + satisfaction_score_5 + satisfaction_score_3 + flg_online_security + tx_concentracao_cobranca_mes_q3 + number_of_referrals + flg_married + number_of_dependents + contract_Two_Year + internet_type_Fiber_Optic + qtd_streamings + contract_One_Year + age + offer_Offer_E + offer_Offer_A + county_El_Dorado_County + flg_device_protection_plan + cltv + flg_multiple_lines + total_charges + valor_cobranca_geral + county_Colusa_County + flg_phone_service + condado_indice_gini_desigualdade_renda + county_Plumas_County + county_Lassen_County + county_Monterey_County + county_Shasta_County + condado_renda_familiar_mediana + condado_idade_mediana_habitantes + county_Trinity_County + county_Tuolumne_County + condado_tx_habitantes_homens + county_Ventura_County + county_Solano_County + condado_tx_habitantes_menor_18_anos + county_Santa_Cruz_County + county_Inyo_County + condado_densidade_populacional + county_Contra_Costa_County + county_Napa_County + county_Santa_Barbara_County + county_Stanislaus_County + county_Glenn_County + qtd_servicos_adicionais + county_Mariposa_County + county_San_Benito_County + county_Calaveras_County + county_Humboldt_County + county_Tehama_County + county_Imperial_County + county_Modoc_County + county_Yolo_County + county_Siskiyou_County + county_Santa_Clara_County + county_Madera_County + county_Marin_County + county_Orange_County + county_Los_Angeles_County + county_Kern_County + county_San_Francisco_County + condado_area_terra_m2 + (1 | flg_internet_service),
                                  data = get_dados_dummizados_para_multinivel('flg_internet_service')$tc_scaled_dummies_train,
                                  family = binomial,
                                  nAGQ = 0)

AIC(rlm_flg_internet_service)

indicadores_rlm_flg_internet_service <- seq(0.1, 0.9, 0.05) %>%
    map_dfr(~ get_indicadores_modelo(rlm_flg_internet_service,
                                     get_dados_dummizados_para_multinivel('flg_internet_service')$tc_scaled_dummies_test,
                                     cutoff = .x,
                                     "Regressão logística multinível stepwise, tendo 'flg_internet_service' como efeito aleatório")) %>% 
    mutate(AIC = AIC(rlm_flg_internet_service),
           qtd_iteracoes = 10146,
           formula_modelo = "flg_churn ~ 1 + satisfaction_score_4 + satisfaction_score_5 + satisfaction_score_3 + flg_online_security + tx_concentracao_cobranca_mes_q3 + number_of_referrals + flg_married + number_of_dependents + contract_Two_Year + internet_type_Fiber_Optic + qtd_streamings + contract_One_Year + age + offer_Offer_E + offer_Offer_A + county_El_Dorado_County + flg_device_protection_plan + cltv + flg_multiple_lines + total_charges + valor_cobranca_geral + county_Colusa_County + flg_phone_service + condado_indice_gini_desigualdade_renda + county_Plumas_County + county_Lassen_County + county_Monterey_County + county_Shasta_County + condado_renda_familiar_mediana + condado_idade_mediana_habitantes + county_Trinity_County + county_Tuolumne_County + condado_tx_habitantes_homens + county_Ventura_County + county_Solano_County + condado_tx_habitantes_menor_18_anos + county_Santa_Cruz_County + county_Inyo_County + condado_densidade_populacional + county_Contra_Costa_County + county_Napa_County + county_Santa_Barbara_County + county_Stanislaus_County + county_Glenn_County + qtd_servicos_adicionais + county_Mariposa_County + county_San_Benito_County + county_Calaveras_County + county_Humboldt_County + county_Tehama_County + county_Imperial_County + county_Modoc_County + county_Yolo_County + county_Siskiyou_County + county_Santa_Clara_County + county_Madera_County + county_Marin_County + county_Orange_County + county_Los_Angeles_County + county_Kern_County + county_San_Francisco_County + condado_area_terra_m2 + (1 | flg_internet_service)")

to_plot_indicadores_rlm_flg_internet_service <- indicadores_rlm_flg_internet_service %>% 
    pivot_longer(c(`Acurácia`, Sensitividade, Especificidade), names_to = 'nome_indicador', values_to = 'indicador')

ggplot(to_plot_indicadores_rlm_flg_internet_service,
       aes(`Ponto de corte`, indicador, color = nome_indicador)) +
    geom_line() +
    geom_point() +
    scale_color_viridis_d(option = 'D') +
    labs(y = NULL, color = 'Indicador')



## contract
rlm_contract <- glmer(flg_churn ~ 1 + satisfaction_score_4 + satisfaction_score_5 + satisfaction_score_3 + flg_online_security + number_of_referrals + monthly_charge + tx_concentracao_cobranca_mes_q3 + flg_married + number_of_dependents + county_San_Diego_County + flg_premium_tech_support + offer_Offer_E + county_Mendocino_County + county_Lake_County + offer_Offer_A + age + county_Nevada_County + tx_contrib_cobrancas_extras_cobranca_geral + county_Fresno_County + flg_streaming_music + avg_monthly_long_distance_charges + internet_type_Fiber_Optic + qtd_streamings + county_Colusa_County + valor_cobranca_geral + county_Lassen_County + cltv + internet_type_None + qtd_servicos_principais + county_Santa_Barbara_County + county_Mariposa_County + county_Tehama_County + qtd_servicos_adicionais + county_Modoc_County + county_Siskiyou_County + county_Calaveras_County + condado_idade_mediana_habitantes + (1 | contract),
                      data = get_dados_dummizados_para_multinivel('contract')$tc_scaled_dummies_train,
                      family = binomial,
                      nAGQ = 0)

AIC(rlm_contract)

indicadores_rlm_contract <- seq(0.1, 0.9, 0.05) %>%
    map_dfr(~ get_indicadores_modelo(rlm_contract,
                                     get_dados_dummizados_para_multinivel('contract')$tc_scaled_dummies_test,
                                     cutoff = .x,
                                     "Regressão logística multinível stepwise, tendo 'contract' como efeito aleatório")) %>% 
    mutate(AIC = AIC(rlm_contract),
           qtd_iteracoes = 17759,
           formula_modelo = "flg_churn ~ 1 + satisfaction_score_4 + satisfaction_score_5 + satisfaction_score_3 + flg_online_security + number_of_referrals + monthly_charge + tx_concentracao_cobranca_mes_q3 + flg_married + number_of_dependents + county_San_Diego_County + flg_premium_tech_support + offer_Offer_E + county_Mendocino_County + county_Lake_County + offer_Offer_A + age + county_Nevada_County + tx_contrib_cobrancas_extras_cobranca_geral + county_Fresno_County + flg_streaming_music + avg_monthly_long_distance_charges + internet_type_Fiber_Optic + qtd_streamings + county_Colusa_County + valor_cobranca_geral + county_Lassen_County + cltv + internet_type_None + qtd_servicos_principais + county_Santa_Barbara_County + county_Mariposa_County + county_Tehama_County + qtd_servicos_adicionais + county_Modoc_County + county_Siskiyou_County + county_Calaveras_County + condado_idade_mediana_habitantes + (1 | contract)")

to_plot_indicadores_rlm_contract <- indicadores_rlm_contract %>% 
    pivot_longer(c(`Acurácia`, Sensitividade, Especificidade), names_to = 'nome_indicador', values_to = 'indicador')

ggplot(to_plot_indicadores_rlm_contract,
       aes(`Ponto de corte`, indicador, color = nome_indicador)) +
    geom_line() +
    geom_point() +
    scale_color_viridis_d(option = 'D') +
    labs(y = NULL, color = 'Indicador')


## offer
rlm_offer <- glmer(flg_churn ~ 1 + satisfaction_score_4 + satisfaction_score_5 + satisfaction_score_3 + flg_online_security + internet_type_None + number_of_referrals + tx_concentracao_cobranca_mes_q3 + flg_married + number_of_dependents + contract_Two_Year + monthly_charge + county_San_Diego_County + flg_premium_tech_support + qtd_servicos_principais + contract_One_Year + county_Mendocino_County + county_Lake_County + county_Nevada_County + age + county_Tulare_County + county_Fresno_County + tx_contrib_cobrancas_extras_cobranca_geral + county_El_Dorado_County + internet_type_Fiber_Optic + flg_online_backup + county_San_Mateo_County + cltv + county_Colusa_County + avg_monthly_long_distance_charges + county_Lassen_County + condado_renda_familiar_mediana + county_Shasta_County + county_Plumas_County + condado_idade_mediana_habitantes + county_Trinity_County + county_Tuolumne_County + valor_cobranca_geral + total_charges + flg_streaming_tv + flg_streaming_movies + flg_streaming_music + county_Monterey_County + county_Santa_Barbara_County + (1 | offer),
                   data = get_dados_dummizados_para_multinivel('offer')$tc_scaled_dummies_train,
                   family = binomial,
                   nAGQ = 0)

AIC(rlm_offer)
summary(rlm_offer)

indicadores_rlm_offer <- seq(0.1, 0.9, 0.05) %>%
    map_dfr(~ get_indicadores_modelo(rlm_offer,
                                     get_dados_dummizados_para_multinivel('offer')$tc_scaled_dummies_test,
                                     cutoff = .x,
                                     "Regressão logística multinível stepwise, tendo 'offer' como efeito aleatório")) %>%
    mutate(AIC = AIC(rlm_offer),
           qtd_iteracoes = 9833,
           formula_modelo = "flg_churn ~ 1 + satisfaction_score_4 + satisfaction_score_5 + satisfaction_score_3 + flg_online_security + internet_type_None + number_of_referrals + tx_concentracao_cobranca_mes_q3 + flg_married + number_of_dependents + contract_Two_Year + monthly_charge + county_San_Diego_County + flg_premium_tech_support + qtd_servicos_principais + contract_One_Year + county_Mendocino_County + county_Lake_County + county_Nevada_County + age + county_Tulare_County + county_Fresno_County + tx_contrib_cobrancas_extras_cobranca_geral + county_El_Dorado_County + internet_type_Fiber_Optic + flg_online_backup + county_San_Mateo_County + cltv + county_Colusa_County + avg_monthly_long_distance_charges + county_Lassen_County + condado_renda_familiar_mediana + county_Shasta_County + county_Plumas_County + condado_idade_mediana_habitantes + county_Trinity_County + county_Tuolumne_County + valor_cobranca_geral + total_charges + flg_streaming_tv + flg_streaming_movies + flg_streaming_music + county_Monterey_County + county_Santa_Barbara_County + (1 | offer)")

to_plot_indicadores_rlm_offer <- indicadores_rlm_offer %>% 
    pivot_longer(c(`Acurácia`, Sensitividade, Especificidade), names_to = 'nome_indicador', values_to = 'indicador')

ggplot(to_plot_indicadores_rlm_offer,
       aes(`Ponto de corte`, indicador, color = nome_indicador)) +
    geom_line() +
    geom_point() +
    scale_color_viridis_d(option = 'D') +
    labs(y = NULL, color = 'Indicador')


## satisfaction_score
rlm_satisfaction_score <- glmer(flg_churn ~ 1 + flg_online_security + number_of_referrals + monthly_charge + tx_concentracao_cobranca_mes_q3 + contract_Two_Year + flg_married + number_of_dependents + flg_premium_tech_support + flg_phone_service + contract_One_Year + offer_Offer_E + county_Mendocino_County + offer_Offer_A + age + tx_contrib_cobrancas_extras_cobranca_geral + county_El_Dorado_County + county_Tulare_County + total_charges + county_San_Mateo_County + flg_streaming_music + avg_monthly_long_distance_charges + internet_type_Fiber_Optic + flg_online_backup + qtd_streamings + county_Colusa_County + county_Lassen_County + cltv + county_Shasta_County + condado_renda_familiar_mediana + county_Ventura_County + county_Santa_Cruz_County + condado_idade_mediana_habitantes + county_Plumas_County + county_Trinity_County + county_Tuolumne_County + condado_indice_gini_desigualdade_renda + county_Contra_Costa_County + county_Santa_Clara_County + total_long_distance_charges + condado_densidade_populacional + (1 | satisfaction_score),
                                data = get_dados_dummizados_para_multinivel('satisfaction_score')$tc_scaled_dummies_train,
                                family = binomial,
                                nAGQ = 0)

indicadores_rlm_satisfaction_score <- seq(0.1, 0.9, 0.05) %>%
    map_dfr(~ get_indicadores_modelo(rlm_satisfaction_score,
                                     get_dados_dummizados_para_multinivel('satisfaction_score')$tc_scaled_dummies_test,
                                     cutoff = .x,
                                     "Regressão logística multinível stepwise, tendo 'satisfaction_score' como efeito aleatório")) %>%
    mutate(AIC = AIC(rlm_satisfaction_score),
           qtd_iteracoes = 10783,
           formula_modelo = "flg_churn ~ 1 + flg_online_security + number_of_referrals + monthly_charge + tx_concentracao_cobranca_mes_q3 + contract_Two_Year + flg_married + number_of_dependents + flg_premium_tech_support + flg_phone_service + contract_One_Year + offer_Offer_E + county_Mendocino_County + offer_Offer_A + age + tx_contrib_cobrancas_extras_cobranca_geral + county_El_Dorado_County + county_Tulare_County + total_charges + county_San_Mateo_County + flg_streaming_music + avg_monthly_long_distance_charges + internet_type_Fiber_Optic + flg_online_backup + qtd_streamings + county_Colusa_County + county_Lassen_County + cltv + county_Shasta_County + condado_renda_familiar_mediana + county_Ventura_County + county_Santa_Cruz_County + condado_idade_mediana_habitantes + county_Plumas_County + county_Trinity_County + county_Tuolumne_County + condado_indice_gini_desigualdade_renda + county_Contra_Costa_County + county_Santa_Clara_County + total_long_distance_charges + condado_densidade_populacional + (1 | satisfaction_score)")

to_plot_indicadores_rlm_satisfaction_score <- indicadores_rlm_satisfaction_score %>% 
    pivot_longer(c(`Acurácia`, Sensitividade, Especificidade), names_to = 'nome_indicador', values_to = 'indicador')

ggplot(to_plot_indicadores_rlm_satisfaction_score,
       aes(`Ponto de corte`, indicador, color = nome_indicador)) +
    geom_line() +
    geom_point() +
    scale_color_viridis_d(option = 'D') +
    labs(y = NULL, color = 'Indicador')


resumo_modelos_rlm <- rbind(indicadores_rlm_flg_internet_service,
                            indicadores_rlm_contract,
                            indicadores_rlm_offer,
                            indicadores_rlm_satisfaction_score) %>% 
    select(descricao, qtd_iteracoes, AIC, AUC, formula_modelo) %>% 
    distinct() %>% 
    mutate(qtd_efeitos_fixos = str_count(str_remove(formula_modelo, "flg_churn [~] 1 [+] "), "[+]")) %>% 
    relocate(qtd_efeitos_fixos, .after = AUC) %>% 
    arrange(AIC)

write_delim(resumo_modelos_rlm %>% 
                mutate(across(where(is.double), ~ round(.x, 3))), 'csvs_redacao/resumo_modelos_rlm.csv', delim = ';')

tabela_efeitos_fixos_aleatorios <- resumo_modelos_rlm %>% 
    select(descricao, formula_modelo) %>% 
    transmute(efeito_aleatorio = str_extract(descricao, "(?<=')[a-z_]+"),
              efeito_fixo = str_remove_all(formula_modelo, "flg_churn [~] 1 [+]| [+] [(]1.+")) %>% 
    separate_rows(efeito_fixo, sep = '[+]') %>% 
    mutate(across(everything(), str_squish)) %>% 
    distinct() %>% 
    mutate(flg_presente = 1) %>% 
    pivot_wider(names_from = efeito_aleatorio, values_from = flg_presente, values_fill = 0) %>% 
    mutate(qtd_modelos = offer + contract + satisfaction_score + flg_internet_service) %>% 
    arrange(desc(qtd_modelos), desc(offer))

write_delim(tabela_efeitos_fixos_aleatorios,
            'csvs_redacao/tabela_efeitos_fixos_aleatorios.csv', delim = ';')

## Todos os modelos
indicadores_diversos_modelos <- rbind(select(indicadores_modelo_rl_both, -c(AIC)),
                                      indicadores_arvore,
                                      indicadores_rf_sem_county,
                                      indicadores_rf_dummy,
                                      indicadores_rf_caret,
                                      select(indicadores_rlm_offer, -(AIC:last_col())))

to_plot_indicadores_diversos_modelos <- indicadores_diversos_modelos %>% 
    pivot_longer(c(`Acurácia`, Sensitividade, Especificidade), names_to = 'nome_indicador', values_to = 'indicador')

ggplot(to_plot_indicadores_diversos_modelos,
       aes(`Ponto de corte`, indicador, color = descricao)) +
    geom_line() +
    # geom_point(aes(shape = descricao)) +
    geom_point() +
    # scale_color_viridis_d(option = 'D') +
    scale_color_viridis_d(option = 'C') +
    # scale_color_brewer(palette = 'Set2') +
    facet_wrap(~ nome_indicador) +
    labs(y = NULL, color = 'Modelo') +
    theme(legend.position = 'bottom')

ggsave("plots/indicadores_diversos_modelos.png", width = 12, height = 6)

auc_modelos <- indicadores_diversos_modelos %>% 
    select(descricao, AUC) %>% 
    distinct() %>% 
    arrange(desc(AUC)) %>% 
    mutate(across(where(is.double), ~ round(.x, 4)))

write_delim(auc_modelos, 'csvs_redacao/auc_diversos_modelos.csv', delim = ';')

