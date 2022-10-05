#' (block) bootstrap the result forecasting table to compute metrics
#
#' @param df result table containing obs and forecast
#' @param model the model name
#' @param res resolution
#' @param block_length = NULL by default which corresponds to the i.i.d case, otherwise specify the block length, and samples of such length will be selected
#' @param n_samples = 250 by default, the number of samples to produce
#' @param target = "DP" by default, the target variable (DP or IP)
#'
#' @return



# Results -----

source("1. Sample Code/99_Utility_v3.R")

data_list = prep()

datMxLearn = data_list[[1]] # low-resolution
datMxTest = data_list[[2]] # low-resolution
DataLearn = data_list[[3]] # high-resolution
DataTest = data_list[[4]] # high-resolution

filter_test = "2015-07-01"

#### DP ####

## Persistence Baseline

DP_pred_persistence = datMxTest %>%
  mutate(pred = datMxTest$peak24) %>%
  filter(ymd >= filter_test)%>%
  mutate(persistence = pred)

DP_persistence = metrics_boot_DP(DP_pred_persistence,
                               model='Persistence', res='NA',
                               block_length = 7)

## High resolution
# ARIMA
load("1. Sample Code/2. Pred Signals/0. ARIMA/hr_arima.RData")
test_and_pred = DataTest %>%
  mutate(pred = HR_arima[[1]]) %>%
  select(ymd,load,pred) %>%
  group_by(ymd) %>%
  summarise_all(max) %>%
  ungroup(.) %>%
  mutate(ym = paste(year(ymd),"-",month(ymd), sep="")) %>% 
  filter(ymd >= filter_test) %>%
  left_join(.,DP_pred_persistence %>% select(ymd,persistence),by="ymd")
DP_HR_arima = metrics_boot_DP(test_and_pred,
                               model='HR-Arima', res='HR',
                               block_length = 7)

# Gauss
load("1. Sample Code/2. Pred Signals/1. GAMs/high_res_rolling_bam.RData")
test_and_pred = DataTest %>%
  mutate(pred = high_res_pred[[1]]) %>%
  select(ymd,load,pred) %>%
  group_by(ymd) %>%
  summarise_all(max) %>%
  ungroup(.) %>%
  mutate(ym = paste(year(ymd),"-",month(ymd), sep="")) %>% 
  filter(ymd >= filter_test)%>%
  left_join(.,DP_pred_persistence %>% select(ymd,persistence),by="ymd")

DP_HR_gauss = metrics_boot_DP(test_and_pred,
                               model='HR-Gauss', res='HR',
                               block_length = 7)

# FCNN
HRFCNN = as.matrix(read.csv("1. Sample Code/2. Pred Signals/2. NNs/1. FC/high-resolution_FC_rolling_pred.csv",header=F))
test_and_pred = DataTest %>%
  mutate(pred = HRFCNN) %>%
  select(ymd,load,pred) %>%
  group_by(ymd) %>%
  summarise_all(max) %>%
  ungroup(.) %>%
  mutate(ym = paste(year(ymd),"-",month(ymd), sep="")) %>% 
  filter(ymd >= filter_test)%>%
  left_join(.,DP_pred_persistence %>% select(ymd,persistence),by="ymd")

DP_HR_FCNN = metrics_boot_DP(test_and_pred,
                                  model='HR-FCNN', res='HR',
                                  block_length = 7)

##### Low resolution
# ARIMA
load("1. Sample Code/2. Pred Signals/0. ARIMA/lr_arima.RData")
test_and_pred = datMxTest %>% mutate(pred = LR_arima[[1]]) %>% 
  filter(ymd >= filter_test)%>%
  left_join(.,DP_pred_persistence %>% select(ymd,persistence),by="ymd")
DP_LR_arima = metrics_boot_DP(test_and_pred,
                                  model='LR-arima', res='LR',
                                  block_length = 7)

# FCNN
LRFCNN = as.matrix(read.csv("1. Sample Code/2. Pred Signals/2. NNs/1. FC/low-resolution_FC_rolling_pred.csv",header=F))
test_and_pred = datMxTest %>% mutate(pred = LRFCNN) %>% 
  filter(ymd >= filter_test)%>%
  left_join(.,DP_pred_persistence %>% select(ymd,persistence),by="ymd")
DP_LR_FCNN = metrics_boot_DP(test_and_pred,
                                   model='LR-FCNN', res='LR',
                                   block_length = 7)

# Gauss
load("1. Sample Code/2. Pred Signals/1. GAMs/gauss_rolling_pred_more_knots_factor.RData")
test_and_pred = datMxTest %>% mutate(pred = gauss_rolling_pred[[1]]) %>% 
  filter(ymd >= filter_test)%>%
  left_join(.,DP_pred_persistence %>% select(ymd,persistence),by="ymd")
DP_LR_gauss = metrics_boot_DP(test_and_pred,
                                  model='LR-gauss', res='LR',
                                  block_length = 7)

# Scat
load("1. Sample Code/2. Pred Signals/1. GAMs/scat_rolling_pred_more_knots_factor.RData")
test_and_pred = datMxTest %>% mutate(pred = scat_rolling_pred[[1]]) %>% 
  filter(ymd >= filter_test)%>%
  left_join(.,DP_pred_persistence %>% select(ymd,persistence),by="ymd")
DP_LR_scat = metrics_boot_DP(test_and_pred,
                                   model='LR-scat', res='LR',
                                   block_length = 7)

# Gev
load("1. Sample Code/2. Pred Signals/1. GAMs/gev_rolling_pred_more_knots_factor.RData")
test_and_pred = datMxTest %>% mutate(pred = gev_rolling_pred[[1]]) %>% 
  filter(ymd >= filter_test)%>%
  left_join(.,DP_pred_persistence %>% select(ymd,persistence),by="ymd")
DP_LR_gev = metrics_boot_DP(test_and_pred,
                                  model='LR-gev', res='LR',
                                  block_length = 7)
##### Multi-resolution

# CNN
# MRCNN = apply(bind_cols(lapply(list.files(path = "1. Sample Code/2. Pred Signals/2. NNs/2. Hybrid/2. Without Trend/", pattern="*.csv", full.names=T), read.delim, header=F)),
#               1,
#               mean)

MRCNN=as.matrix(read.csv("1. Sample Code/2. Pred Signals/2. NNs/2. Hybrid/multi-resolution_CNN_rolling_pred_1.csv",header=F))
test_and_pred = datMxTest %>% mutate(pred = MRCNN) %>% 
  filter(ymd >= filter_test)%>%
  left_join(.,DP_pred_persistence %>% select(ymd,persistence),by="ymd")
DP_MR_CNN = metrics_boot_DP(test_and_pred,
                                 model='MR-CNN', res='MR',
                                 block_length = 7)

# Gauss
load("1. Sample Code/2. Pred Signals/1. GAMs/MR_gauss_rolling_pred_more_knots_factor.RData")
test_and_pred = datMxTest %>% mutate(pred = MR_rolling_pred[[1]]) %>% 
  filter(ymd >= filter_test)%>%
  left_join(.,DP_pred_persistence %>% select(ymd,persistence),by="ymd")
DP_MR_gauss = metrics_boot_DP(test_and_pred,
                                 model='MR-gauss', res='MR',
                                 block_length = 7)

# Scat
load("1. Sample Code/2. Pred Signals/1. GAMs/MR_scat_rolling_pred_factor.RData")
test_and_pred = datMxTest %>% mutate(pred = MR_scat_rolling_pred[[1]]) %>% 
  filter(ymd >= filter_test)%>%
  left_join(.,DP_pred_persistence %>% select(ymd,persistence),by="ymd")
DP_MR_scat = metrics_boot_DP(test_and_pred,
                                 model='MR-scat', res='MR',
                                 block_length = 7)

# Gev
load("1. Sample Code/2. Pred Signals/1. GAMs/MR_rolling_pred_more_knots_factor.RData")
test_and_pred = datMxTest %>% mutate(pred = MR_rolling_pred[[1]]) %>% 
  filter(ymd >= filter_test)%>%
  left_join(.,DP_pred_persistence %>% select(ymd,persistence),by="ymd")
DP_MR_gev = metrics_boot_DP(test_and_pred,
                                 model='MR-gev', res='MR',
                                 block_length = 7)

##### Boxplots
all_DP = list(DP_persistence,
           DP_HR_gauss,DP_HR_arima,DP_HR_FCNN,
           DP_LR_scat,DP_LR_gauss,DP_LR_gev,DP_LR_arima,DP_LR_FCNN,
           DP_MR_scat,DP_MR_gauss,DP_MR_gev,DP_MR_CNN)

pdf(file = "1. Sample Code/3. Plots/DP-block-bootstrap.pdf", width = 11, height = 8)
box_func_DP(all_DP)
dev.off()

#### IP ####

IP_pred_persistence=datMxTest %>%
  mutate(pred = datMxTest$tod24 + 1) %>%
  filter(ymd >= filter_test) %>%
  mutate(persistence=pred)%>%
  add_load_pred(.)

IP_persistence = metrics_boot_IP(IP_pred_persistence,
                                 model='Persistence', res='NA',
                                 block_length = 7)

## Gaussian
load("1. Sample Code/2. Pred Signals/1. GAMs/high_res_rolling_bam.RData")
test_and_pred = high_res_to_peak(DataTest, high_res_pred[[1]]) %>% 
                filter(ymd >= filter_test) %>%
  left_join(.,IP_pred_persistence %>% select(ymd,persistence),by="ymd") %>%
  add_load_pred(.)

IP_HR_gauss = metrics_boot_IP(test_and_pred,
                              model='HR-gauss', res='HR',
                              block_length = 7)

# FCNN
HRFCNN = as.vector(as.matrix(read.csv("1. Sample Code/2. Pred Signals/2. NNs/1. FC/high-resolution_FC_rolling_pred.csv",header=F)))
test_and_pred = high_res_to_peak(DataTest, HRFCNN) %>% 
  filter(ymd >= filter_test)%>%
  left_join(.,IP_pred_persistence %>% select(ymd,persistence),by="ymd")%>%
  add_load_pred(.)
IP_HR_FCNN = metrics_boot_IP(test_and_pred,
                            model='HR-FCNN', res='HR',
                            block_length = 7)
#### Low-resolution ####

# Ocat
load("1. Sample Code/2. Pred Signals/1. GAMs/low_resolution_ocat.RData")
test_and_pred = datMxTest %>%
  mutate(pred = lr_ocat_rolling_pred[[1]]) %>% 
  filter(ymd >= filter_test)%>%
  left_join(.,IP_pred_persistence %>% select(ymd,persistence),by="ymd")%>%
  add_load_pred(.)
IP_LR_ocat = metrics_boot_IP(test_and_pred,
                          model='LR-ocat', res='LR',
                          block_length = 7)

# FCNN
LRFCNN = to_classes(round(as.matrix(read.csv("1. Sample Code/2. Pred Signals/2. NNs/1. FC/low-resolution_FC_rolling_pred_Instant.csv",header=F))))
test_and_pred = datMxTest %>% mutate(pred = LRFCNN) %>% 
  filter(ymd >= filter_test)%>%
  left_join(.,IP_pred_persistence %>% select(ymd,persistence),by="ymd")%>%
  add_load_pred(.)
IP_LR_FCNN = metrics_boot_IP(test_and_pred,
                          model='LR-FCNN', res='LR',
                          block_length = 7)

#### Multi-resolution ####

# Ocat
load("1. Sample Code/2. Pred Signals/1. GAMs/ocat_rolling_pred_more_knots.RData")
test_and_pred = datMxTest %>%
  mutate(pred = ocat_rolling_pred[[1]]) %>% 
  filter(ymd >= filter_test)%>%
  left_join(.,IP_pred_persistence %>% select(ymd,persistence),by="ymd")%>%
  add_load_pred(.)
IP_MR_ocat = metrics_boot_IP(test_and_pred,
                          model='MR-ocat', res='MR',
                          block_length = 7)

# MRCNN
MRCNN = to_classes(round(as.matrix(read.csv("1. Sample Code/2. Pred Signals/2. NNs/2. Hybrid/multi-resolution_CNN_rolling_pred_Instant_1.csv",header=F))))
test_and_pred = datMxTest %>% mutate(pred = MRCNN) %>% 
  filter(ymd >= filter_test)%>%
  left_join(.,IP_pred_persistence %>% select(ymd,persistence),by="ymd")%>%
  add_load_pred(.)
IP_MR_CNN = metrics_boot_IP(test_and_pred,
                         model='MR-CNN', res='MR',
                         block_length = 7)

##### Boxplots
all_IP = list(IP_persistence,
              IP_HR_gauss,IP_HR_FCNN,
              IP_LR_ocat,IP_LR_FCNN,
              IP_MR_ocat,IP_MR_CNN)

pdf(file = "1. Sample Code/3. Plots/IP-block-bootstrap.pdf", width = 11, height = 8)
box_func_IP(all_IP)
dev.off()

pdf(file = "1. Sample Code/3. Plots/legend-block-bootstrap.pdf", width = 11, height = 4)
export_legend(box_func_DP(all_DP,legend="bottom"))
dev.off()

# END

