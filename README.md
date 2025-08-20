rm(list = ls());gc()
# 设置工作路径
setwd("C:/Users/Yuki/Desktop/DDL/SII+NSCLC/clin/clin/clin")
pacman::p_load(moonBook, Hmisc, dplyr, furniture,writexl,openxlsx,mice)
#mydata <- read.csv("CLIN1.csv")
mydata <- read.xlsx("train_data.xlsx")
#mydata <- read.xlsx("test_data.xlsx")
colSums(is.na(mydata))
#str(mydata)
# 设置随机种子确保可重复
#set.seed(123)
# 按7:3比例划分训练集和验证集
#train_index <- sample(1:nrow(mydata), size = 0.7 * nrow(mydata))
#train_data <- mydata[train_index, ]
#test_data  <- mydata[-train_index, ]

# 输出到 xlsx 文件
#write.xlsx(train_data, "train_data.xlsx", rowNames = FALSE)
#write.xlsx(test_data, "test_data.xlsx", rowNames = FALSE)
#mydata <- test_data
#write.xlsx(mydata, "test_nomogram.xlsx", rowNames = FALSE)
# 连续变量
convars <- c("PRE_NEU", "PRE_LYM", "PRE_PLT", "PRE_MON", "PRE_ALB", "POST_NEU", 
             "POST_LYM", "POST_PLT", "POST_MON", "POST_ALB","Age", "MONTH","in_hospital_days","BMI"
             ,"clean_node_number","pdl1")
catvars <- c("Gender", "Tumor_location", "Lobe_number", "his", "cTNM",  
             "T_stage","Smoke","surgery_type","Pleural_Invasion","Neural_Invasion","Vascular_Invasion"
             , "node_metastatic")
mydata[convars] <- lapply(mydata[convars], as.numeric)
# 将分类变量转换为因子类型
mydata$Gender <- factor(mydata$Gender, levels = c(0, 1), labels = c("Female", "Male"))
mydata$Tumor_location <- factor(mydata$Tumor_location, levels = c(1, 2), labels = c("Left", "Right"))
mydata$Lobe_number <- factor(mydata$Lobe_number, levels = c("M", "S"), labels = c("Multiple", "Single"))
mydata$cTNM <- factor(mydata$cTNM, levels = c(1, 2, 3), labels = c("Clinical I", "Clinical II", "Clinical III"))
mydata$T_stage <- factor(ifelse(mydata$T_stage %in% c(3, 4), "T3+", mydata$T_stage),
                         levels = c(1, 2, "T3+"),
                         labels = c("T1", "T2", "T3+"))
mydata$his <- as.character(mydata$his)
mydata$his <- ifelse(mydata$his %in% c("2", "3"), "2", mydata$his)
mydata$his <- factor(mydata$his, levels = c("1", "2"), labels = c("Adenocarcinoma", "Non-Adenocarcinoma"))
mydata$Smoke <- factor(mydata$Smoke, levels = c(0, 1), labels = c("No", "Yes"))
mydata$Pleural_Invasion <- factor(mydata$Pleural_Invasion, levels = c(0, 1), labels = c("No", "Yes"))
mydata$Neural_Invasion <- factor(mydata$Neural_Invasion, levels = c(0, 1), labels = c("No", "Yes"))
mydata$Vascular_Invasion <- factor(mydata$Vascular_Invasion, levels = c(0, 1), labels = c("No", "Yes"))
mydata$surgery_type <- factor(mydata$surgery_type, levels = c(0, 1, 2), labels = c("Wedge Resection", "Lobectomy", "Thoracotomy"))

# 将node_metastatic转换为分类变量
mydata$node_metastatic <- factor(mydata$node_metastatic, levels = c(0, 1, 2), labels = c("No Lymph Node Metastasis", "Single-Station Metastasis", "Multi-Station Metastasis"))
# 设置变量标签
label(mydata$PRE_NEU) <- "Preoperative Neutrophil"
label(mydata$PRE_LYM) <- "Preoperative Lymphocyte"
label(mydata$PRE_PLT) <- "Preoperative Platelet"
label(mydata$PRE_MON) <- "Preoperative Monocyte"
label(mydata$PRE_ALB) <- "Preoperative Albumin"
label(mydata$POST_NEU) <- "Postoperative Neutrophil"
label(mydata$POST_LYM) <- "Postoperative Lymphocyte"
label(mydata$POST_PLT) <- "Postoperative Platelet"
label(mydata$POST_MON) <- "Postoperative Monocyte"
label(mydata$POST_ALB) <- "Postoperative Albumin"
label(mydata$Age) <- "Age"
label(mydata$MONTH) <- "Month"
label(mydata$Gender) <- "Gender"
label(mydata$Tumor_location) <- "Tumor Location"
label(mydata$Lobe_number) <- "Lobe Number"
label(mydata$his) <- "Histology"
label(mydata$cTNM) <- "cTNM Stage"
label(mydata$T_stage) <- "T Stage"
label(mydata$Smoke) <- "Smoking History"
label(mydata$surgery_type) <- "Surgery Type"
label(mydata$Pleural_Invasion) <- "Pleural Invasion"
label(mydata$Neural_Invasion) <- "Neural Invasion"
label(mydata$Vascular_Invasion) <- "Vascular Invasion"
label(mydata$BMI) <- "BMI"
label(mydata$clean_node_number) <- "Clean Node Number"
label(mydata$node_metastatic) <- "Lymph Node Metastasis"
label(mydata$pdl1) <- "PD-L1 expression"
label(mydata$Event) <- "Event"
label(mydata$in_hospital_days) <- "In Hospital Days"
str(mydata)
####基线表格####
csv.out <- moonBook::mytable(
  Event ~ Gender + Age + + Smoke + BMI + surgery_type + clean_node_number + Tumor_location + Lobe_number + his + cTNM + T_stage 
  + node_metastatic + Pleural_Invasion + 
    Neural_Invasion + Vascular_Invasion  + pdl1+
    PRE_NEU + PRE_LYM + PRE_PLT + PRE_MON + PRE_ALB + PRE_NLR + PRE_PLR + PRE_SII + PRE_SIRI + PRE_PIV,
  data = mydata,
  use.labels = FALSE,  # 显示标签而非变量名
  digits = 1,          # 连续变量保留1位小数
  method = 2,          # 连续变量用均值±标准差
  catMethod = 0,       # 分类变量用频数（百分比）
  show.total = TRUE    # 显示总计列
)
# 输出表格
print(csv.out)
# 导出CSV到指定文件夹
#moonBook::mycsv(csv.out, file = "Table1_Baseline_total.csv")

#####炎症指标部分####
# 构建 Δ 值
mydata$NLR <- mydata$POST_NEU / mydata$POST_LYM - mydata$PRE_NEU / mydata$PRE_LYM
mydata$PLR <- mydata$POST_PLT / mydata$POST_LYM - mydata$PRE_PLT / mydata$PRE_LYM
mydata$SII <- (mydata$POST_NEU * mydata$POST_PLT / mydata$POST_LYM) - 
  (mydata$PRE_NEU * mydata$PRE_PLT / mydata$PRE_LYM)
mydata$SIRI <- (mydata$POST_NEU * mydata$POST_MON / mydata$POST_LYM) - 
  (mydata$PRE_NEU * mydata$PRE_MON / mydata$PRE_LYM)
mydata$PIV <- (mydata$POST_NEU * mydata$POST_PLT * mydata$POST_MON / mydata$POST_LYM) -
  (mydata$PRE_NEU * mydata$PRE_PLT * mydata$PRE_MON / mydata$PRE_LYM)

# 构建术前术后指标
mydata$PRE_NLR <- mydata$PRE_NEU / mydata$PRE_LYM
mydata$PRE_PLR <- mydata$PRE_PLT / mydata$PRE_LYM
mydata$PRE_SII <- mydata$PRE_NEU * mydata$PRE_PLT / mydata$PRE_LYM
mydata$PRE_SIRI <- mydata$PRE_NEU * mydata$PRE_MON / mydata$PRE_LYM
mydata$PRE_PIV <- mydata$PRE_NEU * mydata$PRE_PLT * mydata$PRE_MON / mydata$PRE_LYM

mydata$POST_NLR <- mydata$POST_NEU / mydata$POST_LYM
mydata$POST_PLR <- mydata$POST_PLT / mydata$POST_LYM
mydata$POST_SII <- mydata$POST_NEU * mydata$POST_PLT / mydata$POST_LYM
mydata$POST_SIRI <- mydata$POST_NEU * mydata$POST_MON / mydata$POST_LYM
mydata$POST_PIV <- mydata$POST_NEU * mydata$POST_PLT * mydata$POST_MON / mydata$POST_LYM

# 设置 label（可选）
# 术前指标
label(mydata$PRE_NLR) <- "Preoperative NLR"
label(mydata$PRE_PLR) <- "Preoperative PLR"
label(mydata$PRE_SII) <- "Preoperative SII"
label(mydata$PRE_SIRI) <- "Preoperative SIRI"
label(mydata$PRE_PIV) <- "Preoperative PIV"

# 术后指标
label(mydata$POST_NLR) <- "Postoperative NLR"
label(mydata$POST_PLR) <- "Postoperative PLR"
label(mydata$POST_SII) <- "Postoperative SII"
label(mydata$POST_SIRI) <- "Postoperative SIRI"
label(mydata$POST_PIV) <- "Postoperative PIV"

# 变化值 Δ
label(mydata$NLR) <- "ΔNLR"
label(mydata$PLR) <- "ΔPLR"
label(mydata$SII) <- "ΔSII"
label(mydata$SIRI) <- "ΔSIRI"
label(mydata$PIV) <- "ΔPIV"


# 输出 Table 1B
table1B <- moonBook::mytable(
  Event ~ + POST_NEU + POST_LYM + 
    POST_PLT + POST_MON + POST_ALB +
    POST_NLR + POST_PLR + POST_SII + POST_SIRI + POST_PIV +
    NLR + PLR + SII + SIRI + PIV,
  data = mydata,
  use.labels = TRUE,      # 使用变量标签作为展示列名
  digits = 1,
  method = 1,             # 均值±标准差
  catMethod = 0,
  show.total = TRUE
)
print(table1B)

# 可导出到 Excel
# moonBook::mycsv(table1B, file = "POST_baseline.csv")
####训练vs验证####
# 加载必要包
# 读取数据
library(readxl)
library(dplyr)
library(tableone)

# 读取 Excel 文件
mydata <- read_excel("243.xlsx")

# 标记 Cohort 类型
mydata$Cohort <- ifelse(mydata$Data == "train", "Training cohort", "Validation cohort")

# 指定变量
vars <- c("Gender", "Age", "Smoke", "BMI", "surgery_type", "clean_node_number", 
          "Tumor_location", "Lobe_number", "his", "cTNM", "T_stage", 
          "node_metastatic", "Pleural_Invasion", "Neural_Invasion", 
          "Vascular_Invasion", "pdl1", "PRE_NEU", "PRE_LYM", "PRE_PLT", "PRE_MON", 
          "PRE_ALB", "PRE_NLR", "PRE_PLR", "PRE_SII", "PRE_SIRI", "PRE_PIV")

# 指定分类变量（若有遗漏可补充）
catVars <- c("Gender", "Smoke", "surgery_type", "Tumor_location", "Lobe_number", 
             "his", "cTNM", "T_stage", "node_metastatic", "Pleural_Invasion", 
             "Neural_Invasion", "Vascular_Invasion")

# 创建 TableOne
tab <- CreateTableOne(vars = vars, strata = "Cohort", data = mydata, factorVars = catVars)

# 导出表格（包括 p 值）
print(tab, showAllLevels = TRUE, varLabels = TRUE, labelTranslations = var_label(mydata), test = TRUE)
# 输出为CSV文件
write.csv(print(tab, showAllLevels = TRUE, varLabels = TRUE, labelTranslations = var_label(mydata), test = TRUE), "baseline_comparison.csv")



####循环TIMEROC####
# 加载必要的包
library(timeROC)
library(survival)
library(ggplot2)
library(writexl)
library(cowplot)
library(rms)
library(survival)
library(rms)  # 提供 validate.cph 等函数

# 定义需要计算的变量名
predictors <- c("PRE_PLR", "PRE_NLR", "PRE_SII", "PRE_SIRI", "PRE_PIV",
                "POST_PLR", "POST_NLR", "POST_SII", "POST_SIRI", "POST_PIV",
                "PLR", "NLR", "SII", "SIRI", "PIV")
#predictors <- c("POST_SII","POST_PIV","SII","PIV")
# 在结果表中加入C-index列
auc_results <- data.frame(
  Predictor = character(),
  AUC_1yr = numeric(),
  CI_lower_1yr = numeric(),
  CI_upper_1yr = numeric(),
  P_Value_1yr = numeric(),
  Optimal_Cutoff_1yr = numeric(),
  AUC_3yr = numeric(),
  CI_lower_3yr = numeric(),
  CI_upper_3yr = numeric(),
  P_Value_3yr = numeric(),
  Optimal_Cutoff_3yr = numeric(),
  AUC_5yr = numeric(),
  CI_lower_5yr = numeric(),
  CI_upper_5yr = numeric(),
  P_Value_5yr = numeric(),
  Optimal_Cutoff_5yr = numeric(),
  Cindex = numeric(),      # 新增 C-index
  stringsAsFactors = FALSE
)
# 定义 find_optimal_cutoff 函数
find_optimal_cutoff <- function(TP, FP, marker) {
  sensitivity <- TP / max(TP)
  specificity <- 1 - FP / max(FP)
  youden_index <- sensitivity + specificity - 1
  cutoff_index <- which.max(youden_index)
  return(marker[cutoff_index])
}
# 计算每个指标的 AUC 和 C-index
for (pred in predictors) {
  time_roc_res <- timeROC(T = mydata$MONTH, 
                          delta = mydata$Event, 
                          marker = mydata[[pred]], 
                          cause = 1, 
                          weighting = "marginal", 
                          times = c(12, 36, 59.9), 
                          iid = TRUE)
  
  # 计算最佳截断值
  optimal_cutoff_1yr <- find_optimal_cutoff(time_roc_res$TP[, 1], time_roc_res$FP[, 1], mydata[[pred]])
  optimal_cutoff_3yr <- find_optimal_cutoff(time_roc_res$TP[, 2], time_roc_res$FP[, 2], mydata[[pred]])
  optimal_cutoff_5yr <- find_optimal_cutoff(time_roc_res$TP[, 3], time_roc_res$FP[, 3], mydata[[pred]])
  
  # AUC 和 P值
  cox_model <- coxph(Surv(MONTH, Event) ~ mydata[[pred]], data = mydata)
  cox_summary <- summary(cox_model)
  
  # C-index
  c_index <- cox_summary$concordance[1]
  
  # 将结果存储
  auc_results <- rbind(auc_results, data.frame(
    Predictor = pred,
    AUC_1yr = time_roc_res$AUC[1],
    CI_lower_1yr = confint(time_roc_res)$CI_AUC[1,1] / 100,
    CI_upper_1yr = confint(time_roc_res)$CI_AUC[1,2] / 100,
    P_Value_1yr = cox_summary$coefficients[5],
    Optimal_Cutoff_1yr = optimal_cutoff_1yr,
    AUC_3yr = time_roc_res$AUC[2],
    CI_lower_3yr = confint(time_roc_res)$CI_AUC[2,1] / 100,
    CI_upper_3yr = confint(time_roc_res)$CI_AUC[2,2] / 100,
    P_Value_3yr = cox_summary$coefficients[5],
    Optimal_Cutoff_3yr = optimal_cutoff_3yr,
    AUC_5yr = time_roc_res$AUC[3],
    CI_lower_5yr = confint(time_roc_res)$CI_AUC[3,1] / 100,
    CI_upper_5yr = confint(time_roc_res)$CI_AUC[3,2] / 100,
    P_Value_5yr = cox_summary$coefficients[5],
    Optimal_Cutoff_5yr = optimal_cutoff_5yr,
    Cindex = c_index
  ))
}

# 输出结果到 Excel
# write_xlsx(auc_results, "auc_timeroc0801.xlsx")

plots <- list()  # 创建一个列表，用于存储每个指标的ROC曲线图
for (pred in predictors) {
  time_roc_res <- timeROC(
    T = mydata$MONTH,
    delta = mydata$Event,
    marker = mydata[[pred]],
    cause = 1,
    weighting = "marginal",
    times = c(12, 36, 59.9),
    iid = TRUE
  )
  
  time_ROC_df <- data.frame(
    TP_1year = time_roc_res$TP[, 1],
    FP_1year = time_roc_res$FP[, 1],
    TP_3year = time_roc_res$TP[, 2],
    FP_3year = time_roc_res$FP[, 2],
    TP_5year = time_roc_res$TP[, 3],
    FP_5year = time_roc_res$FP[, 3]
  )
  
  # 获取变量标签作为图标题
  pred_label <- sjlabelled::get_label(mydata[[pred]])
  if (is.null(pred_label) || pred_label == "") pred_label <- pred  # fallback
  
  # 绘图
  p <- ggplot(data = time_ROC_df) +
    geom_line(aes(x = FP_1year, y = TP_1year), size = 1, color = "#BC3C29FF") +
    geom_line(aes(x = FP_3year, y = TP_3year), size = 1, color = "#0072B5FF") +
    geom_line(aes(x = FP_5year, y = TP_5year), size = 1, color = "#E18727FF") +
    geom_abline(slope = 1, intercept = 0, color = "grey", size = 1, linetype = 2) +
    theme_bw() +
    theme(
      text = element_text(family = "serif"),
      axis.text = element_text(face = "bold", size = 11, color = "black"),
      axis.title.x = element_text(face = "bold", size = 14, color = "black", margin = margin(c(15, 0, 0, 0))),
      axis.title.y = element_text(face = "bold", size = 14, color = "black", margin = margin(c(0, 15, 0, 0))),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    ) +
    labs(
      x = "False positive rate",
      y = "True positive rate",
      title = pred_label
    ) +
    geom_text(
      x = 0.75, y = 0.25,
      label = paste0("1 year = ", sprintf("%.3f", auc_results$AUC_1yr[which(auc_results$Predictor == pred)])),
      color = "#BC3C29FF", size = 4.5
    ) +
    geom_text(
      x = 0.75, y = 0.15,
      label = paste0("3 years = ", sprintf("%.3f", auc_results$AUC_3yr[which(auc_results$Predictor == pred)])),
      color = "#0072B5FF", size = 4.5
    ) +
    geom_text(
      x = 0.75, y = 0.05,
      label = paste0("5 years = ", sprintf("%.3f", auc_results$AUC_5yr[which(auc_results$Predictor == pred)])),
      color = "#E18727FF", size = 4.5
    )
  
  plots[[pred]] <- p
}


# 合并多个 ROC 曲线结果到一张图中
combined_plot <- cowplot::plot_grid(plotlist = plots,labels = NULL, nrow = 1)
combined_plot
ggsave("timeROC_inside_legend.tif", combined_plot, width = 16, height = 4.5, dpi = 300)
####OVERALL ROC####
library(pROC)
library(survival)

# 创建一个空数据框，用于存储结果
auc_results <- data.frame(
  Predictor = predictors,
  AUC = numeric(length(predictors)),
  CI_lower = numeric(length(predictors)),
  CI_upper = numeric(length(predictors)),
  Optimal_Cutoff = numeric(length(predictors)),
  P_Value = numeric(length(predictors)),  # P值
  Cindex = numeric(length(predictors))    # 新增C-index
)

# 创建一个空列表，用于存储每个指标的 ROC 对象
roc_objects <- vector("list", length(predictors))

# 使用循环批量计算 AUC、最佳截断值、置信区间和C-index
for (i in seq_along(predictors)) {
  pred <- predictors[i]
  formula <- as.formula(paste0("Event ~ ", pred))
  
  # 计算ROC
  res <- roc(formula, data = mydata, ci = TRUE, boot.n = 1000)  # bootstrap计算置信区间
  auc_results$AUC[i] <- res$auc
  auc_results$CI_lower[i] <- res$ci[1]
  auc_results$CI_upper[i] <- res$ci[2]
  auc_results$Optimal_Cutoff[i] <- as.numeric(coords(res, "best", ret = "threshold"))
  
  # 计算P值（与随机预测比较）
  auc_results$P_Value[i] <- roc.test(res, roc(mydata$Event, runif(length(mydata$Event))))$p.value
  
  # 计算C-index
  cox_model <- coxph(Surv(MONTH, Event) ~ mydata[[pred]], data = mydata)
  auc_results$Cindex[i] <- summary(cox_model)$concordance[1]
  
  roc_objects[[i]] <- res
}

# 美化P值
auc_results$P_Value <- ifelse(auc_results$P_Value < 0.001, "<0.001", round(auc_results$P_Value, 3))

# 打印结果
print(auc_results)

# 导出 AUC+CI+Cindex 结果到 Excel 文件
# write_xlsx(auc_results, "auc_ALL0801.xlsx")

# 创建一个主题函数，设置所有文本元素的字体为罗马字体
theme_times_new_roman <- function() {
  theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, family = "serif", size = 16, face = "bold"),
      axis.title.x = element_text(family = "serif", size = 14, face = "bold", color = "black", margin = margin(c(15, 0, 0, 0))),
      axis.title.y = element_text(family = "serif", size = 14, face = "bold", color = "black", margin = margin(c(0, 15, 0, 0))),
      axis.text.x = element_text(family = "serif", size = 11, face = "bold", color = "black"),
      axis.text.y = element_text(family = "serif", size = 11, face = "bold", color = "black"),
      legend.text = element_text(family = "serif", size = 10),
      legend.title = element_text(family = "serif", size = 10),
      plot.caption = element_text(family = "serif", size = 10)
    )
}

# 绘制 ROC 图
auc_x <- 0.6
auc_y <- 0.3
cutoff_x <- 0.6
cutoff_y <- 0.2

# 循环绘制 ROC 图
plots <- list()
library(sjlabelled)
for (i in seq_along(predictors)) {
  p <- ggroc(roc_objects[[i]], legacy.axes = TRUE, color = "#BC3C29FF") +
    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color = "darkgrey", linetype = 4) +
    theme_times_new_roman() +
    ggtitle(sjlabelled::get_label(mydata[[predictors[i]]])) +  # 自动提取标签
    labs(x = "False Positive Rate", y = "True Positive Rate") +
    annotate("text", x = auc_x, y = auc_y, label = paste0("AUC = ", round(auc_results$AUC[i], 3))) +
    annotate("text", x = cutoff_x, y = cutoff_y, label = paste0("Cut-off = ", round(auc_results$Optimal_Cutoff[i], 3))) +
    theme(plot.title = element_text(hjust = 0.5))
  
  plots[[i]] <- p
}

# 合并多个 ROC 曲线结果到一张图中
combined_plot <- cowplot::plot_grid(plotlist = plots, labels = NULL, nrow = 1)
print(combined_plot)
ggsave("OVERROC.TIF", combined_plot, width = 16, height = 4.5, dpi = 300)
str(mydata)
####循环survival####
# 加载必要的包
library(survival)
library(survminer)
library(cowplot)
library(writexl)
library(labelled)
# 定义需要计算的变量名
predictors <- c("PRE_NEU", "PRE_LYM", "PRE_PLT", "PRE_MON", "PRE_ALB", 
                "POST_NEU", "POST_LYM", "POST_PLT", "POST_MON", "POST_ALB",
                "PRE_PLR", "PRE_NLR", "PRE_SII", "PRE_SIRI", "PRE_PIV",
                "POST_PLR", "POST_NLR", "POST_SII", "POST_SIRI", "POST_PIV",
                "PLR", "NLR", "SII", "SIRI", "PIV")

predictors <- c("POST_SII", "POST_PIV", "SII", "PIV")
titles <- c("Postoperative SII", "Postoperative PIV", "ΔSII", "ΔPIV")
surv_plots <- list()

# 循环绘图
for (i in seq_along(predictors)) {
  pred <- predictors[i]
  cutoff <- auc_results$Optimal_Cutoff[i]
  group_var <- paste0(pred, "_Group")
  
  # 构造分组变量，Low/High
  mydata[[group_var]] <- factor(ifelse(mydata[[pred]] >= cutoff, "High", "Low"),
                                levels = c("Low", "High"))
  
  # 用 get() 提取 factor 向量，避免 formula 报错
  surv_fit <- survfit(Surv(MONTH, Event == 1) ~ get(group_var), data = mydata)
  
  # 画图
  p <- ggsurvplot(
    surv_fit,
    data = mydata,
    title = titles[i],
    risk.table = FALSE,
    pval = TRUE,
    conf.int = TRUE,
    conf.int.alpha = 0.3,
    palette = c("#61AACFFF", "#DA9599FF"),
    legend.labs = c("Low Group", "High Group"),
    legend.title = " ",
    xlab = "Time (months)",
    ylab = "Survival Probability",
    ggtheme = theme_bw(base_family = "serif") +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11)
      )
  )
  
  surv_plots[[i]] <- p
}

                                
# 合并多个生存曲线结果到一张图中
combined_surv_plot <- cowplot::plot_grid(plotlist = lapply(surv_plots, function(x) x$plot), labels = NULL, nrow = 1)
print(combined_surv_plot)
ggsave("SURVIVAL.TIF", combined_surv_plot, width = 16, height = 4.5, dpi = 300)
####UNICOX####
# 遍历 auc_results 中每个 predictor，根据其 cutoff 创建新列
for(i in 1:nrow(auc_results)) {
  varname <- auc_results$Predictor[i]                     # 如 POST_PIV
  cutoff <- as.numeric(auc_results$Optimal_Cutoff[i])     # 如 1009.99
  newvar <- paste0(varname, "_Group")                     # 新列名，如 POST_PIV_Group
  
  # 如果变量存在于数据中，就创建分组列
  if (varname %in% colnames(mydata)) {
    mydata[[newvar]] <- ifelse(mydata[[varname]] > cutoff, "High", "Low")
  } else {
    cat("❗ Warning: ", varname, " not found in mydata!\n")
  }
}

# 将所有新生成的 _Group 变量转换为因子类型，便于后续Cox/K-M分析
group_vars <- grep("_Group$", names(mydata), value = TRUE)
mydata[group_vars] <- lapply(mydata[group_vars], factor)
#####unicox####
# 检查所有变量类型是否正确（特别是Group变量应为factor）
str(mydata)
# 检查是否存在NA
library(survival)
# 定义生存对象（以总生存OS为例）
surv_obj <- Surv(time = mydata$MONTH, event = mydata$Event)
# 可供分析的变量列表
vars_for_cox <- c(
  # 临床变量
  "Gender", "Age","BMI", "Smoke","surgery_type", "clean_node_number",
  "Tumor_location", "Lobe_number", "his", 
  "cTNM", "T_stage","node_metastatic",  "Pleural_Invasion", "Neural_Invasion", 
  "Vascular_Invasion", "pdl1","PRE_ALB","POST_ALB",
  
  # 炎症指标分组变量
  "PRE_PLR_Group", "PRE_NLR_Group", "PRE_SII_Group", "PRE_SIRI_Group", "PRE_PIV_Group",
  "POST_PLR_Group", "POST_NLR_Group", "POST_SII_Group", "POST_SIRI_Group", "POST_PIV_Group",
  "PLR_Group", "NLR_Group", "SII_Group", "SIRI_Group", "PIV_Group"
)


# 初始化结果表
univ_results <- data.frame(Variable = character(),
                           Level = character(),
                           HR = numeric(),
                           CI_lower = numeric(),
                           CI_upper = numeric(),
                           P_value = numeric(),
                           stringsAsFactors = FALSE)
# 提取所有 _Group 结尾的变量名
group_vars <- grep("_Group$", names(mydata), value = TRUE)

# 批量设置参考水平为 "Low"
for (var in group_vars) {
  if (is.factor(mydata[[var]])) {
    mydata[[var]] <- relevel(mydata[[var]], ref = "Low")
  }
}

for (var in vars_for_cox) {
  formula <- as.formula(paste("surv_obj ~", var))
  model <- coxph(formula, data = mydata)
  summary_model <- summary(model)
  
  # 提取变量名称、各水平HR
  coefs <- summary_model$coefficients
  confints <- summary_model$conf.int
  
  for (i in 1:nrow(coefs)) {
    level_name <- rownames(coefs)[i]
    hr <- coefs[i, "exp(coef)"]
    ci_low <- confints[i, "lower .95"]
    ci_up <- confints[i, "upper .95"]
    pval <- coefs[i, "Pr(>|z|)"]
    
    univ_results <- rbind(univ_results, data.frame(
      Variable = var,
      Level = level_name,
      HR = hr,
      CI_lower = ci_low,
      CI_upper = ci_up,
      P_value = pval
    ))
  }
}

# 查看结果
#univ_results <- univ_results[order(univ_results$P_value), ]
print(univ_results)
write.xlsx(univ_results, "univ_results0801.xlsx", rowNames = FALSE)


####multicox####
library(survival)
library(openxlsx)

# 基础变量
clinical_vars <- c("Gender", "Age", "Smoke", "his", "cTNM", "T_stage", "node_metastatic", "pdl1", "PRE_ALB")
inflammation_vars <- c(
  "PRE_PLR_Group", "PRE_NLR_Group", "PRE_SII_Group", "PRE_SIRI_Group", "PRE_PIV_Group",
  "POST_PLR_Group", "POST_NLR_Group", "POST_SII_Group", "POST_SIRI_Group", "POST_PIV_Group",
  "PLR_Group", "NLR_Group", "SII_Group", "SIRI_Group", "PIV_Group"
)

# 创建工作簿
wb <- createWorkbook()
summary_results <- data.frame()  # 初始化汇总数据框

# 遍历每个炎症变量
for (var in inflammation_vars) {
  # 确保该变量为 factor 且 Low 为 reference level
  mydata[[var]] <- relevel(factor(mydata[[var]]), ref = "Low")
  
  # 构建模型公式
  formula_text <- paste("Surv(MONTH, Event) ~", paste(c(clinical_vars, var), collapse = " + "))
  formula_obj <- as.formula(formula_text)
  
  # 拟合模型
  model <- coxph(formula_obj, data = mydata)
  summary_model <- summary(model)
  
  # 提取所有变量
  result_df <- data.frame(
    Variable = rownames(summary_model$coefficients),
    HR = round(summary_model$coefficients[, "exp(coef)"], 3),
    CI_lower = round(summary_model$conf.int[, "lower .95"], 3),
    CI_upper = round(summary_model$conf.int[, "upper .95"], 3),
    P_value = signif(summary_model$coefficients[, "Pr(>|z|)"], 3),
    row.names = NULL
  )
  
  # 保存当前模型所有变量
  sheet_name <- gsub("_GroupHigh|_Group", "", var)
  addWorksheet(wb, sheetName = sheet_name)
  writeData(wb, sheet = sheet_name, result_df)
  
  # 提取目标炎症变量行（带 High），标准化名称为原变量名
  selected_row <- result_df[grepl(paste0("^", var, "High$"), result_df$Variable), ]
  if (nrow(selected_row) == 0) {
    selected_row <- result_df[grepl(paste0("^", var), result_df$Variable), ]
  }
  selected_row$Variable <- var  # 标准化变量名
  summary_results <- rbind(summary_results, selected_row)
}

# 添加汇总 sheet
addWorksheet(wb, sheetName = "Summary")
writeData(wb, sheet = "Summary", summary_results)

# 保存工作簿
#saveWorkbook(wb, file = "multivariable_cox_with_summary_refLow.xlsx", overwrite = TRUE)
#####AUCINDEX####
# 加载必要包
library(survival)
library(timeROC)
library(survcomp)
library(writexl)
library(dplyr)

# 设置基础变量和炎症指标
clinical_vars <- c("Gender", "Age","Smoke","his", 
                   "cTNM", "T_stage","node_metastatic",  "pdl1","PRE_ALB")

inflammation_markers <- c(
  "PRE_PLR_Group", "PRE_NLR_Group", "PRE_SII_Group", "PRE_SIRI_Group", "PRE_PIV_Group",
  "POST_PLR_Group", "POST_NLR_Group", "POST_SII_Group", "POST_SIRI_Group", "POST_PIV_Group",
  "PLR_Group", "NLR_Group", "SII_Group", "SIRI_Group", "PIV_Group"
)

# 初始化结果表
results <- data.frame(
  Marker = character(),
  AUC_1yr = numeric(), P_1yr = numeric(),
  AUC_3yr = numeric(), P_3yr = numeric(),
  AUC_5yr = numeric(), P_5yr = numeric(),
  Cindex = numeric(), Cindex_p = numeric(),
  stringsAsFactors = FALSE
)

# 主循环
for (marker in inflammation_markers) {
  # 构建模型公式
  formula_str <- paste("Surv(MONTH, Event) ~", paste(c(clinical_vars, marker), collapse = " + "))
  model_formula <- as.formula(formula_str)
  
  # 拟合模型
  model <- coxph(model_formula, data = mydata)
  risk_score <- predict(model, type = "lp")
  
  # 计算 timeROC AUC 和 P 值
  times <- c(12, 36, 59.9)
  roc <- timeROC(
    T = mydata$MONTH,
    delta = mydata$Event,
    marker = risk_score,
    cause = 1,
    times = times,
    weighting = "marginal",
    iid = TRUE
  )
  
  auc_vals <- roc$AUC
  se_vals <- sqrt(roc$inference$vect_sd_1)
  z_vals <- (auc_vals - 0.5) / se_vals
  auc_pvals <- round(2 * pnorm(-abs(z_vals)), 4)
  
  # 计算 C-index 及其 P 值
  cindex_res <- concordance.index(
    x = risk_score,
    surv.time = mydata$MONTH,
    surv.event = mydata$Event,
    method = "noether"
  )
  
  # 汇总
  results <- rbind(results, data.frame(
    Marker = marker,
    AUC_1yr = round(auc_vals[1], 3), P_1yr = auc_pvals[1],
    AUC_3yr = round(auc_vals[2], 3), P_3yr = auc_pvals[2],
    AUC_5yr = round(auc_vals[3], 3), P_5yr = auc_pvals[3],
    Cindex = round(cindex_res$c.index, 3),
    Cindex_p = signif(cindex_res$p.value, 4)
  ))
}
print(results)
# 保存为 Excel 文件
write_xlsx(results, "Combined_Model_AUC_train.xlsx")

####LASSO####
library(survival)
library(glmnet)
library(dplyr)
ref_low_vars <- c("POST_SII_Group", "SII_Group", "POST_PIV_Group", "PIV_Group")
for (var in ref_low_vars) {
  mydata[[var]] <- relevel(as.factor(mydata[[var]]), ref = "Low")
}
# 设定临床变量与炎症指标
clinical_vars <- c("Gender", "Smoke", "cTNM", "T_stage", "his","node_metastatic", "pdl1", "PRE_ALB")
inflammation_vars <- c("POST_SII_Group", "SII_Group", "POST_PIV_Group","PIV_Group")

# 构建设计矩阵（去掉Intercept）
x <- model.matrix(
  as.formula(paste("~", paste(c(clinical_vars, inflammation_vars), collapse = " + "))),
  data = mydata
)[, -1]

# 构建生存对象
y <- Surv(mydata$MONTH, mydata$Event)

# 运行LASSO回归
cvfit <- cv.glmnet(x, y, family = "cox", alpha = 1)
lasso_fit <- glmnet(x, y, family = "cox", alpha = 1)

# 提取 lambda 值
lambda_min_val <- round(cvfit$lambda.min, 4)
lambda_1se_val <- round(cvfit$lambda.1se, 4)

# CV曲线
tiff("LASSO_CV_Curve.tiff", width = 2000, height = 1600, res = 300, compression = "lzw")
par(family = "serif")  # 罗马字体
plot(cvfit)
abline(v = log(cvfit$lambda.min), col = "#BC3C29FF", lty = 2)
abline(v = log(cvfit$lambda.1se), col = "#0072B5FF", lty = 2)
text(x = log(cvfit$lambda.min), y = min(cvfit$cvm)+0.75,
     labels = paste0("λ.min = ", lambda_min_val),
     pos = 4, col = "#BC3C29FF", cex = 0.8)
text(x = log(cvfit$lambda.1se), y = min(cvfit$cvm) + 0.55,
     labels = paste0("λ.1se = ", lambda_1se_val),
     pos = 4, col = "#0072B5FF", cex = 0.8)
legend("topright", legend = c("λ.min", "λ.1se"),
       col = c("#BC3C29FF", "#0072B5FF"), lty = 2, cex = 0.8)
dev.off()

# 系数路径图
lasso_fit <- glmnet(x, y, family = "cox", alpha = 1)
tiff("LASSO_Coefficient_Path.tiff", width = 2000, height = 1600, res = 300, compression = "lzw")
par(family = "serif")  # 罗马字体
plot(lasso_fit, xvar = "lambda", label = TRUE)
abline(v = log(cvfit$lambda.min), col = "#BC3C29FF", lty = 2)
abline(v = log(cvfit$lambda.1se), col = "#0072B5FF", lty = 2)
text(x = log(cvfit$lambda.min), y = -0.5,
     labels = paste0("λ.min = ", lambda_min_val),
     pos = 4, col = "#BC3C29FF", cex = 0.8)
text(x = log(cvfit$lambda.1se), y = -0.2,labels = paste0("λ.1se = ", lambda_1se_val),pos = 4, col = "#0072B5FF", cex = 0.8)
legend("topright", legend = c("λ.min", "λ.1se"),
       col = c("#BC3C29FF", "#0072B5FF"), lty = 2, cex = 0.8)
dev.off()

cat("LASSO 可视化图已输出为 TIFF 格式，并标注 λ.min 和 λ.1se。\n")


#保存LASSO选择变量
coef_min <- coef(cvfit, s = "lambda.min")
coef_1se <- coef(cvfit, s = "lambda.1se")

selected_min <- rownames(coef_min)[which(as.vector(coef_min) != 0)]
selected_1se <- rownames(coef_1se)[which(as.vector(coef_1se) != 0)]

print(selected_min)
print(selected_1se)

write.xlsx(data.frame(LASSO_Selected = selected_min), "LASSO_selected_lambda_min.xlsx", row.names = FALSE)
write.xlsx(data.frame(LASSO_Selected = selected_1se), "LASSO_selected_lambda_1se.xlsx", row.names = FALSE)

#逐步回归模型
# 可切换使用final_formula_min 或 final_formula_1se（建议用1se）
final_formula_1se <- as.formula(Surv(MONTH, Event) ~  Gender+Age+Smoke + cTNM+
                                  PRE_ALB + POST_SII_Group + POST_PIV_Group+PIV_Group)

lasso_cox <- coxph(final_formula_1se, data = mydata)
lasso_summary <- summary(lasso_cox)
print(lasso_summary)

initial_model <- coxph(final_formula_1se, data = mydata)
step_model <- step(initial_model, direction = "both")
summary_step <- summary(step_model)
print(summary_step)
# 提取并整理逐步回归结果表
step_result <- data.frame(
  Variable = rownames(summary_step$coefficients),
  Coef = summary_step$coefficients[, "coef"],
  HR = summary_step$coefficients[, "exp(coef)"],
  Lower_CI = summary_step$conf.int[, "lower .95"],
  Upper_CI = summary_step$conf.int[, "upper .95"],
  P_Value = summary_step$coefficients[, "Pr(>|z|)"]
)

write.xlsx(step_result, "stepwise_cox_result.xlsx", row.names = FALSE)
####multinomogram####
# 构建最终 Cox 模型（已知模型结构）
#cox_model <- coxph(Surv(MONTH, Event) ~ Smoke + cTNM +T_stage+POST_SII_Group + POST_NLR_Group + PIV_Group, data = mydata)
cox_model <- coxph(Surv(MONTH, Event) ~ Smoke + cTNM +
                     PRE_ALB + POST_SII_Group + POST_PIV_Group, data = mydata)
# 2. 提取回归结果
summary_cox <- summary(cox_model)
coef_info <- summary_cox$coefficients
conf_info <- summary_cox$conf.int
print(summary_cox)
# 3. 整理结果为数据框
cox_table <- data.frame(
  Variable = rownames(coef_info),
  HR       = round(conf_info[, "exp(coef)"], 3),
  `95%CI_lower` = round(conf_info[, "lower .95"], 3),
  `95%CI_upper` = round(conf_info[, "upper .95"], 3),
  `P_value`     = signif(coef_info[, "Pr(>|z|)"], 3)
)
print(cox_table)
# 可选：将变量名更美化（可略）
# cox_table$Variable <- gsub("_Group", "", cox_table$Variable)

# 4. 写入 Excel 文件
library(openxlsx)
#write.xlsx(cox_table, file = "Cox_model_result0802.xlsx", rowNames = FALSE)
####modeltimeroc####
library(survival)
library(timeROC)
library(openxlsx)

# 1. 拟合 Cox 模型
model_final <- coxph(Surv(MONTH, Event) ~ Smoke + cTNM +PRE_ALB+
                       POST_SII_Group + POST_PIV_Group,
                     data = mydata)

# 2. 获取线性预测评分
risk_score <- predict(model_final, type = "lp")

# 3. 设置时间点
times <- c(12, 36, 59.9)

# 4. 计算 timeROC（使用 iid = TRUE 以获得推断标准误）
roc_res <- timeROC(
  T = mydata$MONTH, delta = mydata$Event,
  marker = risk_score, cause = 1,
  times = times, iid = TRUE, weighting = "marginal"
)

# 5. 提取 AUC、SE、置信区间和 P 值
auc_vals <- roc_res$AUC
se_auc   <- roc_res$inference$vect_sd_1
ci_lower <- pmax(0, auc_vals - 1.96 * se_auc)
ci_upper <- pmin(1, auc_vals + 1.96 * se_auc)
z_auc    <- (auc_vals - 0.5) / se_auc
p_auc    <- round(2 * pnorm(-abs(z_auc)), 4)

# 6. 计算 C-index 与其 P 值
cs <- summary(model_final)$concordance
c_index    <- cs[1]
c_index_se <- cs[2]
ci_c_lo    <- pmax(0, c_index - 1.96 * c_index_se)
ci_c_hi    <- pmin(1, c_index + 1.96 * c_index_se)
z_c        <- (c_index - 0.5) / c_index_se
p_c        <- round(2 * pnorm(-abs(z_c)), 4)

# 7. 汇总所有结果
results <- data.frame(
  Time     = c(paste0(times, " months"), "C-index"),
  AUC      = c(round(auc_vals, 3), round(c_index, 3)),
  CI_lower = c(round(ci_lower, 3), round(ci_c_lo, 3)),
  CI_upper = c(round(ci_upper, 3), round(ci_c_hi, 3)),
  SE       = c(round(se_auc, 4), round(c_index_se, 4)),
  P_value  = c(p_auc, p_c)
)

# 8. 输出结果（可选）
print(results)

# 可选导出
# write.xlsx(results, "nomogram_AUC_test.xlsx", rowNames = FALSE)
#####3model####
library(survival)
library(timeROC)
library(openxlsx)
Nomogram = coxph(Surv(MONTH, Event) ~ cTNM + Smoke + PRE_ALB + POST_SII_Group + POST_PIV_Group, data = mydata)

# 定义模型列表
models <- list(
  TNM = coxph(Surv(MONTH, Event) ~ cTNM, data = mydata),
  Clinical = coxph(Surv(MONTH, Event) ~ cTNM + Smoke + PRE_ALB, data = mydata),
  Nomogram = coxph(Surv(MONTH, Event) ~ cTNM +  Smoke + PRE_ALB + POST_SII_Group + POST_PIV_Group, data = mydata)
)

# 设置时间点
times <- c(12, 36, 59.9)

# 初始化结果列表
results_list <- list()

for (model_name in names(models)) {
  model <- models[[model_name]]
  
  # 获取预测风险评分
  lp <- predict(model, type = "lp")
  
  # 计算 timeROC
  roc_res <- timeROC(
    T = mydata$MONTH, delta = mydata$Event,
    marker = lp, cause = 1,
    times = times, iid = TRUE, weighting = "marginal"
  )
  
  # 提取 AUC 与 CI/P 值
  auc_vals <- roc_res$AUC
  var_auc  <- roc_res$inference$vect_sd_1
  se_auc   <- sqrt(var_auc)
  ci_lower <- pmax(0, auc_vals - 1.96 * se_auc)
  ci_upper <- pmin(1, auc_vals + 1.96 * se_auc)
  z_auc    <- (auc_vals - 0.5) / se_auc
  p_auc    <- round(2 * pnorm(-abs(z_auc)), 4)
  
  # C-index 与 P 值
  cs <- summary(model)$concordance
  c_index    <- cs[1]
  c_index_se <- cs[2]
  ci_c_lo    <- pmax(0, c_index - 1.96 * c_index_se)
  ci_c_hi    <- pmin(1, c_index + 1.96 * c_index_se)
  z_c        <- (c_index - 0.5) / c_index_se
  p_c        <- round(2 * pnorm(-abs(z_c)), 4)
  
  # 汇总结果
  df <- data.frame(
    Model    = model_name,
    Time     = c(paste0(times, " months"), "C-index"),
    AUC      = c(round(auc_vals, 3), round(c_index, 3)),
    CI_lower = c(round(ci_lower, 3), round(ci_c_lo, 3)),
    CI_upper = c(round(ci_upper, 3), round(ci_c_hi, 3)),
    P_value  = c(p_auc, p_c)
  )
  
  results_list[[model_name]] <- df
}

# 合并结果
final_result <- do.call(rbind, results_list)
print(final_result)
# 导出 Excel 文件
write.xlsx(final_result, "Model_AUC_Cindex_Results_0801.xlsx", rowNames = FALSE)

####KM曲线#####
library(survival)
library(survminer)
library(ggplot2)

# 构建最终 Cox 模型（已知模型结构）
cox_model <- coxph(Surv(MONTH, Event) ~  Gender + Age + Smoke + cTNM +PRE_ALB + POST_SII_Group + POST_PIV_Group, data = mydata)

# 预测线性风险评分
mydata$risk_score <- predict(cox_model, type = "lp")

# 自动选择最佳cutoff值
cutpoint <- surv_cutpoint(
  data = mydata,
  time = "MONTH",
  event = "Event",
  variables = "risk_score"
)
cutoff_val <- cutpoint$cutpoint[1, "cutpoint"]
#cutoff_val <- 0.1901407 
cat("最佳截断值为：", cutoff_val, "\n")

# 根据cutoff值进行风险分组（以Low为参考）
mydata$risk_group <- factor(
  ifelse(mydata$risk_score > cutoff_val, "High risk", "Low risk"),
  levels = c("Low risk", "High risk")
)

# 构建生存对象
surv_obj <- Surv(time = mydata$MONTH, event = mydata$Event)

# KM生存拟合
fit <- survfit(surv_obj ~ risk_group, data = mydata)

# Cox模型提取HR
cox_group_model <- coxph(surv_obj ~ risk_group, data = mydata)
hr <- round(exp(coef(cox_group_model)), 2)

# 绘制 KM 图（不自动添加 p 值）
surv_plot <- ggsurvplot(
  fit,
  data = mydata,
  conf.int = TRUE,
  pval = FALSE,  # 禁用默认 p 值
  risk.table = TRUE,
  risk.table.height = 0.2,
  xlab = "Time (Months)",
  ylab = "Survival Probability",
  legend.title = "",
  legend.labs = c("Low risk", "High risk"),
  palette = c("#61AACFFF", "#DA9599FF"),
  ggtheme = theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid = element_blank(),
      axis.line = element_line(color = "black", size = 0.5),
      axis.ticks = element_line(color = "black", size = 0.5),
      axis.text = element_text(family = "serif", size = 12),
      axis.title = element_text(family = "serif", size = 14, face = "bold"),
      legend.text = element_text(family = "serif", size = 12),
      legend.title = element_text(family = "serif", size = 12, face = "bold")
    ),
  break.time.by = 12,
  surv.median.line = "hv"
)

# 添加 HR 和 P 值注释（使用 serif 字体）
surv_plot$plot <- surv_plot$plot +
  annotate("text", x = max(mydata$MONTH) * 0.1, y = 0.22,
           label = "p < 0.0001", size = 5, family = "serif") +
  annotate("text", x = max(mydata$MONTH) * 0.1, y = 0.12,
           label = paste("HR =", hr), size = 5, family = "serif")

# 输出图像
print(surv_plot)


# Log-rank检验
logrank <- survdiff(Surv(MONTH, Event) ~ risk_group, data = mydata)
cat("Log-Rank 检验结果：\n")
print(logrank)

####DCA####
library(survival)
library(ggDCA)
library(patchwork)
library(ggplot2)
library(rms)
dd <- datadist(mydata)
options(datadist = "dd")
# 建立三个模型
model_ctnm <- coxph(Surv(MONTH, Event) ~ cTNM, data = mydata)
model_clinical <- coxph(Surv(MONTH, Event) ~ cTNM + Smoke + PRE_ALB, data = mydata)
model_nomogram <- coxph(Surv(MONTH, Event) ~ cTNM  + Smoke + PRE_ALB + POST_SII_Group + POST_PIV_Group, data = mydata)

# 选择时间点，例如36个月
t0 <- 12

# DCA分析
dca_result <- dca(
  model_ctnm,
  model_clinical,
  model_nomogram,
  model.names = c("cTNM", "Clinical", "Nomogram"),
  times = t0
)

# 绘图
p <- ggplot(dca_result, lwd = 1.2) +
  scale_color_manual(
    values = c("cTNM" = "#2166AC", "Clinical" = "#FF9743", "Nomogram" = "#9D5BB9",
               "All" = "grey50", "None" = "black")
  ) +
  scale_linetype_manual(values = rep("solid", 5)) +  # 所有曲线实线
  coord_cartesian(xlim = c(0, 0.5)) +   # 截断x轴到0.5
  theme_bw() +
  labs(
    title = paste0("DCA at ", t0/12, " Years"),
    x = "Risk Threshold",
    y = "Net Benefit"
  ) +
  theme(
    text = element_text(family = "serif", size = 12),
    axis.title = element_text(face = "bold"),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

print(p)
dev.off()
# 保存为高分辨率TIFF
ggsave("DCA_cTNM_Clinical_Nomogram_12_test.tiff", p, width = 7, height = 6, dpi = 300)

####NRI/IDI####
# 1. 加载必要包
library(rms)
library(survival)
library(survIDINRI)
library(openxlsx)

# 2. 设置 datadist（如果使用 rms 包建模）
dd <- datadist(mydata)
options(datadist = "dd")

model_ctnm <- coxph(Surv(MONTH, Event) ~ cTNM, data = mydata)
model_clinical <- coxph(Surv(MONTH, Event) ~ cTNM  + Smoke + PRE_ALB, data = mydata)
model_nomogram <- coxph(Surv(MONTH, Event) ~ cTNM  + Smoke + PRE_ALB + POST_SII_Group + POST_PIV_Group, data = mydata)


# 4. 准备 time-event 数据
indata <- data.frame(
  time = as.numeric(mydata$MONTH),
  status = ifelse(mydata$Event == 1, 1, 0)
)

# 5. 构建设计矩阵
covs_tnm <- model.matrix(~ cTNM, data = mydata)[, -1]
covs_clinical <- model.matrix(~ cTNM + Smoke + PRE_ALB, data = mydata)[, -1]
covs_nomogram <- model.matrix(~ cTNM + Smoke + PRE_ALB + POST_SII_Group + POST_PIV_Group, data = mydata)[, -1]

# 6. 检查 NA
stopifnot(!anyNA(covs_tnm))
stopifnot(!anyNA(covs_clinical))
stopifnot(!anyNA(covs_nomogram))

# 7. 定义通用函数：以 TNM 为基线比较模型
calc_nri_idi <- function(covs_base, covs_new, t0) {
  result <- IDI.INF(
    indata = as.matrix(indata),
    covs0 = covs_base,
    covs1 = covs_new,
    t0 = t0,
    npert = 1000
  )
  IDI.INF.OUT(result)
}

# 8. 分别比较 Clinical vs TNM 和 Nomogram vs TNM
# ---- 1年
res_clinical_vs_tnm_1yr <- calc_nri_idi(covs_tnm, covs_clinical, t0 = 12)
res_nomogram_vs_tnm_1yr <- calc_nri_idi(covs_tnm, covs_nomogram, t0 = 12)

# ---- 3年
res_clinical_vs_tnm_3yr <- calc_nri_idi(covs_tnm, covs_clinical, t0 = 36)
res_nomogram_vs_tnm_3yr <- calc_nri_idi(covs_tnm, covs_nomogram, t0 = 36)

#5年
res_clinical_vs_tnm_5yr <- calc_nri_idi(covs_tnm, covs_clinical, t0 = 60)
res_nomogram_vs_tnm_5yr <- calc_nri_idi(covs_tnm, covs_nomogram, t0 = 60)

# 9. 整理所有结果为格式化表格（合并输出，保留三位小数）
# 拼接估计值 (95%CI) 的函数，保留三位小数
format_result <- function(res) {
  ci <- paste0(
    formatC(res[, "Est."], format = "f", digits = 3), " (",
    formatC(res[, "Lower"], format = "f", digits = 3), "–",
    formatC(res[, "Upper"], format = "f", digits = 3), ")"
  )
  p <- ifelse(res[, "p-value"] < 0.001, "<0.001", formatC(res[, "p-value"], format = "f", digits = 3))
  list(ci = ci, p = p)
}

# 提取格式化结果
res_list <- list(
  "1-year" = list(
    Clinical = format_result(res_clinical_vs_tnm_1yr),
    Nomogram = format_result(res_nomogram_vs_tnm_1yr)
  ),
  "3-year" = list(
    Clinical = format_result(res_clinical_vs_tnm_3yr),
    Nomogram = format_result(res_nomogram_vs_tnm_3yr)
  ),
  "5-year" = list(
    Clinical = format_result(res_clinical_vs_tnm_5yr),
    Nomogram = format_result(res_nomogram_vs_tnm_5yr)
  )
)

# 合并所有时间点的数据框
df_all <- do.call(rbind, lapply(names(res_list), function(tp) {
  data.frame(
    Time = tp,
    Model = c("TNM", "Clinical", "Nomogram"),
    `IDI (95%CI)` = c("-", res_list[[tp]]$Clinical$ci[1], res_list[[tp]]$Nomogram$ci[1]),
    `IDI Pvalue` = c("-", res_list[[tp]]$Clinical$p[1], res_list[[tp]]$Nomogram$p[1]),
    `NRI (95%CI)` = c("-", res_list[[tp]]$Clinical$ci[2], res_list[[tp]]$Nomogram$ci[2]),
    `NRI Pvalue` = c("-", res_list[[tp]]$Clinical$p[2], res_list[[tp]]$Nomogram$p[2]),
    `Median Improvement (95%CI)` = c("-", res_list[[tp]]$Clinical$ci[3], res_list[[tp]]$Nomogram$ci[3]),
    `Median Pvalue` = c("-", res_list[[tp]]$Clinical$p[3], res_list[[tp]]$Nomogram$p[3]),
    stringsAsFactors = FALSE
  )
}))

# 10. 导出为 Excel 单一 sheet
library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, "NRI_IDI_1_3_5yr")
writeData(wb, "NRI_IDI_1_3_5yr", df_all)

saveWorkbook(wb, "NRI_IDI_0804_test.xlsx", overwrite = TRUE)



#####单指标vs列线图####
library(survival)
library(survIDINRI)
library(openxlsx)

# 基础临床变量
clinical_vars <- c("Gender", "Age", "Smoke", "cTNM", "T_stage", "node_metastatic", "pdl1", "PRE_ALB")

base_formula <- as.formula(Surv(MONTH, Event) ~ cTNM)


# 定义炎症指标
inflammation_vars <- c(
  "PRE_PLR_Group", "PRE_NLR_Group", "PRE_SII_Group", "PRE_SIRI_Group", "PRE_PIV_Group",
  "POST_PLR_Group", "POST_NLR_Group", "POST_SII_Group", "POST_SIRI_Group", "POST_PIV_Group",
  "PLR_Group", "NLR_Group", "SII_Group", "SIRI_Group", "PIV_Group"
)

# 构建基础时间-事件数据
indata <- data.frame(
  time = mydata$MONTH,
  status = mydata$Event
)

# 构建 cTNM 设计矩阵
covs_base <- model.matrix(~ cTNM, data = mydata)[, -1]

# 设定时间点
timepoints <- c(12, 36, 60)

# 存储结果
results_list <- list("1-year" = list(), "3-year" = list(), "5-year" = list())

# 定义结果格式函数
format_result <- function(res) {
  ci <- paste0(
    round(res[, "Est."], 3), " (",
    round(res[, "Lower"], 3), "–",
    round(res[, "Upper"], 3), ")"
  )
  p <- signif(res[, "p-value"], 3)
  list(ci = ci, p = p)
}

# 计算 NRI/IDI 循环
for (var in inflammation_vars) {
  # 构建新模型的设计矩阵：cTNM + 临床变量 + 当前炎症指标
  formula_new <- as.formula(paste("~ cTNM +", paste(clinical_vars, collapse = " + "), "+", var))
  covs_new <- model.matrix(formula_new, data = mydata)[, -1]
  if (any(is.na(covs_new))) next  # 跳过含 NA 的模型
  
  for (t0 in timepoints) {
    res <- IDI.INF(
      indata = indata,
      covs0 = covs_base,
      covs1 = covs_new,
      t0 = t0,
      npert = 1000
    )
    out <- IDI.INF.OUT(res)
    formatted <- format_result(out)
    
    # 保存结果
    yr_key <- paste0(t0 / 12, "-year")
    results_list[[yr_key]][[var]] <- data.frame(
      Model = var,
      `IDI (95%CI)` = formatted$ci[1],
      `IDI Pvalue` = formatted$p[1],
      `NRI (95%CI)` = formatted$ci[2],
      `NRI Pvalue` = formatted$p[2],
      `Median Improvement (95%CI)` = formatted$ci[3],
      `Median Pvalue` = formatted$p[3]
    )
  }
}

# 导出为 Excel 文件
wb <- createWorkbook()
for (yr in names(results_list)) {
  df <- do.call(rbind, results_list[[yr]])
  addWorksheet(wb, sheetName = yr)
  writeData(wb, sheet = yr, df)
}
saveWorkbook(wb, "NRI_IDI_SingleIndicators_test.xlsx", overwrite = TRUE)

####heatmap####
# 加载必要包
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(reshape2)

# 读取数据（确保文件路径与表名正确）
df <- read.xlsx("all.xlsx", sheet = "ROCSINGLE")

# 修改列名统一格式
colnames(df)[1] <- "Predictor"

# 提取用于热图的列
df <- df[, c("Predictor", "AUC_1yr", "AUC_3yr", "AUC_5yr", "ROC-AUC", "Cindex")]

# 转为长格式
df_long <- melt(df, id.vars = "Predictor")

# 排序：按照 ROC-AUC 降序排列行（y 轴）
df_order <- df %>% arrange(desc(`ROC-AUC`)) %>% pull(Predictor)
df_long$Predictor <- factor(df_long$Predictor, levels = df_order)

# 设置横轴顺序
df_long$variable <- factor(df_long$variable, levels = c("AUC_1yr", "AUC_3yr", "AUC_5yr", "ROC-AUC", "Cindex"),
                           labels = c("1-year AUC", "3-year AUC", "5-year AUC", "ROC-AUC", "C-index"))

# 绘图：热图 + 三位小数 + 渐变色
ggplot(df_long, aes(x = variable, y = Predictor, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.3f", value)), color = "black", family = "serif", size = 4) +
  scale_fill_gradient2(
    low = "#5B9BD5", mid = "#F4F4F4", high = "#C00000",
    midpoint = 0.75, name = "Value"
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    panel.grid = element_blank()
  )


#####ROCMODEL####
library(openxlsx)
library(ComplexHeatmap)
library(dplyr)
library(circlize)
library(tibble)

# 读取数据
df <- read.xlsx("all.xlsx", sheet = "ROCmodel")
colnames(df)[1] <- "Model"

# 分组
train_df <- df %>%
  filter(data == "Train") %>%
  select(Model, AUC_1yr, AUC_3yr, AUC_5yr, Cindex) %>%
  column_to_rownames("Model")

test_df <- df %>%
  filter(data == "Test") %>%
  select(Model, AUC_1yr, AUC_3yr, AUC_5yr, Cindex) %>%
  column_to_rownames("Model")

# 按 train 的 C-index 排序
train_df <- train_df[order(train_df$Cindex, decreasing = TRUE), ]
test_df <- test_df[match(rownames(train_df), rownames(test_df)), ]

# 合并矩阵
merged_matrix <- cbind(train_df[, 1:3], train_df$Cindex,
                       test_df[, 1:3], test_df$Cindex)
colnames(merged_matrix) <- c("Train_1yr", "Train_3yr", "Train_5yr", "Train_Cindex",
                             "Test_1yr", "Test_3yr", "Test_5yr", "Test_Cindex")


# 上方颜色注释
col_anno <- HeatmapAnnotation(
  Group = c(rep("Train", 4), rep("Test", 4)),
  col = list(Group = c("Train" = "#4575b4", "Test" = "#d73027")),
  show_annotation_name = FALSE,
  annotation_legend_param = list(
    title_gp = gpar(fontfamily = "serif", fontsize = 10),
    labels_gp = gpar(fontfamily = "serif", fontsize = 10)
  )
)

# 热图颜色映射函数
color_fun <- colorRamp2(c(0.73, 0.82, 0.91), c("#4575b4", "#f7f7f7", "#d73027"))

# 绘图
Heatmap(
  as.matrix(merged_matrix),
  name = "AUC/Cindex",
  top_annotation = col_anno,
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  row_names_gp = gpar(fontsize = 10, fontfamily = "serif"),
  column_names_gp = gpar(fontsize = 11, fontfamily = "serif"),
  col = color_fun,
  column_title = "Model Evaluation Metrics",
  column_title_gp = gpar(fontsize = 14, fontface = "bold", fontfamily = "serif"),
  heatmap_legend_param = list(
    title_gp = gpar(fontfamily = "serif", fontsize = 10),
    labels_gp = gpar(fontfamily = "serif", fontsize = 9)
  ),
  cell_fun = function(j, i, x, y, width, height, fill) {
    grid.text(sprintf("%.3f", merged_matrix[i, j]), x, y, gp = gpar(fontsize = 8, fontfamily = "serif"))
  }
)

#------仅train----
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)

# 读取数据并筛选 Train
df <- read.xlsx("all.xlsx", sheet = "ROCmodel")
colnames(df)[1] <- "Model"
df <- df %>% filter(Data == "Train")  # 此时使用小写 data 就不会错
str(df)
# 保留并排序
df <- df %>% select(Model, AUC_1yr, AUC_3yr, AUC_5yr, Cindex) %>%
  arrange(desc(Cindex))  # 按Cindex降序排序

# 转换为长格式
df_long <- df %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value")

# 自定义横轴标签
df_long$Metric <- factor(df_long$Metric,
                         levels = c("AUC_1yr", "AUC_3yr", "AUC_5yr", "Cindex"),
                         labels = c("1-year AUC", "3-year AUC", "5-year AUC", "C-index"))

# 为了保持绘图顺序，将 Model 设置为因子
df_long$Model <- factor(df_long$Model, levels = unique(df$Model))

# 绘图
ggplot(df_long, aes(x = Metric, y = Model, fill = Value)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.3f", Value)), color = "black", size = 4, family = "serif") +
  scale_fill_gradient2(
    low = "#5B9BD5", mid = "#F4F4F4", high = "#C00000",
    midpoint = 0.85, name = "Value"
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title = element_blank(),
    legend.title = element_text(family = "serif", size = 10),
    legend.text = element_text(family = "serif", size = 9),
    panel.grid = element_blank()
  )

####forest####
library(openxlsx)
library(dplyr)
library(forestploter)
library(stringr)
library(grid)

# 读取数据（替换为你实际的 Excel 文件路径）
results1 <- read.xlsx("all.xlsx", sheet = "stepwise")

# 设置变量名
results2 <- rbind(
  c("Gender", NA, NA, NA, NA),
  c("Female",  "reference",NA, NA, NA),
  results1[3, ],
  results1[4, ],
  c("Smoking History", NA, NA, NA, NA),
  c("No",  "reference",NA, NA, NA),
  results1[7, ],
  
  c("cTNM Stage", NA, NA, NA, NA),
  c("Clinical I","reference", NA, NA, NA),
  results1[10:11, ],
  
  results1[12, ],
  
  c("Postoperative SII", NA, NA, NA, NA),
  c("Low","reference", NA, NA, NA),
  results1[15, ],
  
  c("Postoperative PIV", NA, NA, NA, NA),
  c("Low","reference", NA, NA, NA),
  results1[18, ],
  
  rep(NA, ncol(results1))
)

colnames(results2) <- c("Characteristics", "HR", "Lower_CI", "Upper_CI", "P_Value")
str(results2)
# 格式化数据
result2 <- results2 %>%
  mutate(
    HR_num = as.numeric(HR),
    lower = as.numeric(Lower_CI),
    upper = as.numeric(Upper_CI),
    se = ifelse(!is.na(HR_num), (log(upper) - log(HR_num)) / 1.96, NA),
    blank = " ",
    
    # 格式化 HR(95%CI)
    `HR(95%CI)` = case_when(
      HR == "reference" ~ "reference",
      is.na(HR_num) ~ "",
      TRUE ~ sprintf("%.2f (%.2f–%.2f)", HR_num, lower, upper)
    ),
    
    # 格式化 P 值
    `P value` = case_when(
      P_Value == "P value" ~ "",
      is.na(P_Value) ~ "",
      as.numeric(P_Value) < 0.001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", as.numeric(P_Value))
    )
  )
# 替换 NA 为 ""
result2$Characteristics <- ifelse(is.na(result2$Characteristics), "", result2$Characteristics)

# 选择绘图数据框
df_plot <- result2[, c("Characteristics", "HR(95%CI)", "blank", "P value")]

# 设置列宽
col_widths <- c(
  unit(5, "cm"),
  unit(4.5, "cm"),
  unit(6, "cm"),
  unit(3.5, "cm")
)

# 主题设置
tm <- forest_theme(
  base_size = 12,
  base_family = "serif",
  ci_pch = 15,
  ci_col = "red3",
  ci_fill = "red3",
  ci_alpha = 0.8,
  ci_lty = 1,
  ci_lwd = 1.5,
  ci_Theight = 0.3,
  refline_lwd = 1,
  refline_lty = "dashed",
  refline_col = "grey30",
  vertline_lwd = 1,
  vertline_lty = "dashed",
  vertline_col = "grey30"
)
# 绘图前修改列名，去掉误差线图的列标题
colnames(df_plot) <- c("Characteristics", "HR(95%CI)", "                  ", "P value")
# 绘制森林图
forest(
  df_plot,
  est = result2$HR_num,
  lower = result2$lower,
  upper = result2$upper,
  sizes = result2$se,
  ci_column = 3,
  ref_line = 1,
  arrow_lab = c("Low risk", "High Risk"),
  xlim = c(0, 8),
  ticks_at = c(0, 1, 3, 5, 7),
  theme = tm,
  colwidths = col_widths
)

####heatpmap####
rm(list = ls());gc()
# 设置工作路径
setwd("C:/Users/Yuki/Desktop/DDL/开题数据/qpcr/clin")
pacman::p_load(moonBook, Hmisc, dplyr, furniture,writexl,openxlsx,mice)
mydata <- read.xlsx("67.xlsx")
convars <- c( "Ehacq22"  , "Amucq22"  , "Rcacq22"  , "Rgncq22"  , "Hincq22" ,  "Cpacq22"   ,"POST_NEU", "POST_LYM" ,"POST_MON" ,"POST_PLT" ,"POST_ALB")
colnames(mydata)
mydata[convars] <- lapply(mydata[convars], as.numeric)
mydata$POST_NLR <- mydata$POST_NEU / mydata$POST_LYM
mydata$POST_PLR <- mydata$POST_PLT / mydata$POST_LYM
mydata$POST_SII <- mydata$POST_NEU * mydata$POST_PLT / mydata$POST_LYM
mydata$POST_SIRI <- mydata$POST_NEU * mydata$POST_MON / mydata$POST_LYM
mydata$POST_PIV <- mydata$POST_NEU * mydata$POST_PLT * mydata$POST_MON / mydata$POST_LYM
str(mydata)
# 载入必要的包
library(corrplot)

# 提取相关变量
cor_data <- mydata[, 2:17]  # 排除ID，只选取连续数值型变量

# 自定义函数：计算 p 值矩阵
cor.mtest <- function(mat, method) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], method = method)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  return(p.mat)
}

# 计算 p 值矩阵
p_matrix <- cor.mtest(cor_data, method = "spearman")
library(corrplot)
png("correlation_plot_beautified.png", width = 2400, height = 2400, res = 300)
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "original",
         tl.col = "black",
         tl.cex = 1.5,           # 标签字体大一些
         number.cex = 0.1,       # 星号字体略小一点（关键！）
         p.mat = p_matrix,
         sig.level = c(0.001, 0.01, 0.05),
         insig = "label_sig",
         col = colorRampPalette(c("#4575b4", "#f7f7f7", "#d73027"))(200),
         cl.cex = 1.2,
         mar = c(0.5, 0.5, 0.5, 0.5)  
)
dev.off()
windows(width = 12, height = 12)  # 或 quartz() on macOS

# 提取变量
metabolic_vars <- c("Ehacq22", "Amucq22", "Rcacq22", "Rgncq22", "Hincq22", "Cpacq22")
immune_vars <- c("POST_NEU", "POST_LYM", "POST_MON", "POST_PLT", "POST_ALB",
                 "POST_NLR", "POST_PLR", "POST_SII", "POST_SIRI", "POST_PIV")

# 构建子数据框
sub_data <- mydata[, c(metabolic_vars, immune_vars)]

# 计算 Spearman 相关系数
cor_matrix <- cor(sub_data, use = "complete.obs", method = "spearman")

# 提取免疫指标行 × 代谢变量列，并转置（前者为列，后者为行）
result <- t(cor_matrix[immune_vars, metabolic_vars])

# 打印结果
print(result)

# 可选：保存为 Excel
# install.packages("openxlsx")  # 若未安装
library(openxlsx)
write.xlsx(result, file = "correlation.xlsx", rowNames = TRUE)
