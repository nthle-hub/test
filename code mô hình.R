# --- 1. TẢI THƯ VIỆN ---
if(!require(quantmod)) install.packages("quantmod")
if(!require(TTR)) install.packages("TTR")
library(quantmod)
library(TTR)

# --- 2. LẤY DỮ LIỆU ---
getSymbols("AAPL", src = "yahoo", from = "2020-01-01", to = "2026-01-01")
data <- AAPL

# --- 3. TÍNH TOÁN CÁC BIẾN CƠ SỞ ---
# RSI
data$RSI <- RSI(Cl(data), n = 14)

# SMA Difference Percentage (Chênh lệch 2 đường trung bình)
data$SMA10 <- SMA(Cl(data), n = 10)
data$SMA50 <- SMA(Cl(data), n = 50)
data$SMA_Diff_Pct <- (data$SMA10 - data$SMA50) / data$SMA50

# Log Volume & Returns
data$logVolume <- log(Vo(data))
data$Return <- dailyReturn(Cl(data))

# Biến tương tác (RSI x logVolume)
data$Interact <- data$RSI * data$logVolume

# --- 4. TẠO ĐỘ TRỄ (LAG) CHO CÁC BIẾN ---
# Để dự báo tương lai, ta phải dùng dữ liệu của "ngày hôm qua" (Lag 1)
data$Lag_RSI <- Lag(data$RSI, 1)
data$Lag_SMA_Diff_Pct <- Lag(data$SMA_Diff_Pct, 1)
data$Lag_logVolume <- Lag(data$logVolume, 1)
data$Lag_Return <- Lag(data$Return, 1)
data$Lag_Interact <- Lag(data$Interact, 1)

# --- 5. BIẾN MỤC TIÊU (DỰ BÁO 5 PHIÊN TỚI) ---
data$Return_5d <- (Next(Cl(data), 5) - Cl(data)) / Cl(data)
data$Direction_5d <- ifelse(data$Return_5d > 0, 1, 0)

# --- 6. LÀM SẠCH VÀ CHIA DỮ LIỆU ---
final_data <- na.omit(data)

# Chia dữ liệu (ví dụ 80% train)
train_size <- floor(0.8 * nrow(final_data))
train_data_5d <- final_data[1:train_size, ]
test_data_5d  <- final_data[(train_size+1):nrow(final_data), ]

# --- 7. CHẠY MÔ HÌNH GLM ---
model <- glm(Direction_5d ~ Lag_RSI + Lag_SMA_Diff_Pct + Lag_logVolume + 
               Lag_Return + Lag_Interact, 
             family = binomial(link = "logit"), 
             data = train_data_5d)

# HIỂN THỊ KẾT QUẢ
summary(model)