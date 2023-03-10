# Uber & Lyft data analysis

# Libraries installation
install_load_package <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE, quiet = TRUE)
  sapply(pkg, suppressPackageStartupMessages(require), character.only = TRUE)
}

list.of.packages <- c("ggplot2","pls","glmnet","Matrix","smbinning","ROCR","cvms","gbm","RCurl","curl","httr","InformationValue","car","caTools","randomForest","ISLR","plyr","knitr","dplyr","readxl","scales","reshape2","rpart.plot","tidyverse","tibble","RColorBrewer","kableExtra","mctest","rpart","viridis","formatR","DescTools","ggpubr","psych","caret","Metrics","reshape","hrbrthemes","Information","MASS","e1071","class","pROC","corrplot","mltools","lubridate")

install_load_package(list.of.packages)

# Main goals:
# 1. Perform data cleaning and EDA on the data set
# 2. Construct classification model to predict whether ride-share price will surge or not

# Import data
df_ride <- read.csv("rideshare_kaggle.csv")
str(df_ride)

# Remove blank and white space
df_ride2 <- df_ride %>% mutate_all(na_if,"") # remove blank cells
df_ride2 <- df_ride2 %>% mutate_if(is.character, str_trim) # remove white space

# NA check
col_names <- colnames(df_ride2)

df_na <- sapply(df_ride2, FUN=function(col_names){
  table(is.na(col_names))
})

df_na <- t(as.data.frame(df_na))
df_na <- as.data.frame(df_na[seq(2,nrow(df_na),2),])
df_na$V2 <- ifelse(df_na$V2 == max(df_na$V1),0,df_na$V2)
df_na <- df_na[order(df_na$V2),]
df_na <- rownames_to_column(df_na)
df_na$rowname <- gsub('.Freq','',df_na$rowname)
colnames(df_na) <- c("Variables","No blank counts","Blank counts")
df_na$`No blank counts` <- as.numeric(df_na$`No blank counts`)
df_na$`Blank counts` <- as.numeric(df_na$`Blank counts`)
df_na

summary(df_ride2)

# Duplicate check
if(nrow(df_ride2) == nrow(distinct(df_ride2))){
  paste("There are no repeated values in the data set")
} else {
  paste("There are",nrow(df_ride2) - nrow(distinct(df_ride2)),
        "duplicated records in the data set")
}

# Check unique values
char_vars <- c(colnames(select_if(df_ride2,is.character)),"day","month")
df_char <- df_ride2[char_vars]
var_uniq <- as.data.frame(lengths(lapply(df_char,unique)))
var_uniq <- rownames_to_column(var_uniq)
colnames(var_uniq) <- c("Variable Name","Unique Count")
rownames(var_uniq) <- NULL
var_uniq <- var_uniq[order(-var_uniq$`Unique Count`),]
var_uniq


# Change data types

# Function that converts time stamp to decimal hours
as_dechour <- function(y){
  y <- format(as.POSIXct(as_datetime(y)), format = "%H:%M")
  sapply(strsplit(y,":"),
         function(x) {
           x <- as.numeric(x)
           x[1]+x[2]/60
         }
  )
}

# Convert all columns showing time to datetime data types & create new features
#df_ride2$datetime <- df_ride$datetime
df_ride2$datetime <- as.POSIXlt.character(df_ride2$datetime,format="%Y-%m-%d %H:%M:%S")
df_ride2$date <- as.Date.character(df_ride2$datetime,format="%Y-%m-%d")
df_ride2$month <- unclass(df_ride2$datetime)$mon
df_ride2$day_of_week <- weekdays(df_ride2$datetime) #(df_ride2$datetime)$wday
df_ride2$day_of_month <- unclass(df_ride2$datetime)$mday

df_ride2$timestamp <- as_datetime(df_ride$timestamp)
df_ride2$hour_of_day <- as_dechour(df_ride$timestamp)
df_ride2$windGustTime <- as_dechour(df_ride$windGustTime)
df_ride2$temperatureHighTime <- as_dechour(df_ride$temperatureHighTime)
df_ride2$temperatureLowTime <- as_dechour(df_ride$temperatureLowTime)
df_ride2$temperatureMaxTime <- as_dechour(df_ride$temperatureMaxTime)
df_ride2$temperatureMinTime <- as_dechour(df_ride$temperatureMinTime)
df_ride2$apparentTemperatureHighTime <- as_dechour(df_ride$apparentTemperatureHighTime)
df_ride2$apparentTemperatureLowTime <- as_dechour(df_ride$apparentTemperatureLowTime)
df_ride2$apparentTemperatureMaxTime <- as_dechour(df_ride$apparentTemperatureMaxTime)
df_ride2$apparentTemperatureMinTime <- as_dechour(df_ride$apparentTemperatureMinTime)
df_ride2$sunriseTime <- as_dechour(df_ride$sunriseTime)
df_ride2$sunsetTime <- as_dechour(df_ride$sunsetTime)
df_ride2$uvIndexTime <- as_dechour(df_ride$uvIndexTime)

# Overview of updated data set
str(df_ride2)
unclass(df_ride2$datetime)

# Change NA values in price to median price
df_price <-
  df_ride2 %>%
  group_by(cab_type) %>% 
  summarise(Mean=mean(price, na.rm=T), Max=max(price, na.rm=T), Min=min(price, na.rm=T), 
            Median=median(price, na.rm=T), Std=sd(price, na.rm=T))

df_ride2$price <- ifelse(df_ride2$cab_type == "Lyft" & is.na(df_ride2$price) == T, df_price$Median[1],df_ride2$price)
df_ride2$price <- ifelse(df_ride2$cab_type == "Uber" & is.na(df_ride2$price) == T, df_price$Median[2],df_ride2$price)

table(is.na(df_ride$price))
table(is.na(df_ride2$price))
table(df_ride2$day_of_week)


# Remove redundant columns
col_remove <- c("id","day","timestamp","timezone","product_id","visibility.1")
df_ride3 <- df_ride2[!names(df_ride2) %in% col_remove]

# Rename columns
df_ride3 <- df_ride3 %>%
  dplyr::rename(product_type = name,
                weather = icon,
                start = source)

str(df_ride3)

# Heat map of day & hour usage by cab_type
table(df_ride3$cab_type)

  # For Lyft
df_lyft <-
  df_ride3 %>%
  subset(cab_type == "Lyft") %>%
  group_by(day_of_week,hour) %>% 
  summarize(Count = n())

# Heatmap by day of week and hour
df_lyft$day_of_week <- factor(df_lyft$day_of_week,
                                levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

ggplot(df_lyft, aes(as.factor(hour), as.factor(day_of_week), fill= Count)) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="darkred") +
  #scale_fill_distiller(palette = "Spectral") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank()) +
  labs(x = paste0("\n","Hour of Day"), y = paste0("Day of Week","\n"),
       title=expression(bold("Lyft usage frequencies by day & hour"))) +
  theme(plot.title = element_text(hjust = 0.5))

# Heatmap by day of month and month
df_lyft_month <-
  df_ride3 %>%
  subset(cab_type == "Lyft") %>%
  group_by(month,day_of_month) %>% 
  summarize(Count = n())

df_lyft_month$month <- as.factor(ifelse(df_lyft_month$month == 10,"October","November"))


ggplot(df_lyft_month, aes(as.factor(day_of_month), month, fill= Count)) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="darkred") +
  #scale_fill_distiller(palette = "Spectral") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank()) +
  labs(x = paste0("\n","Day of Month"), y = paste0("Month","\n"),
       title=expression(bold("Lyft usage frequencies by day & month"))) +
  theme(plot.title = element_text(hjust = 0.5))


  # For Uber
df_uber <-
  df_ride3 %>%
  subset(cab_type == "Uber") %>%
  group_by(day_of_week,hour) %>% 
  summarize(Count = n())

# Heatmap by day of week and hour
df_uber$day_of_week <- factor(df_uber$day_of_week,
                              levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))


ggplot(df_uber, aes(as.factor(hour), as.factor(day_of_week), fill= Count)) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="darkblue") +
  #scale_fill_distiller(palette = "Spectral") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank()) +
  labs(x = paste0("\n","Hour of Day"), y = paste0("Day of Week","\n"),
       title=expression(bold("Uber usage frequencies by day & hour"))) +
  theme(plot.title = element_text(hjust = 0.5))

# Correlation matrix
num_vars <- c("price","distance","precipProbability","temperature","apparentTemperature","humidity","windSpeed","visibility","cloudCover","dewPoint","hour_of_day")

cors <- cor(df_ride3[num_vars], use="pairwise")

corrplot(cors,type="upper",mar=c(0,0,1.5,0),method="color",
         tl.cex=0.8,
         #col=brewer.pal(8,"RdYlBu"),
         addCoef.col = 1,number.digits = 1,number.cex=0.55,
         title="Correlation matrix of numerical values")


# Price by weather types
table(df_ride3$short_summary)
table(df_ride3$weather)

ggplot(df_ride3, aes(x = as.factor(reorder(weather,-price)), y = price, fill=as.factor(cab_type))) +
  geom_boxplot() +
  xlab(paste0("Weather Conditions","\n")) + ylab(paste0("Fare Price","\n")) +
  stat_summary(fun.y="mean", shape=16, color="red", size=0.4,
               position = position_dodge2(width = 0.75,preserve = "single")) +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  #theme(legend.position = "none") +
  labs(title="Fare price by company & weather conditions",fill='Company') +
  scale_fill_brewer(palette="Pastel1") +
  theme(plot.title=element_text(face="bold",hjust=0.5))


# Price by produce type
str(df_ride3)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

df_product <-
  df_ride3 %>%
  group_by(product_type,cab_type) %>%
  summarize(Count=n(),Mean=mean(price),Median=median(price),Revenue=sum(price),Mode=getmode(price))

df_product

ggplot(df_ride3, aes(x = as.factor(reorder(product_type,-price)), y = price, fill=as.factor(cab_type))) +
  geom_boxplot() +
  xlab(paste0("Product Type","\n")) + ylab(paste0("Fare Price","\n")) +
  stat_summary(fun.y="mean", shape=16, color="red", size=0.4,
               position = position_dodge2(width = 0.75,preserve = "single")) +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  #theme(legend.position = "none") +
  labs(title="Fare price by company & product type",fill='Company') +
  scale_fill_brewer(palette="Pastel1") +
  theme(plot.title=element_text(face="bold",hjust=0.5))


# Add weekly gas price as a new feature
df_gas <- read.csv("GASREGW.csv")
df_gas$DATE <- as.Date.character(df_gas$DATE, format="%m/%d/%Y")
df_gas$day_of_month <- as.numeric(format(df_gas$DATE, format="%d"))
df_gas$week_of_month <- paste0(ceiling(df_gas$day_of_month/7),"-",
                               as.numeric(format(df_gas$DATE, format="%m")))
str(df_gas)
df_gas2 <- df_gas[,c(2,4)]

df_ride3$week_of_month <- paste0(ceiling(df_ride3$day_of_month/7),"-",
                                 as.numeric(format(df_ride3$datetime, format="%m")))

df_ride4 <- merge(df_ride3, df_gas2, by=c("week_of_month"))
df_ride4 <- df_ride4 %>% dplyr::rename(gas_price = GASREGW)
str(df_ride4)

num_vars2 <- c(num_vars,"gas_price","surge_multiplier")
cors <- cor(df_ride4[num_vars2], use="pairwise")
corrplot(cors,type="upper",mar=c(0,0,1.5,0),method="color",tl.cex=0.8,
         addCoef.col = 1,number.digits = 1,number.cex=0.55,
         title="Correlation matrix of numerical values")

# Time series plot for price

df_date <-
  df_ride4 %>%
  group_by(date,product_type,cab_type) %>%
  summarize(Count=n(),Mean=mean(price),Median=median(price),Revenue=sum(price),Mode=getmode(price))

df_date

ggplot(df_date, aes(x=date, y=Revenue, color=reorder(product_type,-Revenue))) +
  geom_line() + 
  geom_point() +
  #theme_minimal() +
  #scale_color_viridis(discrete = TRUE) +
  labs(title="Revenue by product type",color=paste0("Product Type","\n","(sorted by revenue)")) +
  xlab("Date") + ylab(paste0("Revenue","\n")) +
  theme(plot.title=element_text(face="bold",hjust=0.5))

df_ride4 <- df_ride4 %>% 
  dplyr::rename("product" = "name",
                "weather" = "icon")

# Predict whether price will surge
df_lyft_surge <- df_ride4[df_ride4$cab_type=="Lyft",c("source", "product", "weather", "distance", "temperature", "windSpeed", "visibility", "pressure", "cloudCover", "ozone", "moonPhase", "hour_of_day", "destination", "day_of_week", "price_surge")]
df_lyft_surge$cab_type <- NULL

# Convert numerical to binary data type for classification
df_lyft_surge$price_surge <- if_else(df_lyft_surge$surge_multiplier > 1,1,0)
char_vars_lyft <- names(select_if(df_lyft_surge,is.character))
df_lyft_surge$surge_multiplier <- NULL
df_lyft_surge$price <- NULL

# Convert categorical to factor
for (i in char_vars_lyft){
  df_lyft_surge[,i] <- as.factor(df_lyft_surge[,i])
}

num_vars_lyft <- names(select_if(df_lyft_surge,is.numeric))
num_vars_lyft <- num_vars_lyft[!num_vars_lyft %in% c("price_surge")]

# Scale numerical features
df_lyft_surge2 <- df_lyft_surge
df_lyft_surge2[num_vars_lyft] <- scale(df_lyft_surge[num_vars_lyft])

#df_lyft_surge2$price_surge <- factor(df_lyft_surge2$price_surge, levels=c(0,1))
str(df_lyft_surge2[,!names(df_lyft_surge2) %in% "destination"])

# Balance classes of dependent variable
df_surge_freq <- as.data.frame(t(as.data.frame(table(df_lyft_surge2$price_surge))))
rownames(df_surge_freq) <- c("Price-Surge categories","Frequency")
colnames(df_surge_freq) <- c("Not-surge","Surge")
df_surge_freq$`Not-surge` <- as.numeric(df_surge_freq$`Not-surge`)
df_surge_freq$Surge <- as.numeric(df_surge_freq$Surge)

df_surge <- df_lyft_surge2[df_lyft_surge2$price_surge == 1,]
df_nosurge <- df_lyft_surge2[df_lyft_surge2$price_surge == 0,]

no_bias_row <- min(nrow(df_surge),nrow(df_nosurge))
private_yes_nobias <- head(df_nosurge,no_bias_row)
df_col_equal <- rbind(private_yes_nobias,df_surge)
table(df_col_equal$price_surge)

df_surge_freq2 <- as.data.frame(t(as.data.frame(table(df_col_equal$price_surge))))
rownames(df_surge_freq2) <- c("Price-Surge categories","Frequency")
colnames(df_surge_freq2) <- c("Not-surge","Surge")
df_surge_freq2$`Not-surge` <- as.numeric(df_surge_freq2$`Not-surge`)
df_surge_freq2$Surge <- as.numeric(df_surge_freq2$Surge)

# Partition unscaled data
set.seed(888)
surge_trainRatio <- createDataPartition(df_col_equal$price_surge,p=0.8,list=F,times=1)
surge_train <- df_col_equal[surge_trainRatio,]
surge_test <- df_col_equal[-surge_trainRatio,]

# Logistic regression model
logit_model <- glm(price_surge~.-destination, data=surge_train, family=binomial(link="logit")) %>% stepAIC(trace = FALSE)
summary(logit_model)

# Variable importance for Uber prices
surge_varImp <- as.data.frame(varImp(logit_model, scale = F))
surge_varImp <- surge_varImp[order(abs(surge_varImp$Overall),decreasing=T),,drop=F]
surge_varImp <- rownames_to_column(surge_varImp, var="Variables")
surge_varImp$RelativeImp <- surge_varImp$Overall/sum(surge_varImp$Overall)
rownames(surge_varImp) <- c(1:nrow(surge_varImp))

ggplot(surge_varImp) +
  geom_col(aes(x = reorder(Variables,RelativeImp), y = RelativeImp*100, fill=reorder(Variables,RelativeImp)), show.legend = F) +
  coord_flip() +
  labs(title=paste0("Variable importance of logistic regression model")) +
  ylab("Relative Importance (%)") + xlab(paste("")) + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Generate predictions using the training set
logit_predict_train <- predict(logit_model, surge_train, type="response")

# Calculate the optimal cutoff for the test set
optCutOff1 <- optimalCutoff(surge_train$price_surge, logit_predict_train)[1]
cat("Optimal cutoff (threshold) for the training set is = ", optCutOff1, "\n")

# Create confusion matrix on the training set:
pred1 <- as.data.frame(logit_predict_train)
pred1$logit_predict_train[pred1$logit_predict_train < optCutOff1] <- 0
pred1$logit_predict_train[pred1$logit_predict_train >= optCutOff1] <- 1

confusion_matrix_train <- confusionMatrix(as.factor(surge_train$price_surge),
                                          as.factor(pred1$logit_predict_train),positive="1")
confusion_matrix_train <- as.tibble(confusion_matrix_train$table)


# Generate predictions using the testing set
logit_predict_test <- predict(logit_model, surge_test, type="response")

# Calculate the optimal cutoff for the testing set
optCutOff2 <- optimalCutoff(surge_test$price_surge, logit_predict_test)[1]
cat("Optimal cutoff (threshold) for the testing set is = ", optCutOff2, "\n")

# Create confusion matrix on the training set:
pred2 <- as.data.frame(logit_predict_test)
pred2$logit_predict_test[pred2$logit_predict_test < optCutOff2] <- 0
pred2$logit_predict_test[pred2$logit_predict_test >= optCutOff2] <- 1

confusion_matrix_test <- confusionMatrix(as.factor(surge_test$price_surge),
                                         as.factor(pred2$logit_predict_test),positive="1")

confusion_matrix_test <- as.tibble(confusion_matrix_test$table)

logit_cfs_train <- plot_confusion_matrix(confusion_matrix_train,
                                         #font_counts = font(size = 6),
                                         target_col = "Reference",
                                         prediction_col = "Prediction",
                                         counts_col = "n",
                                         place_x_axis_above = FALSE,
                                         add_normalized = FALSE,
                                         palette = "Reds") +
  labs(x = "Observation", y = "Prediction",
       subtitle=expression(italic("Training Results"))) +
  theme(plot.subtitle = element_text(hjust = 0.5))

logit_cfs_test <- plot_confusion_matrix(confusion_matrix_test,
                                        #font_counts = font(size = 6),
                                        target_col = "Reference",
                                        prediction_col = "Prediction",
                                        counts_col = "n",
                                        place_x_axis_above = FALSE,
                                        add_normalized = FALSE,
                                        palette = "Greens") +
  labs(x = "Observation", y = "Prediction",
       subtitle =expression(italic("Testing Results"))) +
  theme(plot.subtitle = element_text(hjust = 0.5))

ggarrange(logit_cfs_train, logit_cfs_test,
          #labels = paste0("Figure 7. Confusion matrix from the training and testing predictions"),
          ncol = 2, nrow = 1,hjust = -0.04,vjust= 1.6)

# AUC calculation:
ROC_PLOT <- roc(surge_train$price_surge, logit_predict_train)
ROC_PLOT2 <- roc(surge_test$price_surge, logit_predict_test)

# ROC curves
plot(ROC_PLOT, col = "blue",xlab = "False Positivity Rate (1-Specificity)",
     ylab = "True Positivity Rate (Sensitivity)",print.auc = TRUE)

plot(ROC_PLOT2, col = "red",xlab = "False Positivity Rate (1-Specificity)",
     ylab = "True Positivity Rate (Sensitivity)",print.auc = TRUE,print.auc.y = .4,add = TRUE)

legend("bottomright",c("Training ROC","Testing ROC"),lty=1,col=c("blue","red"),cex=0.8)

#mtext(expression(bold("Figure 8. ROC Curves from the training and testing set")),
#      side = 3, line = -1.5, outer = TRUE, cex=1.2)
