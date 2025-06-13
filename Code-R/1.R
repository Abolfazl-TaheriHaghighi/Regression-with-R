# این  فایل حاوی اطلاعات مشتریان یک بازاریاب مستقیم است.
#DirectMarketing(mini).csv
# بازاریاب محصولات خود را با استفاده از پست مستقیم به فروش می‌رساند.
# او کاتالوگ محصولات را به 200 نفر ارسال کرده و مشتریان با تلفن، محصولات مورد نیازشان را سفارش می‌دهند.
# بازاریاب داده‌های مشتریان را جمع‌آوری کرده و هدفش بررسی عواملی است که باعث می‌شود برخی مشتریان بیشتر از دیگران خرید کنند.

# متغیرهای موجود در این فایل داده عبارتند از:

# Gender: دسته‌بندی جنسیتی مشتری (زن یا مرد)
# OwnHome: مالک یا مستأجر بودن مشتری
# Married: وضعیت تاهل مشتری (مجرد یا متأهل)
# Salary: حقوق سالیانه مشتری به دلار
# Children: تعداد فرزندان مشتری
# Catalogs: تعداد کاتالوگ‌های ارسالی به مشتری
# AmountSpent: میزان خرید مشتری به دلار
# Age: بازه سنی مشتریان
#getwd()
#setwd(dir = "C:/Users/Abolfazl-Pc/Documents")
#rm(list = ls())
my_data <- read.csv("DirectMarketing(200).csv" , header = TRUE)
#my_data


attach(my_data)


#رگرسون خطی ساده و چندگانه
# y = B0 + B1(x) + f
#y = b0 + b1(x1) + b2(x2) + . . . + bp(xp) + f
#ei = y - y^
#f = e1 + e2 + e3 + ... + en

#چون تعداد متغر های مستقل من کم است از روش اول میروم و تمام حالات را در نظر مگیرم (دو به توان تعداد متغر های مستقل)


model_1 <- lm(AmountSpent~Salary)
model_2 <- lm(AmountSpent~poly(Salary^2))
model_3 <- lm(AmountSpent~Catalogs)
model_4 <- lm(AmountSpent~poly(Catalogs^2))
model_5 <- lm(AmountSpent~Children)
model_6 <- lm(AmountSpent~poly(Children^2))
model_7 <- lm(AmountSpent~Salary + Catalogs)
model_8 <- lm(AmountSpent~Salary * Catalogs)
model_9 <- lm(AmountSpent~Salary + Children)
model_10 <- lm(AmountSpent~Salary * Children)
model_11 <- lm(AmountSpent~Catalogs * Children)
model_12 <- lm(AmountSpent~Catalogs + Children)
model_13 <- lm(AmountSpent~(Salary + Catalogs + Children))

summary(model_13)

#بهترین مدل موجود در اینجا ، مدل 13 است و بیشترن ضریب تعیین را دارد و همه انها معنادار هستند در سطح 0.99 درصد

plot(Salary , AmountSpent)
abline(model_13, col = "red" , lwd = 3)
abline(model_8 , col = 'green' , lwd = 3)
#در اینجا میتوانیم برای هر یک از مدل ها نمودار و خط رگرسیونی رو رسم کرد کافی است مدل مد نظر را در تابع خط جایگزاری کنید


catalogs_vals <- seq(min(Catalogs) , max(Catalogs) , length.out = 1000)
pred_1 <- predict(model_4 ,data.frame(Catalogs = catalogs_vals))
lines(catalogs_vals , pred_1 , col = 'skyblue' , lwd = 2)

salary_vals <- seq(min(Salary), max(Salary), length.out = 1000)
pred_2 <- predict(model_2,data.frame(Salary = salary_vals))
lines(salary_vals , pred_2 , col = 'blue' , lwd = 2)

#برای خط های مدل هایی که توان دارند میتوانیم به مانند بالا عمل کنیم


predict(model_13 , data.frame(Salary , Catalogs , Children))
predict(model_13 , data = my_data)
predict(model_13 , data.frame(Salary = (c(25000 , 34000)) , Catalogs = (c(6 , 5)) , Children = (c(0 , 1))))

#با دستور پریدکت میتوان با استفاده از مدلی که ساختیم کار پیشبینی را انجام دهیم به دو صورت که داده های دلخواه بدهیم یا از داده های اصلی استفاده کنیم


model0 <- lm(AmountSpent~1)
sum(resid(model0)^2)

model1 <- lm(AmountSpent~Age)
sum(resid(model1)^2)

model2 <- lm(AmountSpent~Age + Gender)
sum(resid(model2)^2)

model3 <- lm(AmountSpent~Age + Gender + OwnHome)
sum(resid(model3)^2)

model4 <- lm(AmountSpent~Age + Gender + OwnHome + Married)
sum(resid(model4)^2)

model5 <- lm(AmountSpent~Age + Gender + OwnHome + Married + Salary)
sum(resid(model5)^2)

model6 <- lm(AmountSpent~Age + Gender + OwnHome + Married + Salary + Children)
sum(resid(model6)^2)

model7 <- lm(AmountSpent~Age + Gender + OwnHome + Married + Salary + Children + Catalogs)
sum(resid(model7)^2)

predict(model7, data.frame(Salary , Catalogs , Children , Age , Gender , OwnHome , Married))
predict(model7 , data = my_data)
predict(model7, data.frame(Salary = (c(25000 , 34000)) , Catalogs = (c(6 , 5)) , Children = (c(0 , 1)) , Age = (c("Old" , "Old")) , Gender = (c("Female" , "Male")), OwnHome =(c("Own" , "Rent")) , Married = (c("Single" , "Single"))))

plot(model7)

#در این بخش با روش فوروارد رفتیم و بهترین مدل ما  یعنی مدل7 با پایین ترن ار اس اس انتخاب شد

#-------------------------------------------------------------------------------------------------------------------------------

line0 <- lm(AmountSpent~Salary + Children + Catalogs + Age +  Gender + OwnHome + Married)
summary(line0)

line1 <- lm(AmountSpent~Salary + Children + Catalogs + Age +  Gender + Married)
summary(line1)

line2 <- lm(AmountSpent~Salary + Children + Catalogs + Age +  Gender)
summary(line2)

line3 <- lm(AmountSpent~Salary + Children + Catalogs + Age)
summary(line3)

line4 <- lm(AmountSpent~Salary + Children + Catalogs)
summary(line4)

predict(line4 , data.frame(Salary , Catalogs , Children))
predict(line4 , data = my_data)
predict(line4, data.frame(Salary = (c(25000 , 34000)) , Catalogs = (c(6 , 5)) , Children = (c(0 , 1))))

plot(line4)
#در این بخش از روش بکوارد رفتیم و با ترکیب متغیر های کیفی و کمی مدلی را برازش کردیم که متاسفانه هیچ کدام از متغیر های مستقل کیفی معنا دار نبودند و دوباره به مدل قبلی خود یعنی مدل 13 رسیدیم



#-----------------------------------------------------------------------------------------------------------------------

#رگرسیون لجستیک (Logistic Regression)

#مقدار خرید رو به صفر و یک تبدیل میکنم که بتونم از لجستیک استفاده کنم 

my_data$Logistic <- ifelse(my_data$AmountSpent < 350, 0, 1)
#ستون جدیدی به داده ها اضافه شد که اگر مشتری بیش از 350 دلار  کرده است مقدار 1 میگرد و اگر کمتر خرید کرده است مقدار 0 میگیرد
attach(my_data)

Logistic_line0 <- glm(Logistic ~Salary + Children + Catalogs + Age +  Gender + OwnHome + Married, family = binomial)
summary(Logistic_line0)

Logistic_line1 <- glm(Logistic ~Salary + Children + Catalogs + Age +  Gender + OwnHome , family = binomial)
summary(Logistic_line1)

Logistic_line2 <- glm(Logistic ~Salary + Children + Catalogs + Age +  Gender , family = binomial)
summary(Logistic_line2)

Logistic_line3 <- glm(Logistic ~Salary + Children + Catalogs + Gender , family = binomial)
summary(Logistic_line3)

Logistic_line4 <- glm(Logistic ~Salary + Children + Catalogs , family = binomial)
summary(Logistic_line4)


plot(Logistic_line4)

predict(Logistic_line4 , data.frame(Salary , Catalogs , Children))
predict(Logistic_line4 , data = my_data)
predict(Logistic_line4, data.frame(Salary = (c(25000 , 34000)) , Catalogs = (c(6 , 5)) , Children = (c(0 , 1))),  type = "response")


#در این قسمت با رگرسیون لجستیک بهترین مدل ما مدل لجستیک 4 است که با روش بک وارد بدستش آوردیم
#سپس نمودار و پیشبینی آن را رسم و انجام دادیم

#-------------------------------------------------------------------------------------------------------------------------

#لجستیک چندگانه

install.packages("nnet") 
library(nnet)
test <- multinom(Age ~ Salary + AmountSpent + Catalogs + Children + Married + OwnHome + Gender, data = my_data)
summary(test)


test1 <- multinom(Age ~ Salary + Catalogs + Children + Married + OwnHome + Gender, data = my_data)
summary(test1)

test2 <- multinom(Age ~ Salary + Catalogs + Children + Married + Gender, data = my_data)
summary(test2)


#بهترین مدل ما تست 1 است چون مقدار ای ای سی کمتری میده 


#----------------------------------------------------------------------------------------------------------------------


#LDA
install.packages("MASS")
library(MASS)
my_data$Logistic <- factor(my_data$Logistic, levels = c(0, 1), labels = c("Low", "High"))
lda_line0 <- lda(Logistic ~Salary + Children + Catalogs)
lda_line0
predict(lda_line0)
predict(lda_line0)$class
plot(lda_line0)
trin <- (AmountSpent < 500)
length(trin)
my_data.500 <- my_data [! trin , ]
lda_line1 <- lda(Logistic ~Salary + Children + Catalogs , data = my_data[trin ,])
predict(lda_line1 , my_data.500)
predict(lda_line1, data.frame(Salary = (c(25000 , 34000)) , Catalogs = (c(6 , 5)) , Children = (c(0 , 1))))
plot(lda_line1)