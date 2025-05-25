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

#model_0 <- مقدار بتا صفر
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







