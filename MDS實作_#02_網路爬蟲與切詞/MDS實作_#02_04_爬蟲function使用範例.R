
# 設定你的工作目錄
setwd('/home/slave1/git/1062-NTUST-MDS/MDS實作_#02_網路爬蟲與切詞')
source('MDS1062_win.R' , encoding = 'UTF-8')
function_load()

# https://www.urcosme.com/products/85147

author_url = urcos_product_userComment(product_code = 85147)
crawled_result = urcos_crawler(product_code  = 85147)

# author_url1 = author_url[1:50,]
# author_url2 = author_url[50:100,]
# author_url3 = author_url[101:150,]
# author_url4 = author_url[151:200,]
# author_url5 = author_url[201:250,]
# author_url6 = author_url[251:300,]



