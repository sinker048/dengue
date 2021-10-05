#######安裝package
library("shiny")
library("ggplot2")
library("leaflet")
library("scales")
library("shinythemes")
library("plotly")

###讀檔案
people=read.csv("https://data.tainan.gov.tw/dataset/4c260d97-e268-4b4a-8b15-c0fc92a25120/resource/532e1521-5708-4514-97c7-9d0fc502cc8d/download/10402.csv")
dengue=read.csv("https://data.tainan.gov.tw/dataset/3ad9da64-0c29-4299-b769-320b57a09be8/resource/7617bfcd-20e2-4f8d-a83b-6f6b479367f9/download/dengue104.csv")


######################################



###資料前處理
##選取人數較多的行政區域，較少的歸類為其他

tt=(dengue$區別!="東區"&dengue$區別!="北區"&dengue$區別!="中西區"&dengue$區別!="南區"&dengue$區別!="永康區"&dengue$區別!="安南區"&dengue$區別!="安平區")
c=c()
for(i in 1:length(dengue$區別)){
  if(tt[i]){
    a="其他"
    c=c(c,a)
  }else{
    b=as.character(dengue$區別[i])
    c=c(c,b)
  }
}

newdistinct=as.factor(c)

###資料前處理
##更改經度
f=c()
for(i in 1:length(dengue$區別)){
  if(dengue$經度座標[i]<30){
    a=dengue$緯度座標[i]
    f=c(f,a)
  }else if(dengue$經度座標[i]>30&dengue$經度座標[i]<130){
    b=dengue$經度座標[i]
    f=c(f,b)
  }else{
    d=(120.214+120.227)/2
    f=c(f,d)
  }
}

dengue$經度更=f

##更改緯度
d=c()
for(i in 1:length(dengue$區別)){
  if(dengue$緯度座標[i]>30){
    a=dengue$經度座標[i]
    d=c(d,a)
  }else{
    b=dengue$緯度座標[i]
    d=c(d,b)
  }
}

dengue$緯度更=d

###########資料合併
newdengue=cbind(dengue,newdistinct)
colnames(newdengue)=c(colnames(dengue),"新區別")
##########
##製作ring plot

##繪製直方圖

date1=as.Date(newdengue[, "確診日"],"%Y/%m/%d")

##########
##繪製地圖


point.df <- data.frame(
  Lat = newdengue$緯度更,
  Long = newdengue$經度更
)


newdengue$date1=as.Date(newdengue[, "確診日"],"%Y/%m/%d")


newdengue$date2 = as.Date(cut(date1,
                              breaks = "week",
                              start.on.monday =T))
newdengue$Quantity=c(rep(1,length(newdengue$確診日)))

newdengue$date3=as.Date(cut(date1,breaks = "month"))



##########################################

##table(people[,"區域"])

people[1237,"經度"]=120.197882 ## 填補台南市安南區安中路一段698巷 經度
people=people[-1427,]##刪除友愛市場後方
##summary(people)
##levels(people$區域)
###資料前處理
###更改區域
z=c()
for(i in 1:length(people$區域)){
  if(as.character(people$區域[i])=="東"){
    y="東區"  ##12個
    z=c(z,y)
  }else if(as.character(people$區域[i])=="安南"){
    w="安南區"  ##2個  
    z=c(z,w)
  }else if(as.character(people$區域[i])=="麻豆"){
    ww="麻豆區" ##1個
    z=c(z,ww)
  }else if(as.character(people$區域[i])=="新化"){
    www="新化區" ##2個
    z=c(z,www)
  }else if(as.character(people$區域[i])=="新營區 "){
    wwww="新營區" ##2個
    z=c(z,wwww)
  }else{
    z=c(z,as.character(people$區域[i]))
  }
}
cc=as.factor(z)
people$新區域=cc

##table(people[,"新區域"])
####################################
##選取人數較多的行政區域，較少的歸類為其他

tt=(people$新區域!="東區"&
      people$新區域!="北區"&
      people$新區域!="中西區"&
      people$新區域!="南區"&
      people$新區域!="永康區"&
      people$新區域!="安南區"&
      people$新區域!="安平區")
vv=c()
for(i in 1:length(people$新區域)){
  if(tt[i]){
    a="其他"
    vv=c(vv,a)
  }else{
    b=as.character(people$新區域[i])
    vv=c(vv,b)
  }
}
people$更區域=as.factor(vv)


###更改經度
q=c()
for(i in 1:length(people$經度)){
  if(people$經度[i]>1000){
    q=c(q,120.199)##台南市北區臨安路二段195巷
  }else if(people$經度[i]<10){
    q=c(q,120.216175)##台南市安南區安和路一段52巷
  }else if(people$經度[i]>126&people$經度[i]<1000){
    q=c(q,120.241995)##台南市新民街
  }else if(as.character(people$集合地點[i])=="惠安街"){
    q=c(q,120.209063)##惠安街
  }else if(as.character(people$集合地點[i])=="廣州里活動中心(健民街2號)"){
    q=c(q,120.194677)##廣州里活動中心(健民街2號)
  }else{
    q=c(q,people$經度[i])
  }
  
}
people$新經度=q

###更改緯度

##summary(people$緯度)
g=c()
for (i in 1:length(people$緯度)) {
  if(people$緯度[i]<5){
    g=c(g,23.027646)##台南市安南區安和路一段52巷
  }else if(as.character(people$集合地點[i])=="台南市新民街"){
    g=c(g,23.040504)##台南市新民街
  }else if(as.character(people$集合地點[i])=="惠安街"){
    g=c(g,23.026857)##惠安街
  }else if(as.character(people$集合地點[i])=="廣州里活動中心(健民街2號)"){
    g=c(g,22.984521)##廣州里活動中心(健民街2號)
  }else{
    g=c(g,people$緯度[i])
  }
  
}
people$新緯度=g
############
###調整日期的格式

ff=c()
for(i in 1:length(people$日期)){
  ff=c(ff,paste0(c("2015年",as.character(people$日期[i])),collapse = ""))
}
people$新日期=as.Date(ff,format="%Y年%m月%d日")
#########################################

####資料前處理
north=subset(newdengue,newdengue$新區別=="北區")
east=subset(newdengue,newdengue$新區別=="東區")
west=subset(newdengue,newdengue$新區別=="中西區")
south=subset(newdengue,newdengue$新區別=="南區")
unkang=subset(newdengue,newdengue$新區別=="永康區")
ansou=subset(newdengue,newdengue$新區別=="安南區")
anpin=subset(newdengue,newdengue$新區別=="安平區")
###############################
hhhh=hist(date1,breaks = "weeks")
y=hhhh$counts
kk=c()
for(i in 1:length(y)){
  qqq=-7+7*i+5
  now=qqq/365+2015
  kk=c(kk,now)
}

zz=c()
for (i in 1:length(y)) {
  qqq=-7+7*i
  zz=c(zz,qqq)
}
newdengue$date2 = as.Date(cut(date1,
                              breaks = "week",
                              start.on.monday =T))
time_1=as.Date(zz,origin=newdengue$date2[1])

###############################全部 

fffff=c()
fffff$counts=y
fffff$time=kk
fffff$date_1=time_1
fffff$city=c(rep("全部",times=length(y)))
fffff=as.data.frame(fffff)
################################北區
north_hist=hist(north$date1,breaks = "weeks",freq = T)
nnnnn=c()
nnnnn$counts=c(rep(0,52-length(north_hist$counts)),north_hist$counts)
nnnnn$time=kk
nnnnn$date_1=time_1
nnnnn$city=c(rep("北區",times=length(y)))
nnnnn=as.data.frame(nnnnn)
###################################################東區
east_hist=hist(east$date1,breaks = "weeks",freq = T)

eeeee=c()
eeeee$counts=c(rep(0,52-length(east_hist$counts)),east_hist$counts)
eeeee$time=kk
eeeee$date_1=time_1
eeeee$city=c(rep("東區",times=length(y)))
eeeee=as.data.frame(eeeee)
###################################################中西區
west_hist=hist(west$date1,breaks = "weeks",freq = T)

wwwww=c()
wwwww$counts=c(rep(0,52-length(west_hist$counts)),west_hist$counts)
wwwww$time=kk
wwwww$date_1=time_1
wwwww$city=c(rep("中西區",times=length(y)))
wwwww=as.data.frame(wwwww)
####################################################南區
south_hist=hist(south$date1,breaks = "weeks",freq = T)

sssss=c()
sssss$counts=c(rep(0,52-length(south_hist$counts)),south_hist$counts)
sssss$time=kk
sssss$date_1=time_1
sssss$city=c(rep("南區",times=length(y)))
sssss=as.data.frame(sssss)
################################永康
unkang_hist=hist(unkang$date1,breaks = "weeks",freq = T)

uuuuu=c()
uuuuu$counts=c(rep(0,52-length(unkang_hist$counts)),unkang_hist$counts)
uuuuu$time=kk
uuuuu$date_1=time_1
uuuuu$city=c(rep("永康區",times=length(y)))
uuuuu=as.data.frame(uuuuu)

################################安南區
ansou_hist=hist(ansou$date1,breaks = "weeks",freq = T)
annnn=c()
annnn$counts=c(rep(0,52-length(ansou_hist$counts)),ansou_hist$counts)
annnn$time=kk
annnn$date_1=time_1
annnn$city=c(rep("安南區",times=length(y)))
annnn=as.data.frame(annnn)

################################安平區
anpin_hist=hist(anpin$date1,breaks = "weeks",freq = T)
apppp=c()
apppp$counts=c(rep(0,52-length(anpin_hist$counts)),anpin_hist$counts)
apppp$time=kk
apppp$date_1=time_1
apppp$city=c(rep("安平區",times=length(y)))
apppp=as.data.frame(apppp)




################################合併
zzzzz=rbind(fffff,nnnnn,eeeee,wwwww,sssss,uuuuu,annnn,apppp)
##############################移除一些名字
rm("ansou","annnn","anpin","anpin_hist","ansou_hist",
   "apppp","east","east_hist","eeeee","fffff","hhhh",
   "north","north_hist","south","south_hist","sssss",
   "unkang","unkang_hist","uuuuu","west","west_hist",
   "wwwww","nnnnn")


#############################
#############################
accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}
data_time=zzzzz%>%accumulate_by(~time)
#############################移除zzzzz
rm("zzzzz")
#############################




##########################################




ui=fluidPage(theme = shinytheme("flatly"),
             navbarPage(title= "104年台南登革熱災情",
                        tabPanel(title="動機與目的",
                                 verticalLayout(
                                   tags$img(src="http://tnr.com.tw/images/NewsImage/unnamed%20(3)_77.jpg", 
                                            width = "500px", height = "250px"),
                                   mainPanel(
                                     br(),
                                     textOutput("mottitle"),
                                     br(),
                                     textOutput("mot"),
                                     br(),
                                     textOutput("purposetitle"),
                                     br(),
                                     textOutput("purpose"),
                                     br()
                                   )
                                 )
                        ),
                        tabPanel(title="登革熱資料來源及資料前處理",
                                 titlePanel("登革熱病例資料介紹"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     helpText("Show the first n observations"),
                                     numericInput("obs", "Number of observations to view:", 10,min = 0),
                                     actionButton("button","Show",class="btn-primary"),
                                     
                                     helpText("資料連結 :"),
                                     a(href="https://data.tainan.gov.tw/dataset/denguefevercases/resource/7617bfcd-20e2-4f8d-a83b-6f6b479367f9?fbclid=IwAR2iFkT-1-uL79c-mdPj5CvA_XyJx9wgueXBY4nm9p3losveLsRfTJpixf4", 
                                       "104年臺南市本土登革熱病例")
                                   ),
                                   mainPanel(textOutput("socure"),
                                             textOutput("time"),
                                             textOutput("region"),
                                             textOutput("count"),
                                             br(),
                                             textOutput("pretitle"),
                                             textOutput("pre"),
                                             hr(),
                                             tableOutput("table"))
                                 )
                        ),
                        tabPanel(title="噴藥資料來源及資料前處理",
                                 titlePanel("登革熱噴藥資料介紹"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     helpText("Show the first n observations"),
                                     numericInput("obs_people", "Number of observations to view:",5,min = 0),
                                     actionButton("button_people","Show",class="btn-primary"),
                                     
                                     helpText("資料連結 :"),
                                     a(href="https://data.tainan.gov.tw/dataset/104-df-chemical-control/resource/532e1521-5708-4514-97c7-9d0fc502cc8d?fbclid=IwAR3chzRzhvmaKDS9YE8LNTevcYML3bDtGhdvmSXUqcyUId7kdMZkZwVlwv8", 
                                       "臺南市104年登革熱噴藥人力及場次")
                                   ),
                                   mainPanel(textOutput("socure_people"),
                                             textOutput("time_people"),
                                             textOutput("region_people"),
                                             textOutput("count_people"),
                                             br(),
                                             textOutput("pretitle_people"),
                                             textOutput("pre_people"),
                                             hr(),
                                             tableOutput("table_people"))
                                 )
                        ),
                        tabPanel(title="整體資料介紹",
                                 sidebarLayout(
                                   sidebarPanel(
                                     helpText("整體資料概況"),
                                     plotlyOutput(outputId = "date_animate"),
                                     width=6
                                   ),
                                   mainPanel(
                                     helpText("台南市行政區罹患登革熱的人數"),
                                     verbatimTextOutput(outputId = "distinct_data"),
                                     helpText("台南市行政區登革熱噴藥的次數"),
                                     verbatimTextOutput(outputId = "distinct_people"),
                                     width=6
                                   )
                                 ),
                                 fluidRow(
                                   column(6,plotOutput(outputId = "ring")),
                                   column(6,plotOutput(outputId = "ring_people"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(6,plotOutput(outputId = "histogram")),
                                   column(6,plotOutput(outputId = "histogram_people"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(6,leafletOutput(outputId = "map")),
                                   column(6,leafletOutput(outputId = "map_people"))
                                 ),
                                 hr(),
                                 fluidRow(
                                   column(6,leafletOutput(outputId = "map_time")),
                                   column(6,leafletOutput(outputId = "map_people_time"))
                                 ),
                                 sliderInput("slider_time", "日期", min=as.Date("2015-01-01"),
                                             max=as.Date("2015-12-31"),
                                             value=as.Date("2015-01-01"), step = 10,animate = T,width ="100%"),
                                 hr(),
                                 fluidRow(
                                   column(6,leafletOutput(outputId = "map_time_period")),
                                   column(6,leafletOutput(outputId = "map_people_time_period"))
                                 ),
                                 sliderInput("slider_time_period", "日期", min=as.Date("2015-01-01"),
                                             max=as.Date("2015-12-31"),
                                             value=as.Date("2015-01-01"), step = 10,animate = T,width ="100%"),
                                 br()
                        ),
                        tabPanel(title="Dashboard",
                                 sidebarLayout(
                                   sidebarPanel(
                                     helpText("在2015年間選取日期和區域來做呈現"),
                                     dateRangeInput(inputId = "daterange",
                                                    label = "選取日期的區間",
                                                    start="2015-01-01",
                                                    end="2015-12-31",
                                                    min="2015-01-01",
                                                    max="2015-12-31",
                                                    format="yyyy-mm-dd"),
                                     checkboxGroupInput(inputId = "new_distinct",
                                                        label = "行政區域",
                                                        choiceNames=c("東區","北區","中西區","南區","永康區","安南區","安平區","其他"),
                                                        choiceValues =c("東區","北區","中西區","南區","永康區","安南區","安平區","其他"))
                                     
                                   ),
                                   mainPanel(
                                     helpText("台南市行政區罹患登革熱的人數"),
                                     verbatimTextOutput(outputId = "show_data_1"),
                                     br(),
                                     helpText("台南市行政區登革熱噴藥的次數"),
                                     verbatimTextOutput(outputId = "show_people_1")
                                   )
                                 ),
                                 fluidRow(column(6,plotOutput(outputId = "ring_1")),
                                          column(6,plotOutput(outputId = "ring_people_1"))
                                 ),
                                 br(),
                                 fluidRow(column(6,plotOutput(outputId = "histogram_1")),
                                          column(6,plotOutput(outputId = "histogram_people_1"))
                                 ),
                                 br(),
                                 fluidRow(column(6,leafletOutput(outputId = "map_1")),
                                          column(6,leafletOutput(outputId = "map_people_1"))
                                 ),
                                 br(),
                                 br()
                        ),
                        tabPanel(title="結論",
                                 titlePanel("結論"),
                                 helpText("1.  罹患登革熱的人數較多的地方，噴藥次數也較多。"),
                                 helpText("2.  罹患登革熱的人的時間大多集中在8~10月，我們推測有下列原因："),
                                 helpText("(1)  當年蘇迪勒颱風造成八八水災重創台南，也因此出現多處積水。"),
                                 helpText("(2)  7、8月降雨天數多，導致能噴藥的時間不足，噴藥範圍也較小。"),
                                 helpText("(3)  當年遇到聖嬰現象，氣候較為溫暖，蚊子較容易繁殖及散播。"),
                                 helpText("3.  10月之後疫情趨緩，應是噴藥數較多，且氣候轉冷之影響"),
                                 helpText("4.  交通幹道、觀光勝地等區域，因人潮較為密集，而登革熱是較容易群聚感染的疾病，染病人數在這些區域也就較多。")
                        ),
                        tabPanel(title="小組成員工作分配",
                                 titlePanel("工作分配"),
                                 helpText("林澤慶：Dashboard、程式、找老師討論"),
                                 helpText("張伊萱：Dashboard、程式、找老師討論"),
                                 helpText("黃思媛：上台報告、找老師討論"),
                                 helpText("羅盼寧：上台報告、找老師討論"),
                                 helpText("曾筱媛：書面報告、找老師討論")
                        ),
                        tabPanel(title="參考資料",
                                 titlePanel("參考資料"),
                                 tags$a(href="https://nidss.cdc.gov.tw/ch/default.aspx", 
                                        "傳染病統計資料查詢系統"),
                                 br(),
                                 tags$a(href="http://sa.ylib.com/MagArticle.aspx?Unit=featurearticles&id=4083&fbclid=IwAR0rRGOVfW2WydHsgVrhCXslDe3MoGX7hBtKNWFN4g_K-5Lg9QICKf5GdyM",
                                        "科學人雜誌"),
                                 br(),
                                 tags$a(href="http://health.tainan.gov.tw/dengue/warehouse/%7BC1304130-950A-47C8-B824-F29B75BCCAB8%7D/1060330-01-2015%E5%B9%B4%E7%99%BB%E9%9D%A9%E7%86%B1%E7%96%AB%E6%83%85%E5%9B%9E%E9%A1%A7-%E5%8A%89%E7%A2%A7%E9%9A%86%E4%B8%BB%E4%BB%BB.pdf?fbclid=IwAR3GOy4UbDAvMWKbsHboJUhBIsBPtcUaQhcHTxz2Sellqf_34GJw-qcRsIY",
                                        "防治校園重要急性傳染病"),
                                 br(),
                                 tags$a(href="https://data.tainan.gov.tw/dataset/104-df-chemical-control/resource/532e1521-5708-4514-97c7-9d0fc502cc8d?fbclid=IwAR0zTqWnj2HAcWvLnNyV5qOvfXIoDjR5ZfCul0EvdUyOHX7wb4EtVtEUU3o",
                                        "臺南市104年登革熱噴藥人力及場次"),
                                 br(),
                                 tags$a(href="https://data.tainan.gov.tw/dataset/denguefevercases/resource/7617bfcd-20e2-4f8d-a83b-6f6b479367f9",
                                        "104年臺南市本土登革熱病例"),
                                 br(),
                                 tags$a(href="https://shiny.rstudio.com/","Shiny"),
                                 br(),
                                 tags$a(href="https://blog.gtwang.org/r/r-leaflet-interactive-map-package-tutorial/amp/?fbclid=IwAR24QBNqjmZcA8pmk9_YbVMKx7wvYWPLnHQbuGh1AWeVKFVlOHEP7xt_imY",
                                        "R Leaflet 地圖套件：繪製網頁互動式地圖，呈現經緯度座標資料"),
                                 br(),
                                 tags$a(href="https://zh.wikipedia.org/wiki/2015%E5%B9%B4%E8%87%BA%E5%8D%97%E5%B8%82%E7%99%BB%E9%9D%A9%E7%86%B1%E7%96%AB%E6%83%85",
                                        "2015年臺南市登革熱疫情"),
                                 br(),
                                 tags$a(href="https://plot.ly/r/","Plotly")
                                 
                        )
                        
             )
             
)
server=function(input,output){
  output$mottitle <- renderText({
    print("動機與目的")
  })
  
  output$mot <- renderText({
    print("登革熱 (dengue fever) 是一種廣泛存在於熱帶、亞熱帶地區的急性蟲媒傳染病，
          登革出血熱更有致命的危險。目前雖然已研發出世界衛生組織評估有效的登革熱疫苗，但尚未於台灣核准上市，
          且此種疫苗只適用於曾經感染過登革熱的民眾，並不能應用施打於所有人身上。
          因此，現階段仍然以杜絕病媒蚊孳生的方式為最佳防治手段，例如清除容器積水、由衛生單位進行噴藥工作等方式來執行，藉此減少傳染途徑及染病機會。")
  })
  
  output$purposetitle <- renderText({
    print("近幾年南臺灣經常有病媒蚊肆虐，尤其是 2015年爆發近五十年來最嚴重的登革熱疫情，
          其中已確診病例人數超過 4 萬人，死亡病例更是高達 200 多人，在當時造成極大的恐慌。")
  })
  
  output$purpose <- renderText({
    print("而此次登革熱疫情從台南開始，影響最嚴重的地區也是台南，因此本組針對2015年台南之登革熱病例資料及噴藥資料，
          希望能透過資料了解當時登革熱的散佈情形與政府進行噴藥的時間地點，探討從疫情爆發到得以控制的過程，以及政府噴藥對於疫情控制的影響。")
  })
  output$socure <- renderText({
    print("資料來源：台南市政府開放平台-登革熱專區")
  })
  
  output$time <- renderText({
    print("資料時間範圍：2015年1月~2015年12月")
  })
  
  output$region <- renderText({
    print("資料空間範圍：臺南市")
  })
  
  output$count <- renderText({
    print("資料筆數：22877筆")
  })
  
  output$pretitle <- renderText({
    print("資料前處理 : ")
  })
  
  output$pre <- renderText({
    print("觀察資料發現有715筆的經緯度部分數字是相反的，所以必須調換過來，
          數字對調後，仍有數字落在台南市區域外，故篩選落在台南市範圍的資料作為分析資料；
          此外，第22858筆北區仁愛里東豐路這筆資料經度座標為410.22明顯不是台南市座標
          ，故取東豐路最左端與最右端數值的平均作為此筆資料的經緯度座標。
          接著針對登革熱病例數多寡劃分成東區／北區／中西區／南區／永康區／安南區／安平區／其他這８個區域")
  })
  
  
  observeEvent(input$button, {
    cat("Showing", input$obs, "rows\n")
  })
  
  df <- eventReactive(input$button, {
    head(dengue[,1:6], n=input$obs)
  })
  output$table <- renderTable({
    df()
  })
  output$socure_people <- renderText({
    print("資料來源：台南市政府開放平台")
  })
  
  output$time_people <- renderText({
    print("資料時間範圍：2015年1月~2015年12月")
  })
  
  output$region_people <- renderText({
    print("資料空間範圍：臺南市")
  })
  
  output$count_people <- renderText({
    print("資料筆數：1829筆")
  })
  
  output$pretitle_people <- renderText({
    print("資料前處理 : ")
  })
  
  output$pre_people <- renderText({
    print("資料前處理的部分包含資料缺失值,數據整理及資料單位轉換。由於安南區安中路一段698號經度有缺失,故需要填補，
          而填補後的經度為120.197882；此外，因為友愛市場後方的經緯度資料有缺失，故將此刪除。
          從資料中可以發現區域的名稱並不一致，故將不一致名稱更改到一致的格式。接著,針對錯誤的經緯度資料進行修正更改至合適的值。
          在資料單位轉換部分，將原始資料＊月＊日更改為２０１５-＊-＊以方便後續的程式執行。同樣地,針對登革熱病例數多寡劃分成
          東區／北區／中西區／南區／永康區／安南區／安平區／其他這８個區域。最後，挑選出資料中重要的變數，像是區域、里別、日期......等做分析")
  })
  
  
  observeEvent(input$button_people, {
    cat("Showing", input$obs_people, "rows\n")
  })
  
  df_people <- eventReactive(input$button_people, {
    head(people[,c(1:3,6,7,8,12)], n=input$obs_people)
  })
  output$table_people <- renderTable({
    df_people ()
  })
  output$distinct_data=renderPrint({table(dengue[,"區別"])})
  output$distinct_people=renderPrint({table(people[,"新區域"])})
  output$date_animate=renderPlotly({
    data_time %>%
      plot_ly(
        x = ~time, 
        y = ~counts,
        split = ~city,
        text = ~paste0(date_1," 至",(date_1+7),"<br>","人數：",counts),
        frame = ~frame, 
        type = 'scatter',
        mode = 'lines', 
        line = list(simplyfy = F)
      ) %>% 
      layout(
        xaxis = list(
          title = "日期",
          zeroline = F
        ),
        yaxis = list(
          title = "人數",
          zeroline = F
        )
      ) %>% 
      animation_opts(
        frame = 150, 
        transition = 0, 
        redraw = FALSE
      ) %>%
      animation_slider(
        hide = T
      ) %>%
      animation_button(
        x = 1, xanchor = "right", y = 0, yanchor = "middle"
      )
    
    
  })
  output$ring=renderPlot({
    aa=sort(table(newdistinct),decreasing = T)
    names(aa)
    dat=data.frame(Distinct=names(aa),count=as.numeric(aa))
    dat$fraction = dat$count / sum(dat$count)
    dat$ymax = cumsum(dat$fraction)
    dat$ymin = c(0, head(dat$ymax, n=-1))
    
    p1 = ggplot(data=dat, aes(fill=Distinct, ymax=ymax, ymin=ymin, xmax=7, xmin=5)) +
      geom_rect() +
      coord_polar(theta="y") +
      xlim(c(0, 7)) +
      theme(panel.grid=element_blank()) +
      theme(axis.text=element_blank()) +
      theme(axis.ticks=element_blank()) +
      annotate("text", x = 0, y = 0, label = " ") +
      labs(title="台南行政區罹患登革熱人數的比例",x="",y="")
    p1
  })
  output$ring_people=renderPlot({dd=sort(table(people$更區域),decreasing = T)
  dat_people=data.frame(Distinct=names(dd),count=as.numeric(dd))
  dat_people$fraction= dat_people$count /sum(dat_people$count)
  dat_people$ymax = cumsum(dat_people$fraction)
  dat_people$ymin = c(0, head(dat_people$ymax, n=-1))
  
  p1 = ggplot(data=dat_people, aes(fill=Distinct, ymax=ymax, ymin=ymin, xmax=7, xmin=5)) +
    geom_rect() +
    coord_polar(theta="y") +
    xlim(c(0, 7)) +
    theme(panel.grid=element_blank()) +
    theme(axis.text=element_blank()) +
    theme(axis.ticks=element_blank()) +
    annotate("text", x = 0, y = 0, label = " ") +
    labs(title="台南行政區登革熱噴藥次數的比例",x="",y="")
  p1})
  output$histogram=renderPlot({
    detach("package:plotly", unload = TRUE)
    layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
    
    # Draw the boxplot and the histogram 
    par(mar=c(0, 4, 1.1, 2.1))
    boxplot(date1 ,
            horizontal=TRUE ,
            frame=F,
            xaxt="n")
    par(mar=c(4, 4, 1.1, 2.1))
    hist(date1 ,
         breaks="weeks" ,
         col=rgb(0.2,0.8,0.5,0.5) ,
         xlab = "日期",
         freq=T,
         ylab="臺南確診登革熱的人數(人)",
         main=("2015年臺南確診登革熱人數的直方圖") )
    grid()
    library("plotly")
  })
  output$histogram_people=renderPlot({
    detach("package:plotly", unload = TRUE)
    layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
    
    # Draw the boxplot and the histogram 
    par(mar=c(0, 4, 1.1, 2.1))
    boxplot(people$新日期 ,
            horizontal=TRUE ,
            frame=F,
            xaxt="n")
    par(mar=c(4, 4, 1.1, 2.1))
    hist(people$新日期 ,
         breaks="weeks" ,
         col=rgb(0.2,0.8,0.5,0.5) ,
         xlab = "日期",
         freq=T,
         ylab="臺南登革熱噴藥次數(次)",
         main=("2015年臺南登革熱噴藥次數的直方圖") )
    grid()
    library("plotly")
  })
  output$map=renderLeaflet({
    ma <- leaflet(point.df) %>%
      addTiles() %>%
      setView(lng = 120.239, lat = 22.992, zoom = 12) %>%
      addMarkers(
        clusterOptions = markerClusterOptions(),popup = newdengue$確診日
      )
    ma
  })
  output$map_people=renderLeaflet({point.df_people <- data.frame(
    Lat = people$新緯度,
    Long = people$新經度
  )
  ma_people <- leaflet(point.df_people) %>%
    addTiles() %>%
    setView(lng = 120.239, lat = 22.992, zoom = 12) %>%
    addMarkers(
      clusterOptions = markerClusterOptions(),popup = paste0(people$日期,"<br>"," 噴工數:",people$噴工數)
    )
  ma_people})
  output$map_time=renderLeaflet({
    cumtime=subset(newdengue,
                   newdengue$date1<=input$slider_time)
    point.df_time <- data.frame(
      Lat = cumtime$緯度更,
      Long = cumtime$經度更
    )
    m <- leaflet(point.df_time) %>%
      addTiles() %>%
      setView(lng = 120.239, lat = 22.992, zoom = 12) %>%
      addMarkers(
        clusterOptions = markerClusterOptions(),popup = cumtime$確診日
      )
    m
    
  })
  output$map_people_time=renderLeaflet({
    cumtime_people=subset(people,
                          people$新日期<=input$slider_time)
    point.df_time_people <- data.frame(
      Lat = cumtime_people$新緯度,
      Long = cumtime_people$新經度
    )
    m <- leaflet(point.df_time_people) %>%
      addTiles() %>%
      setView(lng = 120.239, lat = 22.992, zoom = 12) %>%
      addMarkers(
        clusterOptions = markerClusterOptions(),popup = paste0(cumtime_people$日期,"<br>"," 噴工數:",cumtime_people$噴工數)
      )
    m
  })
  output$map_time_period=renderLeaflet({
    cumtime_period=subset(newdengue,
                          newdengue$date1<=input$slider_time_period&
                            newdengue$date1>=(input$slider_time_period-9))
    point.df_time_period <- data.frame(
      Lat = cumtime_period$緯度更,
      Long = cumtime_period$經度更
    )
    m <- leaflet(point.df_time_period) %>%
      addTiles() %>%
      setView(lng = 120.239, lat = 22.992, zoom = 12) %>%
      addMarkers(
        clusterOptions = markerClusterOptions(),popup = cumtime_period$確診日
      )
    m
    
  })
  output$map_people_time_period=renderLeaflet({
    cumtime_people_period=subset(people,
                                 people$新日期<=input$slider_time_period&
                                   people$新日期>=(input$slider_time_period-9))
    point.df_time_people_period <- data.frame(
      Lat = cumtime_people_period$新緯度,
      Long = cumtime_people_period$新經度
    )
    m <- leaflet(point.df_time_people_period) %>%
      addTiles() %>%
      setView(lng = 120.239, lat = 22.992, zoom = 12) %>%
      addMarkers(
        clusterOptions = markerClusterOptions(),popup = paste0(cumtime_people_period$日期,"<br>"," 噴工數:",cumtime_people_period$噴工數)
      )
    m
  })
  output$show_data_1=renderPrint({
    s=subset(newdengue,
             newdengue$date1>=input$daterange[1] &
               newdengue$date1<=input$daterange[2])
    b=subset(s,s$新區別 %in% as.character(input$new_distinct))
    
    table(b[,"新區別"])
  })
  output$show_people_1=renderPrint({
    h=subset(people,
             people$新日期>=input$daterange[1] &
               people$新日期<=input$daterange[2])
    t=subset(h,h$更區域 %in% as.character(input$new_distinct))
    table(t[,"更區域"])
  })
  output$ring_1=renderPlot({
    s=subset(newdengue,
             newdengue$date1>=input$daterange[1] &
               newdengue$date1<=input$daterange[2])
    b=subset(s,s$新區別 %in% as.character(input$new_distinct))
    aa=sort(table(b$新區別),decreasing = T)
    names(aa)
    dat=data.frame(Distinct=names(aa),count=as.numeric(aa))
    dat$fraction = dat$count / sum(dat$count)
    dat$ymax = cumsum(dat$fraction)
    dat$ymin = c(0, head(dat$ymax, n=-1))
    
    p1 = ggplot(data=dat, aes(fill=Distinct, ymax=ymax, ymin=ymin, xmax=7, xmin=5)) +
      geom_rect() +
      coord_polar(theta="y") +
      xlim(c(0, 7)) +
      theme(panel.grid=element_blank()) +
      theme(axis.text=element_blank()) +
      theme(axis.ticks=element_blank()) +
      annotate("text", x = 0, y = 0, label = " ") +
      labs(title=paste0(c(as.character(input$new_distinct),
                          "罹患登革熱比例"),
                        collapse = "")
           ,x="",y="")
    p1
    
  })
  output$ring_people_1=renderPlot({
    h=subset(people,
             people$新日期>=input$daterange[1] &
               people$新日期<=input$daterange[2])
    t=subset(h,h$更區域 %in% as.character(input$new_distinct))
    aaaa=sort(table(t$更區域),decreasing = T)
    dat_people_1=data.frame(Distinct=names(aaaa),count=as.numeric(aaaa))
    dat_people_1$fraction= dat_people_1$count /sum(dat_people_1$count)
    dat_people_1$ymax = cumsum(dat_people_1$fraction)
    dat_people_1$ymin = c(0, head(dat_people_1$ymax, n=-1))
    
    p_people_1 = ggplot(data=dat_people_1, aes(fill=Distinct, ymax=ymax, ymin=ymin, xmax=7, xmin=5)) +
      geom_rect() +
      coord_polar(theta="y") +
      xlim(c(0, 7)) +
      theme(panel.grid=element_blank()) +
      theme(axis.text=element_blank()) +
      theme(axis.ticks=element_blank()) +
      annotate("text", x = 0, y = 0, label = " ") +
      labs(title=paste0(c(as.character(input$new_distinct),
                          "登革熱噴藥比例"),
                        collapse = ""),x="",y="")
    p_people_1
  })
  output$histogram_1=renderPlot({
    detach("package:plotly", unload = TRUE)
    s=subset(newdengue,
             newdengue$date1>=input$daterange[1] &
               newdengue$date1<=input$daterange[2])
    b=subset(s,s$新區別 %in% as.character(input$new_distinct))
    if(length(input$new_distinct)==0){
      
    }else{
      layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
      
      # Draw the boxplot and the histogram 
      par(mar=c(0, 4, 1.1, 2.1))
      boxplot(b$date1 ,
              horizontal=TRUE ,
              frame=F,
              xaxt="n")
      par(mar=c(4, 4, 1.1, 2.1))
      hist(b$date1 ,
           breaks="weeks" ,
           col=rgb(0.2,0.8,0.5,0.5) ,
           xlab = "日期",
           freq=T,
           ylab="臺南確診登革熱的人數(人)",
           main=(paste0(c(as.character(input$new_distinct),
                          "確診登革熱直方圖"),
                        collapse = "")) )
      grid()
    }
    library("plotly")
    
  })
  output$histogram_people_1=renderPlot({
    detach("package:plotly", unload = TRUE)
    h=subset(people,
             people$新日期>=input$daterange[1] &
               people$新日期<=input$daterange[2])
    t=subset(h,h$更區域 %in% as.character(input$new_distinct))
    if(length(input$new_distinct)==0){
      
    }else{
      layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
      
      # Draw the boxplot and the histogram 
      par(mar=c(0, 4, 1.1, 2.1))
      boxplot(t$新日期 ,
              horizontal=TRUE ,
              frame=F,
              xaxt="n")
      par(mar=c(4, 4, 1.1, 2.1))
      hist(t$新日期 ,
           breaks="weeks" ,
           col=rgb(0.2,0.8,0.5,0.5) ,
           xlab = "日期",
           freq=T,
           ylab="臺南登革熱噴藥次數(次)",
           main=(paste0(c(as.character(input$new_distinct),
                          "登革熱噴藥直方圖"),
                        collapse = "")) )
      grid()
    }
    library("plotly")
  })
  output$map_1=renderLeaflet({
    s=subset(newdengue,
             newdengue$date1>=input$daterange[1] &
               newdengue$date1<=input$daterange[2])
    b=subset(s,s$新區別 %in% as.character(input$new_distinct))
    point.df_2 = data.frame(
      Lat = b$緯度更,
      Long = b$經度更
    )
    m <- leaflet(point.df_2) %>%
      addTiles() %>%
      setView(lng = 120.239, lat = 22.992, zoom = 12) %>%
      addMarkers(
        clusterOptions = markerClusterOptions(),popup = b$確診日
      )
    m
  })
  output$map_people_1=renderLeaflet({
    h=subset(people,
             people$新日期>=input$daterange[1] &
               people$新日期<=input$daterange[2])
    t=subset(h,h$更區域 %in% as.character(input$new_distinct))
    point.df_people_2 = data.frame(
      Lat = t$新緯度,
      Long = t$新經度
    )
    m <- leaflet(point.df_people_2) %>%
      addTiles() %>%
      setView(lng = 120.239, lat = 22.992, zoom = 12) %>%
      addMarkers(
        clusterOptions = markerClusterOptions(),popup = paste0(t$日期,"<br>"," 噴工數:",t$噴工數)
      )
    m
  })
}
shinyApp(ui=ui,server = server)
