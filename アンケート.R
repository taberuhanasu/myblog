library(plyr)
library(tidyverse)

d01r <- data.frame(ID=1:35,
                 Q01=c(2, 6, 1, 2, 6, 6, 6, 2, 2, 5, 5, 5, 1, 1, 5, 2, 2, 4, 6, 2,
                       3, 2, 6, 6, 5, 1, 2, 6, 1, 1, 1, 2, 3, 5, 3),
                 Q02=c(1, 1, 1, 1, 1, 2, 2, 1, 2, 2, 2, 1, 2, 2, 1, 1, 2, 2, 1, 1,
                       1, 1, 2, 2, 1, 1, 1, 2, 2, 1, 2, 1, 2, 2, 1),
                 Q03=c(2, 2, NA, 2, 1, 1, 1, 2, 1, 1, 1, 1, 2, 2, 1, 1, 1, 2, 1, 2,
                       2, 2, 1, 1, 2, 1, 1, 1, 1, 2, 2, 2, 1, 2, 2),
                 Q04=c(10, 50, 1, 100, 2, 0.1, 1, 100, 0.5, 1, 1, 0.1, 25, 10,
                       0.01,0.5, 0.1, 10, 0.1, 6, 100, 100, 5, 5, 18, 1, 5, 1, 5,
                       1.5, 2, 100,0.01, 6, 1.5),
                 Q05_01=c(0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1,
                          0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0),
                 Q05_02=c(0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0,
                          1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0),
                 Q05_03=c(1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0,
                          1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1),
                 Q05_04=c(1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1,
                          1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0))

d01q <- data.frame(qID=c("Q01", "Q02", "Q03", "Q04", "Q05"),
                 question=c("あなたの実家はどこですか？",
                            "たこ焼き器はありますか？",
                            "1 万円札を拾ったら交番に届けますか？",
                            "いくら以上なら交番に届けますか？",
                            "あなたの性格は？（複数回答）"),
                 graph=c("単一回答", "単一回答", "単一回答", "数値記入", "複数回答"),
                 order=c("大", "同", "同", "", "大"),
                 stringsAsFactors = FALSE)

d01a<-data.frame(Q01=c("1. 京都府", "2. 大阪府", "3. 兵庫県", "4. 滋賀県",
                       "5. 奈良県", "6. その他"),
                 Q02=c("1. 有り", "2. 無し", "", "", "", ""),
                 Q03=c("1. 届ける", "2. 届けない", "", "", "", ""),
                 Q04=c(0,1,10,50,100,NA),
                 Q05=c("1. メニューは最後",
                       "2. すぐに後悔する",
                       "3. 行き先が決められない",
                       "4. 押しに弱い", "", ""))




i <- 1
  
  
qID<-d01q[,"qID"]
d02r<-select(d01r,contains(qID[i]))
d02q<-d01q[i,"question"]
d02g<-d01q[i,"graph"]
d02a_0<-d01a[,qID[i]]
d02a_0<-d02a_0[d02a_0!=""]
colnames(d02r)<-"aID"
na01<-length(d02a_0)
d02a<-data.frame(aID=1:na01,xtext=d02a_0)
d02<-join(d02r,d02a,by="aID")
t02<-plyr::count(d02)
p02<-transform(t02,prop=freq/sum(freq))

               
               
#データ抽出
d02r<-select(d01r,contains(qID[i])) #i 番目の回答データ
ttl01<-paste(d02q,"\n","（",d02g,"N=",sum(p02$freq),"）")
d02q<-d01q[i,"question"] #i 番目の設問文
d02g<-d01q[i,"graph"] #i 番目のグラフの種類
d02_ord<-d01q[i,"order"] #i 番目の x の並べ方
d02a_0<-d01a[,qID[i]] #i 番目の選択肢
d02a_0<-d02a_0[d02a_0!=""] #選択肢から空白を削除
na01<-length(d02a_0) #選択肢の数をカウント
#xy を与えるデータを作成
colnames(d02r)<-"aID"
d02a<-data.frame(aID=1:na01,xtext=d02a_0)
d02<-join(d02r,d02a,by="aID") #選択肢番号を文に変換
t02<-plyr::count(d02) #集計
p02<-transform(t02,prop=freq/sum(freq)) #構成比を求める

#カラーパレットの指定
cb_palette<-c("#0072B2","#F0E442","#009E73","#56B4E9",
              "#CC79A7","#D55E00","#E69F00","#999999")

#グラフを作る関数
gfbars<-theme_bw(base_family = "HiraKakuPro-W3")+ #白黒基調の組み込みテーマを使う
  theme(panel.border=element_blank(), #描画領域の枠線を消す
        panel.grid=element_blank(), #目盛線を消す
        axis.title=element_blank(), #軸タイトルを消す
        axis.ticks.y=element_blank(), #Y 軸（縦軸？）の目盛を消す
        axis.text.y=element_text(size=10), #Y 軸（縦軸？）ラベルの文字サイズ
        axis.line.x=element_line(colour="grey"))#軸線を grey で書き足す

ggbars<-ggplot(p02, #データを指定
               aes(x=reorder(x=xtext,X=xord), #X 軸は xtext を xord の順番で
                   y=prop))+ #Y 軸は prop のデータを
  coord_flip()+ #横棒グラフに　
  scale_y_continuous(labels=scales::percent, #Y 軸の目盛は％表記
                     limits=c(0,1))+ #Y 軸の範囲は 0～1
  geom_text( #データラベルを記入
    aes(y=prop, #ラベルの位置
        label=sprintf("%.1f%%",prop*100)), #prop*100 を、数点以下 1 桁で％表記
    size=4, #ラベルの文字サイズは 4
    hjust=-0.1)+ #ラベルの位置調整、0.1 大きく
  ggtitle(ttl01)+ #グラフタイトル
  guides(fill=FALSE) 

#X 軸の並べ方を指定
if(d02_ord=="同"){
    xord<--p02$aID
    }else if(d02_ord=="大"){
      xord<-p02$prop
      xord[grepl("その他",p02$xtext)]<--1
      }else{
        xord<--xord01
        }
#グラフの定義
ttl01<-paste(d02q,"\n","（",d02g,"N=",sum(p02$freq),"）")#グラフタイトル
ggbars<-ggplot(p02, #データを指定
                aes(x=reorder(x=xtext,X=xord),#X 軸は xtext を xord の順番で
                    y=prop))+ #Y 軸は prop のデータを
  coord_flip()+ #横棒グラフに　
  scale_y_continuous(labels=scales::percent, #Y 軸の目盛は％表記
                     limits=c(0,1))+ #Y 軸の範囲は 0～1
  geom_text( #データラベルを記入
    aes(y=prop, #ラベルの位置
        label=sprintf("%.1f%%",prop*100)), #prop*100 を、数点以下 1 桁で％表記
    size=4, #ラベルの文字サイズは 4
    hjust=-0.1)+ #ラベルの位置調整、0.1 大きく
  ggtitle(ttl01)+ #グラフタイトル
  guides(fill=FALSE) #凡例は非表示
#グラフを表示

ggbars+gfbars+geom_bar(stat="identity",fill=cb_palette[1])


#関数の場合
fgsingle_bar<-function(dr,dq,da,qNo,
                       xord01=NULL,theme=gfbars,palette=cb_palette[1]){
   #データ抽出
     d02r<-select(dr,contains(qID[qNo])) #i 番目の回答データ
     d02q<-dq[qNo,"question"] #i 番目の設問文
     d02g<-dq[qNo,"graph"] #i 番目のグラフの種類
     d02_ord<-dq[qNo,"order"] #i 番目の x の並べ方
     d02a_0<-da[,qID[qNo]] #i 番目の選択肢
     d02a_0<-d02a_0[d02a_0!=""] #選択肢から空白を削除
     na01<-length(d02a_0) #選択肢の数をカウント
    
       #xy を与えるデータを作成
       colnames(d02r)<-"aID"
       d02a<-data.frame(aID=1:na01,xtext=d02a_0)
       d02<-join(d02r,d02a,by="aID") #選択肢番号を文に変換
       t02<-plyr::count(d02) #集計
       p02<-transform(t02,prop=freq/sum(freq)) #構成比を求める
      
         #X 軸の並べ方を指定
         if(d02_ord=="同"){
           xord<--p02$aID
           }else if(d02_ord=="大"){
             xord<-p02$prop
             xord[grepl("その他",p02$xtext)]<--1
             }else{
               xord<--xord01
               }
      
         #グラフの定義
         ttl01<-paste(d02q,"\n","（",d02g,"N=",sum(p02$freq),"）")#グラフタイトル
         ggbars<-ggplot(p02, #データを指定
                          aes(x=reorder(x=xtext,X=xord),#X 軸は xtext を xord の順番で
                                y=prop))+ #Y 軸は prop のデータを
           coord_flip()+ #横棒グラフに　
           scale_y_continuous(labels=scales::percent, #Y 軸の目盛は％表記
                                limits=c(0,1))+ #Y 軸の範囲は 0～1
           geom_text( #データラベルを記入
             aes(y=prop, #ラベルの位置
                   label=sprintf("%.1f%%",prop*100)), #prop*100 を、数点以下 1 桁で％表記
             size=4, #ラベルの文字サイズは 4
             hjust=-0.1)+ #ラベルの位置調整、0.1 大きく
           ggtitle(ttl01)+ #グラフタイトル
           guides(fill=FALSE) #凡例は非表示
        
           #グラフを表示
           ggbars+theme+geom_bar(stat="identity",fill=palette)
         }
fgsingle_bar(dr=d01r,dq=d01q,da=d01a,qNo=1)
