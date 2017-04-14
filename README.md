# DM-Pro

首先将工作目录设置为当前目录   例如:```setwd(‘/home/DM_Proj1/')```

之后执行```source('preconditioning.R')``` 即可运行写好的代码

变量说明：  
+ 读入的新闻数据：dataframe
+ BagOfWords格式：m
+ 相似矩阵：cosine_matrix
+ 平均相似度向量：inclass_sim

函数说明
+ 求类别A与类别B的平均相似度调用函数```cal_ave_sim(A,B,class_list,cosine_matrix)
```

运行前请保证 XML、NLP、tm、RColorBrewer、wordcloud、ggplot2等包已安装
