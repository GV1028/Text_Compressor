
encodeMessage<-function(message,rel)
{
  library(qualV)
  library(tools)
  library(stringr)
  library(stringi)
  library(parallel)
  message<-toString(message)
  checkset<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z",2,3,4,5,6,7,8,9,"!","\"","#","$","%","&","'","(",")","*","+",",","-",".","/",":",";","<","=",">","?","@","[","\\","]","^","_","`","{","|","}","~")
  checknum<-c("0","1")
  chkmess<-unlist(strsplit(message,""))
  leftout<-checkset[!checkset %in% chkmess]
  leftnum<-checknum[!checknum %in% chkmess]
  encflag<-0
  
  if(length(leftout)>=2 || length(leftnum)>0)
  {
    encflag<-1
    message<-gsub("0",leftout[1],message)
    message<-gsub("1",leftout[2],message)
  }
  k<-1
  abs<-rel*nchar(message)/100
  
  
  message<-gsub("\\(","\\\\(",message)
  message<-gsub("\\[","\\\\[",message)
  message<-gsub("\\{","\\\\{",message)
  message<-gsub("\\n","\\\\n",message)
  
  k<-1
  #c1<-list()
  c1freq<-vector()
  messsplit<-unlist(strsplit(message,""))
  c1<-unique(messsplit)
  i<-1
  c1gen<-function(x){
    
    c1freq[i]<<-sum(str_count(message,fixed(x)))
    
    i<<-i+1
  }
  sapply(c1,c1gen)
  
  c2<-substring(message, seq(1, nchar(message)-1, 1), seq(2, nchar(message), 1))
  c2<-unique(c2)
  c2freq<-vector()
  i<-1
  c2gen<-function(x){
    c2freq[i]<<-sum(str_count(message,fixed(x)))
    i<<-i+1
  }
  sapply(c2,c2gen)
  c2<-c2[c2freq>abs]
  c2freq<-c2freq[c2freq>abs]
  
  ck<-list()
  ckfreq<-list()
  ck[[1]]<-c1
  ck[[2]]<-unlist(c2)
  ckfreq<-list()
  ckfreq[[1]]<-c1freq
  ckfreq[[2]]<-unlist(c2freq)
  
  k<-2
  cnt<-0
  flag<-1
  while(flag==1)
  {
    ck[[k+1]]<-""
    ckfreq[[k+1]]<-0 
    l<-0
    for(i in 1:length(ck[[k]]))
    {
      end<-substr(ck[[k]][i],k-cnt,k)
      for(j in 1:length(ck[[k]]))
      {
        
        start<-substr(ck[[k]][j],1,k-1)
        if(i!=j & end==start)
        {      
          l<-l+1  
          temp<-""    
          
          
          temp<-paste(temp,ck[[k]][i],sep="")
          
          
          temp<-paste(temp,unlist(str_split(ck[[k]][j],""))[k],sep="")
          
          if(grepl(temp,message,fixed=TRUE)!=TRUE)
          {
            l<-l-1
          }
          else
          {
            if(sum(str_count(message,fixed(temp)))>abs)
            {
              ck[[k+1]][l]<-temp
              ckfreq[[k+1]][l]<-sum(str_count(message,fixed(temp)))
            }
            else
              l<-l-1  
          } 
        } 
      } 
    } 
    if(l==0)
      flag<-0
    else
    {
      k<-k+1
      cnt<-cnt+1  
    } 
  }
  
  
  ck<-ck[1:length(ck)-1]
  ckfreq<-ckfreq[1:length(ckfreq)-1]
  pat<-unlist(ck)
  sup<-unlist(ckfreq)
  freq<-cbind(pat,sup)
  #View(freq)
  freq<-as.data.frame(freq)
  freq[,1]<-as.character(freq[,1])
  
  #View(freq)
  freq[,2]<-as.character(freq[,2])
  freq[,2]<-as.numeric(freq[,2])
  freqitems<-freq
  
  
  new<-list()
  new<-freq[nchar(freq[,1])==1,]
  l1<-length(new[,1])+1
  
  minimalgen<-function(x)
  {
    if(nchar(x)>1){
      
      k<-1
      sub<-list()
      for(i in 0:nchar(x)-1){
        sub[k]<-substr(x,i+1,i+nchar(x)-1)
        k<-k+1
        
      }
      sub<-unique(sub[sub!=x])[unique(sub[sub!=x])!="" & nchar(sub)==nchar(x)-1]
      sub<-sub[sapply(sub,function(x) {!is.null(x)})]
      flag<-0
      for(j in 1:length(sub))
      {
        c<-match(sub[[j]],new[,1])
        c1<-match(x,freq[,1])
        
        if(!is.na(c))
        {
          if(new[c,2]==freq[c1,2])
            flag<-1
        }
      }
      
      if(flag==0) 
      {
        new<<-rbind(new,freq[c1,]) 
        
      }
    }
    
  }
  sapply(freq[,1],minimalgen)
  
  split<-freq[which.max(freq[nchar(freq[,1])==1,2]),1]
  
  if(split=="\\")
    k<-str_split(message,"\\\\")
  else
    k<-str_split(message,split)
  trans<-list()
  u<-1
  i<-1
  punc<-"!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
  text<-message
  while(i<=length(k[[1]])) {
    trans[u]<-""
    if(k[[1]][i]=="") {
      j<-i
      while(j<=length(k[[1]])) {
        if(k[[1]][j]=="") {
          trans[u]<-paste(split,trans[u],sep="")
          
          
        }
        else {
          if(u!=1)
            trans[u]<-paste(split,trans[u],sep="")
          break
        }
        j<-j+1
      }
      i<-j
      u<-u+1
      if(u==2)
      { 
        u<-u-1       
      }
      else
      {
        u<-u+1
        trans[u]<-k[[1]][i]
        u<-u+1 
        i<-i+1
      }
      
    }
    else {
      if(i!=1)
      {
        trans[u]<-paste(split,k[[1]][i],sep="")
      } 
      else
      {
        trans[u]<-k[[1]][i]
        text<-gsub(k[[1]][i],"",text,fixed=TRUE)
      }
      u<-u+1
      i<-i+1
      
    }
  }
  
  trans<-unlist(trans)
  trans<-trans[!is.na(trans)]
  h<-as.data.frame(trans)
  h[,1]<-as.character(h[,1])
  
  
  
  h<-as.data.frame(h[order(sapply(h[,1],str_length)),])
  h[,1]<-as.character(h[,1])
  t<-list()
  g<-h
  i<-1
  txgen<-function(x)
  {
    t[i]<<-""
    v<-vector()
    len<-str_length(x)
    j<-1
    txopr<-function(y)
    {
      
      flag<-0
      check<-y
      for(k in 1:len)
      {
        char<-substr(x,k,k)
        if(grepl(char,check,fixed=TRUE)==1)
        {
          check<-gsub(char,"",check,fixed=TRUE)
          
        }
        else
        {
          if(k==len)
            flag<-1
          break
        }
      }
      if(k==len & flag==0)
        v<<-c(v,j)
      j<<-j+1
    }
    sapply(g[,1],txopr)
    
    if(is.null(v) || length(v)==0)
      v<-NaN
    t[[i]]<<-v
    i<<-i+1
  }
  sapply(new[,1],txgen)
  
  
  itx<-list()
  i<-1
  itxfunc<-function(x)
  {
    common<-as.character(h[x[1],1])
    
    #h[t[[i]][1],1]<-as.character(h[t[[i]][1],1])	
    if(length(x)>1)
    {
      
      lcsfunc<-function(y)
      {
        common<<-LCS(as.character(unlist(str_split(common,""))),as.character(unlist(str_split(h[y,1],""))))$LCS
      }
      sapply(x,lcsfunc)
      
    }
    common<-as.character(common)
    
    if(is.null(common) || is.na(common) || length(common)==0) 
      itx[[i]]<<-NaN
    else
      itx[[i]]<<-common
    
    i<<-i+1
  }
  sapply(t,itxfunc)
  
  items<-list()
  i<-1
  pasteitx<-function(x)
  {
    items[i]<<-x[1]
    if(length(x)>1)
    {
      j<-1
      pst<-function(y)
      {
        if(j>1)
          items[i]<<-paste(items[i],y,sep="")
        j<<-j+1
      }
      sapply(x,pst)
    }
    i<<-i+1
  }
  sapply(itx,pasteitx)
  
  itx<-items
  #View(itx)
  y<-list()
  i<-1
  narem<-function(x)
  {
    if(!is.nan(x))
    {
      flag<-0
      comparena<-function(y)
      {
        
        if(grepl(x,h[y,1],fixed=TRUE)!=TRUE)
        {
          flag<<-1
          
        }  
      }
      sapply(t[[i]],comparena)
      if(flag==1)
        y[[i]]<<-NaN
      else
        y[[i]]<<-x
    }
    else
      y[[i]]<<-NaN
    
    i<<-i+1
  }
  sapply(itx,narem)
  itx<-y
  
  itxf<-cbind(itx,t)
  itxf<-unique(itxf)
  freq<-list()
  
  for(i in 1:length(itxf[,1]))
  { if(!is.nan(itxf[i,1][[1]]))
  {
    if(!is.na(itxf[i,1][[1]]) & str_length(itxf[i,1][[1]])>1)
    {
      fr<-length(itxf[i,2][[1]])
      for(j in 1:length(itxf[,1]))
      {
        if(!is.nan(itxf[j,1][[1]]))
        {	
          if(j!=i & itxf[i,1][[1]]!=itxf[j,1][[1]] & grepl(itxf[i,1][[1]],itxf[j,1][[1]],fixed=TRUE)==1)
          {
            fr<-fr-length(itxf[j,2][[1]])
          }
          
        }}
      
      
      #if(grepl(itxf[i,1][[1]],message)!=1)
      #freq[i]<-0
      if(fr<0)
        freq[i]<-0
      else
        freq[i]<-fr
    }
    else
      freq[i]<-0
  }
    else
      freq[i]<-0
  } 
  
  
  itxf<-itxf[,1]
  itxp<-cbind(itxf,freq)
  itxp<-as.data.frame(itxp)
  itxp<-itxp[sapply(itxp[,2],function(x) {!is.null(x)}),]
  itxp<-itxp[sapply(itxp[,2],function(x) x!=0),]
  itxp<-itxp[sapply(itxp[,1],function(x) !is.nan(x)),]
  #View(itxp)
  itxp<-unique(itxp)
  #View(itxp)
  
  onelen<-freqitems[nchar(freqitems[,1])==1,]
  onefreq<-list()
  for(i in 1:length(onelen[,1]))
  {
    fr<-onelen[i,2]
    for(j in 1:length(itxp[,1]))
    {
      if(str_length(unlist(itxp[,1])[j])>1)
      {
        if(grepl(onelen[i,1],unlist(itxp[,1])[j],fixed=TRUE)==1)
        {
          mat<-gregexpr(onelen[i,1],unlist(itxp[,1])[j],fixed=TRUE)
          fr<-fr-(itxp[j,2][[1]]*length(mat[[1]]))
        }
      }
    }
    if(fr<0) 
      onelen[i,2]<-0
    else
      onelen[i,2]<-fr
  }  
  onelen<-onelen[onelen[,2]!=0,]
  colnames(onelen)<-c("itxf","freq")
  itemsfinal<-rbind(onelen,itxp)
  frequency<-as.numeric(unlist(itemsfinal[,2]))
  set<-unlist(itemsfinal[,1])
  itemsfinal<-as.data.frame(itemsfinal)
  itemsfinal[,1] <- as.character(itemsfinal[,1])
  itemsfinal[,2] <- as.integer(itemsfinal[,2])
  #View(itemsfinal)
  code<-vector()
  bhuffman<-function(pvecin)
  {
  pvecin<-as.numeric(pvecin)
  pvec=pvecin[which(pvecin>0)];
  
  # Make couple of checks
  if (class(pvec) != "numeric") stop("Input must be numeric");
  if (length(pvec) < 2) stop("Numbers of positive elements must be 2 or more");
  
  # Recursion stop condition if the number of rows exactly 2
  if (length(pvec) == 2) code<<-c("0","1")
  else
  {
    # Compress low probabilities values and call again. Use last min
    min1loc <- length(pvec)-which.min(rev(pvec))+1;
    min1 <- pvec[min1loc];
    pvec <- pvec[-min1loc];
    # min2loc <- which.min(pvec);
    min2loc <- length(pvec)-which.min(rev(pvec))+1;
    min2 <- pvec[min2loc];
    pvec <- pvec[-min2loc];
    pvec <- append(pvec,min1+min2);
    
    code <<- bhuffman(pvec);
    
    # Get the last entry code, remove it and add split it back to previous entries
    min1code <- paste0(tail(code,n=1),"0");
    min2code <- paste0(tail(code,n=1),"1");
    # Remove last entry and add this code to the min1, min2 locations
    code <<- code[-length(code)];
    # Add back the new codes
    code <<- append(code,min2code,min2loc-1);
    code <<- append(code,min1code,min1loc-1);
  }
  }
  bhuffman(itemsfinal[,2])
  huffd<-itemsfinal[,1]
  huffd<-cbind(huffd,code)
  huffd<-as.data.frame(huffd[order(sapply(huffd[,1],str_length),decreasing=TRUE),])
  encmes<-""
  if(encflag==1)
  {
    for(i in 1:length(huffd[,1]))
    {
      message<-gsub(huffd[i,1],huffd[i,2],message,fixed=TRUE)
    }
    encmes<-message
  }
  else
  {
    encode<-matrix(ncol=2)
    randvect<-matrix(nrow=str_length(message),ncol=1)
    i<-1
    encfunc<-function(x)
    { 
      t<-gregexpr(x,message,fixed="TRUE")
      
      temp<-function(y,z)
      {
        if(is.na(randvect[y,1]))
        {
          v<-c(y,i)
          encode<<-rbind(encode,v)
          st<-y
          fin<-y+z-1
          for(k in st:fin)
          {
            randvect[k,1]<<-1 
          } 
        }    
        
      }
      
      mapply(temp,t[[1]],attr(t[[1]],"match.length")[1])
      i<<-i+1  
    }
    sapply(huffd[,1],encfunc)
    encode<-encode[2:length(encode[,1]),]
    encode<-encode[order(encode[,1]),]
    #View(encode)
    
    
    for(i in 1:length(encode[,1]))
    {
      string<-huffd[encode[i,2],2]
      encmes<-paste(encmes,string,sep="")  
    } 
  }
  
  m<-strsplit(encmes,"")
  var<-""
  l<-""
  h<-function(x){
    var<<-paste(var,x,sep="")
    #print(var)
    chk<-match(var,huffd[,2])
    if(!is.na(chk))
    {
      l<<-paste(l,huffd[chk,1],sep="")
      var<<-""
    }
  }
  sapply(m[[1]],function(x) h(x))
  
  #print(l)
  l<-gsub("\\\\n","\n",l)
  l<-gsub("\\\\","\\",l)
  l<-gsub(leftout[1],"0",l)
  l<-gsub(leftout[2],"1",l)
  result<-list(enc=encmes,dec=l,codetable=huffd)
  return (result)
}
shinyServer(
  function(input, output) {
    output$encmessage<- renderPrint({
      if (input$goButton == 0) "You have not pressed the button"
      if (input$goButton >= 1) encodeMessage(input$text1,input$id1)$enc
    })
    output$codetab<- renderDataTable({
      if (input$goButton == 0) "You have not pressed the button"
      if (input$goButton >= 1) encodeMessage(input$text1,input$id1)$codetable
    })
    output$decmessage<- renderPrint({
      if (input$goButton == 0) "You have not pressed the button"
      if (input$goButton >= 1) encodeMessage(input$text1,input$id1)$dec
    })
  }
)