printfn=function(data)
{
  print("Which columns do you require to compute on?")
  colname=names(data)
  for(i in 1:length(colname)){
    print(paste("Press ",i," For: ",colname[i]))
  }
  print("Press 0 to exit")
}
inputfn1=function(data)
{
  colname=names(data)
  printfn(data)
  choice=as.integer(readline(prompt=))
  selectedcols=c()
  #choice limited(not=0,and less than or equal length of columns' names)
  while(choice!=0)
    {
    if(!choice<=length(colname))
    {
      print("Incorrect input")
    }
    else
    {
      selectedcols=append(selectedcols,choice)
    }
    printfn(data)
    choice=as.integer(readline(prompt=))
  }
  #table in order to prevent repeating choices 
  t=table(selectedcols)
  scols=as.integer(names(t))
  return(scols)
}
printfn2=function()
{
  print("what do you want to calculate?")
  print("1. Count    2. Mean   3. Std")
  print("4. Min      5. 25%    6. 50%")
  print("7. 75%      8. Max")
  print("Press 0 to exit")
}
inputfn2=function()
{
  printfn2()
  choice=as.integer(readline(prompt=))
  selectedop=c()
  #choice is limited (not 0, and less than or equal 8 (operations))
  while(choice!=0)
  {
    if(!choice<=8)
    {
      print("Incorrect input")
    }
    else
    {
      selectedop=append(selectedop,choice)
    }
    printfn2()
    choice=as.integer(readline(prompt=))
  }
  #table in order to prevent repeating choices 
  t=table(selectedop)
  scols=as.integer(names(t))
  return(scols)
}

selectedopfn=function(data,op)
{
result=0
   if(op==1)
   {
     #return NA if not number instead of error 
     result=sum(as.integer(data))
     return(result)
   }
   else if(op==2)
   {
     result=mean(data)
     return(result)
   }
   else if(op==3)
   {
     result=sd(data)
     return(result)
   }
   else if(op==4)
   {
     result=min(data)
     return(result)
   }
   else if(op==5)
   {
     result=quantile(data,probs=0.25)
     return(result)
   }
   else if(op==6)
   {
     result=quantile(data,probs=0.5)
     return(result)
   }
   else if(op==7)
   {
     result=quantile(data,probs=0.75)
     return(result)
   }
   else if(op==8)
   {
     result=max(data)
     return(result)
   }
 return(result)
}
rownamefn=function(selectedop)
{
  #return name of slected operations
  rows=c()
  for(i in 1:length(selectedop))
  {
    if(selectedop[i]==1)
    {
      rows=append(rows,"Count")
    }
    else if(selectedop[i]==2)
    {
      rows=append(rows,"Mean")
    }
    else if(selectedop[i]==3)
    {
      rows=append(rows,"Std")
    }
    else if(selectedop[i]==4)
    {
      rows=append(rows,"Min")
    }
    else if(selectedop[i]==5)
    {
      rows=append(rows,"25%")
    }
    else if(selectedop[i]==6)
    {
      rows=append(rows,"50%")
    }
    else if(selectedop[i]==7)
    {
      rows=append(rows,"75%")
    }
    else if(selectedop[i]==8)
    {
      rows=append(rows,"Max")
    }
  }
  return(rows)
}
output=function(selectedcols,selectedop,data)
{
  cols=c()
  rows=rownamefn(selectedop)
  result=matrix(NA,nrow=length(selectedop),ncol=length(selectedcols))
  for(i in 1:length(selectedcols)){
    #use names function to store names of selected col
    cols=append(cols,names(data[selectedcols[i]]))
  }
  #let col.row names of matrix be names of selected cols,operations
  colnames(result)=cols
  rownames(result)=rows
  for(k in 1:length(selectedop))
  {
    for(j in 1:length(selectedcols))
    {
        #select from df selected columns only
        #select operations by index of selectedop vector
      result[k,j]=selectedopfn(data[,selectedcols[j]],selectedop[k])
    }
  }
  print(result)
}
print("Please choose the database file")
data=read.csv(file.choose())
selectedcols=inputfn1(data)
selectedop=inputfn2()
output(selectedcols,selectedop,data)