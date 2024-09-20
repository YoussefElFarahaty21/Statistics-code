printfn=function()
{
  print("Enter item code from 1 to 50")
  print("Press 0 to exit")
}
inputfn=function()
{
  printfn()
  choice=as.integer(readline(prompt=))
  selecteditems=c()
  #choice is limited between 1 and 50 only,0 to exit
  while(choice!=0)
  {
    printfn()
    if(choice>0 & choice<=50)
    {
      selecteditems=append(selecteditems,choice)
      choice=as.integer(readline(prompt=))
    }
    else
    {
      print("incorrect input")
      choice=as.integer(readline(prompt=))
    }
  }
  return(selecteditems)
}
countitems=function(items)
{
  #create table to count how many times each item selected(frequency)
  tab=table(items)
  #use as.integer function to names of table to return each item number
  itemno=as.integer(names(tab))
  quantity=as.integer(tab)
  count=data.frame(Items=itemno,Quantity=quantity)
  return(count)
}
getprice=function(countitem,prices)
{
  #send df which stores (item no and quantity )and prices from original df
  price=c()
  for(i in 1:nrow(countitem))
  {
    price=append(price,(countitem[i,2]*prices[countitem[i,1]]))
  }
  return (price)
}
calreciept=function(selecteditem,prices)
{
  #add price column to count item df
  countitem=countitems(selecteditem)
  price=getprice(countitem,prices)
  countitem=cbind(countitem,price)
  return(countitem)
}
summary=function(reciept)
{
  #create matrix to store :
  #No of items,Mean,Min Max,Required to pay prices
  mat=matrix(NA,nrow=5,ncol=1,byrow=FALSE)
  row.names(mat)=c("Number of items","Mean price of all items","Min item price","Max item price","Required to pay")
  colnames(mat)=c("")
  totalprice=sum(reciept[,3])
  no_ofitems=sum(reciept[,2])
  mean=totalprice/no_ofitems
  mat[1,1]=no_ofitems
  mat[2,1]=mean
  mat[3,1]=min(reciept[,3])
  mat[4,1]=max(reciept[,3])
  mat[5,1]=totalprice
  return(mat)
}
printreciept=function(selecteditem,prices)
{
  reciept=calreciept(selecteditem,prices)
  summary=summary(reciept)
  print(reciept,row.names=FALSE)
  print(summary)
}
print("Please choose the database file")
data=read.csv(file.choose())
itemnumbers=inputfn()
printreciept(itemnumbers,data$Price)

