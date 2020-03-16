quickSort::(Ord a)=>[a]->[a]
quickSort []=[]
quickSort (x:xs)
 let smallpart=quickSort [a|a<-xs,a<=x]
     bigpart=quc [a|a<-xs,a>x]
 in smallpart++[x]++bigpart