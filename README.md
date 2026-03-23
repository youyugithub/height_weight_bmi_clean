# height_weight_bmi_clean

```
big_weight_change<-function(weights,days){
  
  idx<-!(is.na(weights)|is.na(days))
  if(sum(idx)<=1){
    return(ifelse(is.na(weights),NA_character_,"good"))
  }else if(sum(idx)<=2){
    weights_sub<-weights[idx]
    days_sub<-days[idx]
    
    slope<-abs(weights_sub[2]-weights_sub[1])/abs(days_sub[2]-days_sub[1])
    if(slope<3/30){
      result_sub<-rep("good",2)
    }else{
      result_sub<-rep("potential",2)
    }
    result<-rep(NA_character_,length(days))
    result[idx]<-result_sub
    return(result)
  }else{
    weights_sub<-weights[idx]
    days_sub<-days[idx]
    
    slope_sub<-rep(NA,length(weights_sub))
    for(ii in 2:(length(weights_sub)-1)){
      change_left<-abs(weights_sub[ii]-weights_sub[ii-1])
      change_right<-abs(weights_sub[ii+1]-weights_sub[ii])
      slope_sub[ii]<-(change_left+change_right)/(days_sub[ii+1]-days_sub[ii-1])
    }
    result_sub<-ifelse(abs(slope_sub)>3/30,"outlier","good")
    
    change_right12<-abs(weights_sub[2]-weights_sub[1])
    change_right13<-abs(weights_sub[3]-weights_sub[1])
    slope_right12<-change_right12/(days_sub[2]-days_sub[1])
    slope_right13<-change_right13/(days_sub[3]-days_sub[1])
    change_leftn1<-abs(weights_sub[length(weights_sub)]-weights_sub[length(weights_sub)-1])
    change_leftn2<-abs(weights_sub[length(weights_sub)]-weights_sub[length(weights_sub)-2])
    slope_leftn1<-change_leftn1/(days_sub[length(weights_sub)]-days_sub[length(weights_sub)-1])
    slope_leftn2<-change_leftn2/(days_sub[length(weights_sub)]-days_sub[length(weights_sub)-2])
    
    result_sub[1]<-dplyr::case_when(
      abs(slope_right12)>3/30&slope_right13>3/30~"outlier",
      abs(slope_right12)>3/30~"potential",
      abs(slope_right13)>3/30~"potential",
      TRUE~"good")
    
    result_sub[length(weights_sub)]<-dplyr::case_when(
      abs(slope_leftn1)>3/30&slope_leftn2>3/30~"outlier",
      abs(slope_leftn1)>3/30~"potential",
      abs(slope_leftn2)>3/30~"potential",
      TRUE~"good")
    
    result<-rep(NA_character_,length(days))
    result[idx]<-result_sub
    
    return(result)
  }
}

big_height_change<-function(heights,days){
  
  idx<-!(is.na(heights)|is.na(days))
  if(sum(idx)<=1){
    return(ifelse(is.na(heights),NA_character_,"good"))
  }else if(sum(idx)<=2){
    heights_sub<-heights[idx]
    days_sub<-days[idx]
    
    slope<-abs(heights_sub[2]-heights_sub[1])/abs(days_sub[2]-days_sub[1])
    if(slope<10/30){
      result_sub<-rep("good",2)
    }else{
      result_sub<-rep("potential",2)
    }
    result<-rep(NA_character_,length(days))
    result[idx]<-result_sub
    return(result)
  }else{
    heights_sub<-heights[idx]
    days_sub<-days[idx]
    
    slope_sub<-rep(NA,length(heights_sub))
    for(ii in 2:(length(heights_sub)-1)){
      change_left<-abs(heights_sub[ii]-heights_sub[ii-1])
      change_right<-abs(heights_sub[ii+1]-heights_sub[ii])
      slope_sub[ii]<-(change_left+change_right)/(days_sub[ii+1]-days_sub[ii-1])
    }
    result_sub<-ifelse(abs(slope_sub)>10/30,"outlier","good")
    
    change_right12<-abs(heights_sub[2]-heights_sub[1])
    change_right13<-abs(heights_sub[3]-heights_sub[1])
    slope_right12<-change_right12/(days_sub[2]-days_sub[1])
    slope_right13<-change_right13/(days_sub[3]-days_sub[1])
    change_leftn1<-abs(heights_sub[length(heights_sub)]-heights_sub[length(heights_sub)-1])
    change_leftn2<-abs(heights_sub[length(heights_sub)]-heights_sub[length(heights_sub)-2])
    slope_leftn1<-change_leftn1/(days_sub[length(heights_sub)]-days_sub[length(heights_sub)-1])
    slope_leftn2<-change_leftn2/(days_sub[length(heights_sub)]-days_sub[length(heights_sub)-2])
    
    result_sub[1]<-dplyr::case_when(
      abs(slope_right12)>10/30&slope_right13>10/30~"outlier",
      abs(slope_right12)>10/30~"potential",
      abs(slope_right13)>10/30~"potential",
      TRUE~"good")
    
    result_sub[length(heights_sub)]<-dplyr::case_when(
      abs(slope_leftn1)>10/30&slope_leftn2>10/30~"outlier",
      abs(slope_leftn1)>10/30~"potential",
      abs(slope_leftn2)>10/30~"potential",
      TRUE~"good")
    
    result<-rep(NA_character_,length(days))
    result[idx]<-result_sub
    
    return(result)
  }
}


```
