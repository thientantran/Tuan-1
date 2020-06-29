tienDien.function=function(kWh){
  donGia1=1678
  donGia2=1734
  donGia3=2014
  donGia4=2536
  donGia5=2834
  donGia6=2927
  if(kWh<=50){
    tienThanhToan=donGia1*kWh
  }else if(kWh<=100){
    tienThanhToan=donGia1*50+donGia2*(kWh-50)
  }else if(kWh<=200){
    tienThanhToan=donGia1*50+donGia2*50+donGia3*(kWh-100)
  }else if(kWh<=300){
    tienThanhToan=donGia1*50+donGia2*50+donGia3*100+donGia4*(kWh-200)
  }else if(kWh<=400){
    tienThanhToan=donGia1*50+donGia2*50+donGia3*100+donGia4*100+donGia5(kWh-300)
  }else{
    tienThanhToan=donGia1*50+donGia2*50+donGia3*100+donGia4*100+donGia5*100+donGia6*(kWh-400)
  }
}
snt.function=function(n){
  dem=0
  for(i in c(1:n)){
    if(n%%i==0){
      dem=dem+1
    }
  }
  if(dem==2){
    kq=TRUE
  }else{
    kq=FALSE
  }
  return(kq)
}
thuePhong.function=function(loaiPhong,soNgayO){
  v1=1000000
  v2=900000
  v3=850000
  s1=700000
  s2=550000
  donGia=switch(loaiPhong,v1,v2,v3,s1,s2)
  tien=0;
  if(soNgayO<3){
    tien=soNgayO*donGia
  }else if(soNgayO<7){
    tien=soNgayO*donGia*0.9
  }else{
    tien=soNgayO*donGia*0.8
  }
  return(tien)
}
thueXe.function=function(dichVu,soKM){
  giaCuoc1=c(10000,3600)
  giaCuoc2=c(15000,4000)
  giaCuoc=switch(dichVu,giaCuoc1,giaCuoc2)
  tienThanhToan=giaCuoc[1]
  if(soKM>2){
    tienThanhToan=giaCuoc[1]+giaCuoc[2]*(soKM-2)
  }
  return(tienThanhToan)
}