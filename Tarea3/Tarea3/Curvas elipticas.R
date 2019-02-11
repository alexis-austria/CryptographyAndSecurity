#En todas las funciones se supondra una curva eliptica E:y^{2}=x^{3}+a*x+b, dado un modulo

#funcion para obtener los puntos de la curva eliptica E, devuelve una matriz de orden |E| por 2 con cada renglon un punto de E

PuntosElipticos<-function(a,b,modulo){
  v<-numeric()
for (j in 0:modulo-1) {
  w<-j^{3}+a*j+b
  for (i in 0:modulo-1) {
    u<-i^{2}
    if (u %% modulo == w %% modulo){
     v<-c(v,c(j,i))
    }
  }
}
  M<-matrix(v,length(v)/2,2,byrow = T)
  M
}

#Funcion para medir cuantos puntos tiene la curva eliptica en Z_modulo, devuelve |E|

CantPuntosElipticos<-function(a,b,modulo){
  v<-numeric()
  for (j in 0:modulo-1) {
    w<-j^{3}+a*j+b
    for (i in 0:modulo-1) {
      u<-i^{2}
      if (u %% modulo == w %% modulo){
        v<-c(v,c(j,i))
      }
    }
  }
  E<-length(v)/2
  E
}

#ejemplos de listas de puntos en una curva eliptica dada

PuntosElipticos(1,9,17)
PuntosElipticos(-20,21,35)
CantPuntosElipticos(1,9,17)
CantPuntosElipticos(-20,21,35)

#Funcion que obtiene el inverso de un numero n dado un modulo, devuelve su inverso en Z*_modulo, si no tiene inverso devuelve un cero

InversoMod<-function(n,modulo){
  x<-numeric(1)
  for (r in 1:modulo) {
    if ((n*r) %% modulo == 1){
      x<-r
    } 
  }
  x
}

#ejemplos de inverso dado un modulo

InversoMod(5,15)
InversoMod(7,15)

#Funcion para obtener una lista de los inversos de cada numero n dado un modulo, devuelve las parejas (n,n^{-1}) si existe o (n,0) si no existe

ListaDeInversos<-function(modulo){
  v<-numeric()
for (n in 1:modulo) {
  v<-c(v,c(n,InversoMod(n,modulo)))
  }
  M<-matrix(v,length(v)/2,2,byrow = T)
  M
}

#ejemplos de lista de inversos

ListaDeInversos(35)

#funcion para obtener -P, la resta de un punto en la curva eliptica

NegEliptica<-function(P){
  c(P[1],-P[2])
}

#funcion suma sobre la curva eliptica el punto (1/2,1/2) es el punto al infinito, si no se puede sumar entonces devuelve el valor (1/4,1/4)

SumaEliptica<-function(P,Q,a,mod){
  x1<-P[1]
  y1<-P[2]
  x2<-Q[1]
  y2<-Q[2]
  W<-numeric(2)
  decision<-0
  if(x1==1/2) {
    W<-c(x2,y2)
  } else{
    if(x2==1/2){
    W<-c(x1,y1)
  } else{
    if((x1%%mod)==(x2%%mod)){
      if((y1%%mod)==(-y2%%mod)){
        W<-c(1/2,1/2)
      }  else{
        if(InversoMod(y1,mod)==0){
          decision<-1
        }else{
          u<-numeric(1)
          u<-((3*(x1^{2})+a)*InversoMod(2*y1,mod))
          x3<-((u^{2}-2*x1)%%mod)
          y3<-((-y1+u*(x1-x3))%%mod)
          W<-c(x3,y3)
        }
      }
    }   else{
      if(InversoMod(x2-x1,mod)==0){
        decision<-1
      }else{
        v<-numeric(1)
        v<-((y2-y1)*InversoMod(x2-x1,mod))
        x3<-((v^{2}-x1-x2)%%mod)
        y3<-((-y1+v*(x1-x3))%%mod)
        W<-c(x3,y3)
      }
    }
  }
  }
  if(decision==0){
    W 
  } else {
    W<-c(1/4,1/4)
    W
  }
}

#ejemplos de suma eliptica

SumaEliptica(c(11,12),c(7,-11),1,17)
SumaEliptica(c(15,-4),c(15,-4),-20,35)
SumaEliptica(c(0,14),c(15,-4),-20,35)


#funcion de multiplicacion por escalares naturales sobre la curva eliptica

#funcion que calcula un multiplo de un divisor del modulo

MomentoPosibleDiv<-function(P,a,modulo){
y<-P
memoria<-numeric()
memoria<-0
i<-0
while ((memoria==0)&&(i<modulo) ){
  i<-i+1
  if(y[1]==1/8){ 
    if(memoria==0){ break
      print(i-1)
    } 
  } else{
    memoria<-memoria
    if(SumaEliptica(P,y,a,modulo)[1]==1/4){
      y<-c(1/8,1/8)
    } else{
      y<-SumaEliptica(P,y,a,modulo)
    }
  }
  }
i
}


MomentoPosibleDiv(c(0,3),1,17)
MomentoPosibleDiv(c(15,-4),-20,35)
MomentoPosibleDiv(c(0,14),-20,35)

min(c(4,5,7,8))

MultEliptica<-function(P,k,a,modulo){
  y<-P
  if(k==1){
    y<-P
  } else{
    for (i in 2:k) {
    if(y[1]==1/8){
        y<-y
    } else{
      if(SumaEliptica(P,y,a,modulo)[1]==1/4){
      y<-c(1/8,1/8)
    } else{
      y<-SumaEliptica(P,y,a,modulo)
    }
  }
  }
  }
  y
}    
    
    
    


#ejemplos de multiplicacion sobre la curva eliptica

PosibleDivisor<-function(P,a,modulo){
    x1<-P[1]
    y1<-P[2]
    j<-MomentoPosibleDiv(P,a,modulo)
    Q<-MultEliptica(P,j-1,a,modulo)
    x2<-Q[1]
    y2<-Q[2]
    
    if(((x2==1/2) || (x2==1/4)) || (x2==1/8)){
      d<-y1
    } else{
        d<-1
        if((x1%%modulo)==(x2%%modulo)){
          if((y1%%modulo)==(-y2%%modulo)){
            d<-1/2 
            } else{
        d<-(2*y1%%modulo)
        }
          } else{ 
            d<-((x2-x1)%%modulo)
          }
        }
  d
}

PosibleDivisor(c(15,-4),-20,35)
PosibleDivisor(c(15,-4),-20,35)
PosibleDivisor(c(0,14),-20,35)
InversoMod(4,35)
InversoMod(2,35)

5%%5


MultEliptica(c(15,-4),2,-20,35)
MultEliptica(c(15,-4),3,-20,35)
MultEliptica(c(15,-4),4,-20,35)
MultEliptica(c(15,-4),5,-20,35)
MultEliptica(c(15,-4),8,-20,35)

MultEliptica(c(13,3),3,17)

if(c(3,3)==c(3,3)){
  2+3
}

#funcion que da los puntos sobre una orbita

OrbitaEliptica<-function(P,a,modulo){
  v<-P
  for (i in 1:MomentoPosibleDiv(P,a,modulo)-1) {
    if((MultEliptica(P,i,a,modulo)[1]==1/4)||(MultEliptica(P,i,a,modulo)[1]==1/8)){
      v<-v
    } else{
      v<-c(v,MultEliptica(P,i+1,a,modulo))
    }
  }
  M<-matrix(v,length(v)/2,2,byrow = T)
  M
}

MultEliptica(c(0,3),2,1,17)
OrbitaEliptica(c(15,-4),-20,35)
OrbitaEliptica(c(0,3),1,17)
OrbitaEliptica(c(14,7),35)
OrbitaEliptica(c(1,5),35)
