A<-0
B<-1
C<-2
D<-3
E<-4
F<-5
G<-6
H<-7
I<-8
J<-9
K<-10
L<-11
M<-12
N<-13
O<-14
P<-15
Q<-16
R<-17
S<-18
T<-19
U<-20
V<-21
W<-22
X<-23
Y<-24
Z<-25

modulo<-function(M){
  cosa<-M
  for (i in 1:dim(M)[2]) {
    cosa[1,i]<-M[1,i]-26*floor(M[1,i]/26)
    
    cosa[2,i]<-M[2,i]-26*floor(M[2,i]/26)}
  cosa
}


Desencriptar<-function(vectorEncriptado,MatrizDescifrado){
  A<-0
  B<-1
  C<-2
  D<-3
  E<-4
  F<-5
  G<-6
  H<-7
  I<-8
  J<-9
  K<-10
  L<-11
  M<-12
  N<-13
  O<-14
  P<-15
  Q<-16
  R<-17
  S<-18
  T<-19
  U<-20
  V<-21
  W<-22
  X<-23
  Y<-24
  Z<-25
  MatrizEncriptada<-matrix(vectorEncriptado, ncol = 2, byrow = F )
  MatrizDesencriptada<-modulo(MatrizDescifrado%*%t(MatrizEncriptada))
  VectorDesencriptado<-as.vector(MatrizDesencriptada)
  nombres<-c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
  Mensaje<-character()
  for (i in 1:length(VectorDesencriptado) ){
    Mensaje<-paste(Mensaje,nombres[VectorDesencriptado[i]+1])
  }
  Mensaje
}

Desencriptar(c(N,M,K,G,N,N,E,C,T,I,P,J,X,T,L,T,E,C,B,X,E,S,M,Q,R,W,L,S,N,Q,Y,A,L,X,N,I,L,G,Q,K,L,I,G,E,D,R,T,Z,N,A,E,S,F,F,L,V,E,D,A,O,X,J,L,X,A,W,N,M,G,E,Y,C,R,L,L,I,H,Z,R,W,L,K,N,C,W,M,E,E,L,G,L,I,A,O,W,G,N,P,R,W,Y,M,N,Q,Y,M,N,Q,W,A,G,E,N,M,N,S,W,G,U,S,X,N,H,F,Y,F,A,W,U,S,X,L,U,S,X,N,M,Q,R,W,Y,C,F,N,B,X,X,L,T,K,W,M,F,N,M,K,A,L,D,E,X,D,E,K,D,G,K,A,L,I,T,I,N,X,L,K,G,E,M,F,T,C,B,Z,U,S,E,K,L,W,L,T,X,J,T,V,F,N,J,Z,L,X,A,W,M,F,T,W,G,E,W,A,W,G,N,P,E,C,Q,Q,N,E,N,M,J,U,L,M,E,Q,M,A,Y,C,X,N,T,I,N,U,A,C,B,X,V,T,G,O,N,I,Y,I,B,Z,M,E,W,G,A,V,N,I,A,K,N,N,R,B,U,W,B,B,N,I,Y,U,T,I,P,R,A,D,G,E,N,H,J,Z,M,U,G,E,Y,C,Z,X,E,S,G,H,L,C,T,K,U,S,L,D,N,B,T,I,P,R,A,D,T,Y,G,O,N,A,F,F,D,E,L,S,Y,M,L,G,X,D,F,N,M,Q,F,L,D,E,T,Q,Y,I,O,J,G,E,Y,O,Q,I,J,Z,B,Z,A,D,Y,G,O,N,Y,A,L,V,U,S,F,N,H,H,L,Z,L,S,T,Y,T,Q,G,W,T,I,P,R,A,D,T,W,Q,G,F,N,E,Q,Y,N,W,O,N,H,Y,B,L,G,N,I,T,U,N,K,J,A,T,Y,Y,C,W,I,F,N,B,L,T,I,X,N,N,X,T,Q,Y,U,N,A,X,D,Y,J,T,Q,G,E,D,I,M,K,A,L,D,E,X,D,J,M,R,F,N,R,X,D,F,N,L,V,R,O,Q,K,L,I,N,U,B,Z,X,N,L,G,Y,I,B,L,N,C,F,P,N,N,O,F,L,G,T,I,W,O,N,P,B,X,Z,X,X,L,N,B,W,M,F,N,B,L,T,I,X,N,N,X,G,K,D,L,N,C,F,P,N,N,O,F,L,I,F,N,F,F,A,V,Y,C,L,O,L,O,Q,S,L,G,T,S,N,U,L,I,N,R,N,D,D,E,A,O,N,C,F,P,N,N,B,B,F,L,F,P,A,D,L,I,U,U,F,P,A,C,A,D,L,V,B,X,R,W,L,G,B,S,L,I,L,S,X,L,T,K,Y,M,N,I,Y,W,T,Q,L,S,M,F,Q,G,T,V,S,I,Y,I,A,D,T,Q,G,E,W,I,X,L,U,S,N,S,W,G,Q,S,X,N,M,K,S,I,Y,U,T,I,X,L,N,D,N,M,A,A,V,T,G,O,N,Q,W,A,N,I,L,S,N,X,U,U,L,N,N,M,X,T,L,T,O,L,Q,G,F,N,A,J,T,K,U,C,B,X,L,P,Y,K,A,J,J,M,J,C,A,W,M,F,T,W,G,E,W,A,W,G,N,P,E,W,Q,G,E,I,N,R,A,V,Y,U,J,U,J,G,B,Z,E,Q,R,W,Y,U,J,O,S,I,Y,C,W,O,B,T,W,I,X,L,U,S,Y,C,M,K,N,J,W,I,X,L,A,D,Y,C,W,C,Q,G,E,S,L,G,F,F,B,R,X,J,A,D,G,E,J,Y,Y,G,N,N,F,P,U,M,E,Y,N,N,F,N,Y,C,A,W,M,F,T,W,G,E,W,A,W,G,N,P,E,C,G,O,G,E,J,Y,Y,G,N,N,F,P,U,M,E,K,D,E,Y,W,A,O,W,G,N,D,T,Q,L,S,M,F,T,C,G,Q,Q,G,B,X,D,V,Z,X,X,L,E,S,R,W,L,S,M,K,A,D,J,M,Q,Y,Y,C,M,K,N,J,W,I,X,L,A,D,Y,U,T,I,B,X,N,S,W,G,U,S,X,N,M,Q,M,K,A,L,D,E,X,N,R,E,A,O,T,S,L,G,M,F,T,C,G,Q,Q,G,X,J,H,M,W,O,A,Z,G,K,E,K,D,E,Y,W,A,O,W,G,N,N,B,B,K,G,N,M,S,I,Y,I,B,X,V,T,G,O,N,I,Y,I,B,Z,M,E,W,G,N,J,L,W,L,T,X,J,F,N,D,E,N,O,L,N,Z,X,E,S,M,U,N,R,A,V,Y,U,J,U,J,G,B,Z,Z,D,L,T,K,L,N,U,N,Y,N,C,M,K,N,P,O,J,A,W,E,Q,M,F,T,W,T,I,Y,K,N,B,T,K,Z,X,L,T,Y,F,A,W,N,Y,W,A,D,L,E,S,R,E,Y,M,N,I,Y,G,V,T,G,O,A,W),matrix(c(24,13,19,15),ncol = 2,byrow = F))
