import random
import fractions


def gcd_extendido(aa, bb):
    """ Es algoritmo de euclides extendido, es decir
    Obtenemos al gcd, ademas obtenemos lo factores de aa y bb tales que podemos expresar a gcd
    como combinacion lineal de aa y bb
    Parametros
    aa -- entero
    b -- entero"""
    # Dado que el gcd se preservar bajo el valor absolutos
    A, B = abs(aa), abs(bb)
    x, ultimox, y, ultimoy = 0, 1, 1, 0
    while (B != 0):
        A, (cociente, B) = B, divmod(A, B)
        x, ultimox = ultimox - cociente * x, x
        y, ultimoy = ultimoy - cociente * y, y
    return A, ultimox * (-1 if aa < 0 else 1), ultimoy * (-1 if bb < 0 else 1)


def suma(A, B, a, mod):
    """ Calcula la suma de los puntos A y B
    El metodo regresa un arreglo, donde la primer entrada indica con un boolean si se encontro un factor del modulo
    en caso de que esto ocurra la segunda entrada es un factor del modulo, en caso contrario la segunda entrada regresa el resultado de la suma
    el cual es un punto de la curva.
    Notese que hay un caso en el cual si A = - B o A = B = 0(Donde 0 indica el neutro del grupo) entonces el resultado es 0. En este caso la funcion regresa
    un arreglo donde la primer entreda es un false el cual significa que no encontro un factor del modulo y la segunda entrada es True, lo cual indica
    que el resultado es 0.
    Parametros
    A -- Primer punto
    B -- Segundo punto
    a -- Este numero determina la curva eliptica
    mod -- Este indica en que Grupo trabajamos """
    if (A == True and not(B == True)): # Caso en el que A es el neutro del Grupo
        return False, B
    elif (B == True and not(A == True)): # Caso en el que B es el neutro del Grupo
        return False, A
    elif (B == True and A == True): # Caso en el que los dos son el neutro del Grupp
        return False, True # En este caso la funcion regresa un arreglo donde la segunda entrada indica que el resultado es el neutro del grupo

    if (A[0] == B[0] and A[1] == (-B[1] % mod)): # Caso donde A = -B
        return False, True

    lam = 0
    if (A[0] == B[0] and A[1] == B[1]):# Caso donde A = B
        lam = 3 * A[0] * A[0] + a
        z = gcd_extendido(2 * A[1], mod)
        if (not(z[0] == 1)): # Si el gcd no es 1 hemos encontrado un factor del modulo
            return True, z[0]

        lam = (lam * z[1]) % mod
    else:
        lam = B[1] - A[1]
        z = gcd_extendido(B[0] - A[0], mod)
        if (not(z[0] == 1)): # Si el gcd no es 1 hemos encontrado un factor del modulo
            return True, z[0]

        lam = (lam * z[1]) % mod
    # Calculamos las coordenadas
    x = (lam * lam - A[0] - B[0]) % mod
    y = (lam * (A[0] - x) - A[1]) % mod
    resultado = [x, y]
    return False, resultado


def multiplo(A, entero, a, mod):
    """ Metodo encargado de calcular el Multiplo de A, es decir (entero)A
        Parametros
        A -- Punto al que se le calculara su Multiplo
        entero -- Indica que multiplo debe ser calculado
        a -- Este numero determina la curva eliptica
        mod -- Este indica en que Grupo trabajamos

    """
    multiplo = A
    for i in range(1, entero):
        resultado = suma(multiplo, A, a, mod)
        if (resultado[0] == True):# Si encontramos un factor del modulo
            return resultado
        else:
            multiplo = resultado[1]

    return False, multiplo


def lenstra(n):
    """ Funcion que implementa el algorimto de Lenstra.
        Notese que dado dos coordenas x , y , junto un entero a y un modulo n, determinan una unica curva eliptica
        Con la ecuacion b = y^2 - x^3 - a*x mod n   o  y^2 = x^3 + a*x + b mod n, Por lo que no es necesario determinar el entero b
        y tampoco hace falta determinar que el punto (x,y) este en una curva."""
    m = 100 # Este el numero de multiplos que seran calculados por cada curva elipitca
    j = 0
    while(True): # El algoritmo se repetira hasta encontrar los factores
        j = j + 1
        a = random.randint(0, 1000000000) # Esto nos genera curvas aleatorias
        A = [random.randint(0, 1000), random.randint(0, 1000)] # Un punto en la curva eliptica
        for i in range(2, m):
            resultado = multiplo(A, i, a, n)
            if(resultado[0] == True):
                return resultado[1], n / resultado[1]
            elif(resultado[1] == True): # Si el multiplo del punto es el neutra usa una nueva curva
                break
            else:
                A = resultado[1] # Para calcular siguiente multiplo

        if (j % 100 == 0 ): # En caso de que hayamos usado mas de 100*n(donde n es un entero) curvas elipiticas aumentaremos el numero de multiplos a calcular.
            m = m + 15






def main():
    """ Metodo Principal
        Integrantes
        Felipe De Jesus Cova Pacheco
        Fernando Fortanel Rojas
        """
    A = lenstra(1073561597)
    print "Los factores de 1073561597 son " , A[0], " y ", A[1]

    # 272396328951937621 = 165044357*1650443153
    B = lenstra(272396328951937621)
    print "Los factores de 272396328951937621 son ", B[0], " y ", B[1]


if __name__ == '__main__':
    main()
