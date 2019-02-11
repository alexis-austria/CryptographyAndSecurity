import java.math.*;


public class Hill {

static String alfabetoMayusculas = "ABCDEFGHIJKLMNÑOPQRSTUVWXYZ";

/**
 *
 */
private static String codificar(String clave, String mensaje) {
        String rtnCadena = "";

        //1. Construcción y verificación de la Matriz correspondiente a la clave
        int[][] claveMatriz = getMatrizClave(clave);

        //2. Construcción de n-gramas del mensaje
        int[][] claveMensaje = getMatrizMensaje(mensaje, claveMatriz[0].length);


        return rtnCadena;
}

/**
 *
 * @param clave
 * @return
 */
private static int[][] getMatrizClave(String clave){
        int[][] rtnClave = new int[0][0];

        double raizDouble = Math.sqrt( clave.length() );
        if (raizDouble == (int)raizDouble) {

                int raizInt = (int)raizDouble;
                rtnClave = new int[raizInt][raizInt];
                int flagClave = 0;
                for(int i=0; i<raizInt; i++) {
                        for(int j=0; j<raizInt; j++) {
                                rtnClave[i][j] = alfabetoMayusculas.indexOf(clave.charAt(flagClave));
                                flagClave++;
                        }
                }

        }
        else{ //no es exacto la raíz cuadrada del la clave
                System.out.println("Llave invalida, no se puede formar una matriz de NxN");
                // rtnClave[i][j] = null;
        }

        return rtnClave;
}

/**
 *
 * @param mensaje
 * @return
 */
private static int[][] getMatrizMensaje(String mensaje, int longitudeMatriz){
        int[][] rtnMensaje = new int[0][0];

        if( mensaje.length()%longitudeMatriz == 0 ) {
                int column = mensaje.length() / longitudeMatriz;
                rtnMensaje = new int[column][longitudeMatriz];

                int flagMesaje = 0;
                for(int i=0; i<column; i++) {
                        for(int j=0; j<longitudeMatriz; j++) {
                                rtnMensaje[i][j] = alfabetoMayusculas.indexOf(mensaje.charAt(flagMesaje));
                                flagMesaje++;
                        }
                }

        }else{
                System.out.println("Mensaje invalido, no se puede formar una matrices de Nx1");
        }

        return rtnMensaje;
}

/**
 *
 */
private static String decodificar(String cadena, int[][] llave, int dimension) {
        String rtnCadena = "";
        int det = calculaDeterminante(llave);
        int[] valor = new int[cadena.length()];

        if (det == 0 ) {
                System.out.println("No es posible decifrar el mensaje, dado que el determinante es 0");
        } else {

                for (int i = 0; i < cadena.length(); i++) {
                        valor[i] = alfabetoMayusculas.indexOf(cadena.charAt(i));
                }
                int[][] inversa = calculaInversa(llave,det);
                int[] resultado = new int[dimension];

                for(int i = 0; i + dimension <= cadena.length(); i = i + dimension) {
                        for(int n = 0; n < dimension; n++ ) {
                                for (int m = 0; m < dimension; m++) {
                                        resultado[n] += inversa[n][m]*valor[i + m];
                                }
                        }
                        for(int j = 0; j < dimension; j++) {
                                resultado[j] = ((resultado[j] % 27) + 27) % 27;
                                rtnCadena += alfabetoMayusculas.charAt(resultado[j]);
                                resultado[j] = 0;
                        }
                }
        }
        return rtnCadena;
}

/**
 *
 *
 */
public static int calculaDeterminante(int[][] matriz) {


        if (matriz.length == 1) {

                return matriz[0][0];

        } else {

                int determinante = 0;

                for (int i = 0; i < matriz.length; i++) {
                        int[][] cofactor = calculaCofactor(matriz,0,i);
                        determinante = determinante + (int)Math.pow(-1, i)* (matriz[0][i])*(calculaDeterminante(cofactor));
                }

                return determinante;
        }

}

/**
 *
 *
 */
public static int[][] calculaCofactor(int[][] matriz, int renglon, int columna) {

        int[][] cofactor = new int[matriz[0].length - 1][matriz[0].length - 1];

        for (int i = 0; i < columna; i++) {
                for (int j = 0; j < renglon; j++) {
                        cofactor[j][i] = matriz[j][i];
                }
        }

        for (int i = columna + 1; i < matriz.length; i++ ) {
                for (int j = 0; j < renglon; j++) {
                        cofactor[j][i - 1] = matriz[j][i];
                }
        }

        for (int i = renglon + 1; i < matriz.length; i++) {
                for (int j = 0; j < columna; j++) {
                        cofactor[i - 1][j] = matriz[i][j];
                }
        }

        for (int i = columna + 1; i < matriz.length; i++) {
                for (int j = renglon + 1; j < matriz.length; j++) {
                        cofactor[j - 1][i - 1] = matriz[j][i];
                }
        }

        return cofactor;
}

/**
 *
 *
 */
public static int[][] calculaInversa(int[][] matriz,int det){

        int[][] inversa = new int[matriz.length][matriz.length];
        int a_inv = 0;
        int flag = 0;
        for (int i = 0; i < 27; i++) {
                flag = (det * i) % 27;
                if (flag == 1) {
                        a_inv = i;
                }
        }

        for (int i = 0; i < matriz.length; i++) {
                for (int j = 0; j < matriz.length; j++) {
                        int[][] cofactor = calculaCofactor(matriz,i,j );
                        inversa[j][i] = ((a_inv*((int)Math.pow(-1,i + j)*calculaDeterminante(cofactor)) % 27) + 27) % 27;
                }
        }
        return inversa;
}



/**
 * Método principal
 */
public static void main(String[] args) {
        String mensaje = "PHUGDRKYH";
        int[][] matriz = {{1,2,3},{1,1,6},{1,2,1}};
        // String clave   = "FORTALEZA";
        //
        // // 1. Mensaje original
        // System.out.println("Mensaje original: " + mensaje);
        // System.out.println("Clave:            " + clave);
        // System.out.println();
        //
        // // 2. Codificar
        // String mensajeCodificado = codificar(clave, mensaje);
        // System.out.println("Mensaje codificado:   " + mensajeCodificado);

        // 3. Decodificar
        String cadenaDecodificada = decodificar(mensaje, matriz, 3);
        System.out.println("Texto decodificado: " + cadenaDecodificada);

}

}
