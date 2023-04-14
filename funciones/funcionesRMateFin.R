# funciones múltiples para matemáticas financieras:

#----------Anualidades vencidas

# Función para obtener la tasa conociendo el valor futuro:


tasaAnualidadVencida=function(VF,r,A,Tpers){
  
  #Valores de entrada y umbral
  umbral=1*10^-12
  iteraciones=0
  
  izquierda=VF/A
  derecha=((1+r)^Tpers-1)/r
  diferencia=izquierda-derecha
  
  if (diferencia>0){
    # Cuando la r inicial es menor a lo deseado:  
    # Algoritmo de alza de tasa:
    
    iteraciones=0
    
    while (diferencia>0){
      r1=r  
      iteraciones=iteraciones+1  
      r=r*1.01
      
      izquierda=VF/A
      derecha=((1+r)^Tpers-1)/r
      diferencia=izquierda-derecha
      cat("\f")
      print(paste0("Iteración ",iteraciones))
      
      r2=r
      
    }  
    
    # algoritmo de la bisección:
    
    #diferencia1=diferencia
    
    while (abs(diferencia)>umbral){
      iteraciones=iteraciones+1
      r=(r1+r2)/2    
      
      izquierda=VF/A
      derecha=((1+r)^Tpers-1)/r
      diferencia=izquierda-derecha
      
      if (diferencia<0){
        r1=r1
        r2=r
        
      } else {
        r2=r2  
        r1=r
        
      }
      
      cat("\f")
      print(paste0("Iteración: ",iteraciones))
    }
    
  } else {
    # Cuando la r inicial es mayor a lo deseado:  
    # Algoritmo de bajada de tasa:
    
    while (diferencia<0){
      r2=r 
      iteraciones=iteraciones+1  
      r=r/1.01
      
      izquierda=VF/A
      derecha=((1+r)^Tpers-1)/r
      diferencia=izquierda-derecha
      cat("\f")
      print(paste0("Iteración ",iteraciones))
      
      r1=r
      
    }  
    # algoritmo de la bisección:
    
    #diferencia1=diferencia
    
    while (abs(diferencia)>umbral){
      iteraciones=iteraciones+1
      r=(r1+r2)/2    
      
      izquierda=VF/A
      derecha=((1+r)^Tpers-1)/r
      diferencia=izquierda-derecha
      
      if (diferencia>0){
        r1=r
        r2=r2
        
      } else {
        r2=r  
        r1=r1
        
      }
      
      cat("\f")
      print(paste0("Iteración ",iteraciones))
    }
    
    
    
  }
  
  # Genera objeto de salida:
  objetoTasa=list(Iteraciones=iteraciones,tasa=r)
  
  return(objetoTasa)  
}