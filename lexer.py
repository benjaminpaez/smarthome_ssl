import ply.lex as lex
import re

# ==========================================
# 1. Palabras Reservadas
# ==========================================

# Palabras Reservadas (Control y Lógica)
reserved_control = {
    'when': 'WHEN',
    'every': 'EVERY',
    'do': 'DO',
    'end': 'END',
    'if': 'IF',
    'then': 'THEN',
    'else': 'ELSE',
    'and': 'AND',
    'or': 'OR',
    'not': 'NOT'
}

# Palabras Reservadas de Atributos (Propiedades de dispositivos)
reserved_attributes = {
    'estado': 'ESTADO',
    'brillo': 'BRILLO',
    'color': 'COLOR',
    'modo': 'MODO',
    'temp_obj': 'TEMP_OBJ',
    'temp_act': 'TEMP_ACT',
    'posicion': 'POSICION',
    'hora': 'HORA_ATTR',   # Diferenciado de TOKEN_HORA
    'fecha': 'FECHA_ATTR', # Diferenciado de TOKEN_FECHA
    'volumen': 'VOLUMEN',
    'mute': 'MUTE',
    'mensaje': 'MENSAJE',
    'email_notif': 'EMAIL_NOTIF',
    'activada': 'ACTIVADA'
}

# Booleanos y Discretos
reserved_literals = {
    'true': 'TRUE',
    'false': 'FALSE',
    'on': 'ON',
    'off': 'OFF',
    'rojo': 'ROJO',
    'blanco': 'BLANCO',
    'azul': 'AZUL',
    'frio': 'FRIO',
    'calor': 'CALOR',
    'vent': 'VENT'
}

# Unidades (Sufijos)
reserved_units = {
    'lux': 'LUX',
    'm': 'M',
    's': 'S',
    'h': 'H'
}

# Unimos todas las palabras reservadas en un solo diccionario
reserved = {}
reserved.update(reserved_control)
reserved.update(reserved_attributes)
reserved.update(reserved_literals)
reserved.update(reserved_units)

# ==========================================
# 2. Lista de Tokens
# ==========================================

tokens = [
    # Operadores y Delimitadores
    'IGUALDAD', 'DISTINTO', 'MAYOR_IGUAL', 'MENOR_IGUAL', 'MAYOR', 'MENOR', 'ASIGNACION', 'PUNTO',
    
    # Identificadores Estrictos de Dispositivos (Actuadores y Sensores)
    'FOCO_ID', 'AIRE_ID', 'PERSIANA_ID', 'CERRADURA_ID', 'RELOJ_ID', 'ALTAVOZ_ID', 'ALARMA_ID',
    'SENSOR_ID_TEMP', 'SENSOR_ID_HUMEDAD', 'SENSOR_ID_LUZ', 'SENSOR_ID_MOVIMIENTO', 'SENSOR_ID_HUMO',
    
    # Literales Complejos
    'TOKEN_NUMERO', 'TOKEN_HORA', 'TOKEN_FECHA', 'TOKEN_EMAIL', 'TOKEN_TEXTO',
    
    # Unidades Especiales
    'PORCENTAJE', 'GRADOS_C',
    
    # Identificador Genérico
    'ID',
    
    # Comentarios
    'COMMENT'
] + list(reserved.values())

# ==========================================
# 3. Expresiones Regulares Simples
# ==========================================

t_IGUALDAD    = r'=='
t_DISTINTO    = r'!='
t_MAYOR_IGUAL = r'>='
t_MENOR_IGUAL = r'<='
t_MAYOR       = r'>'
t_MENOR       = r'<'
t_ASIGNACION  = r'='
t_PUNTO       = r'\.'

t_PORCENTAJE  = r'%'

# Caracteres a ignorar (espacios, tabulaciones y retornos de carro)
t_ignore = ' \t\r'

# ==========================================
# 4. Reglas Complejas (Funciones)
# ==========================================

def t_COMMENT(t):
    r'//.*'
    return t

def t_TOKEN_FECHA(t):
    r'\d{2}/\d{2}/(?:20[0-9]{2})'
    return t

def t_TOKEN_HORA(t):
    r'(?:[01]?[0-9]|2[0-3]):[0-5][0-9]'
    return t

def t_TOKEN_EMAIL(t):
    r'[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}'
    return t

def t_TOKEN_NUMERO(t):
    r'-?\d+(\.\d+)?'
    # No convertimos a float aquí para mantener el string en t.value, 
    # pero el parser podrá hacerlo luego.
    return t

def t_TOKEN_TEXTO(t):
    r'"[^"]*"'
    return t

def t_GRADOS_C(t):
    r'°[cC]'
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    
    val_lower = t.value.lower()
    
    # Comprobar si es palabra reservada
    if val_lower in reserved:
        t.type = reserved[val_lower]
        return t
        
    # Comprobar si es un identificador de dispositivo
    dispositivos = {
        'foco': 'FOCO_ID',
        'aire': 'AIRE_ID',
        'persiana': 'PERSIANA_ID',
        'cerradura': 'CERRADURA_ID',
        'reloj': 'RELOJ_ID',
        'altavoz': 'ALTAVOZ_ID',
        'alarma': 'ALARMA_ID',
        'sensor_temp': 'SENSOR_ID_TEMP',
        'sensor_humedad': 'SENSOR_ID_HUMEDAD',
        'sensor_luz': 'SENSOR_ID_LUZ',
        'sensor_movimiento': 'SENSOR_ID_MOVIMIENTO',
        'sensor_humo': 'SENSOR_ID_HUMO'
    }
    
    for prefix, tipo in dispositivos.items():
        if val_lower == prefix or val_lower.startswith(prefix + '_'):
            t.type = tipo
            return t
            
    # Si no es dispositivo ni reservada, es un ID común
    t.type = 'ID'
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print(f"Error Léxico: Caracter no reconocido '{t.value[0]}' en la línea {t.lexer.lineno}")
    t.lexer.skip(1)

# Construir el analizador léxico con ignorar mayúsculas/minúsculas en las regex
lexer = lex.lex(reflags=re.IGNORECASE)

# ==========================================
# 5. Bloque Principal - Consola Interactiva
# ==========================================

if __name__ == '__main__':
    print("Iniciando Smart-Home Lexer Console...")
    print("Escribe 'salir', 'exit' o 'quit' para terminar.\n")
    
    while True:
        try:
            # Pedir entrada al usuario
            data = input('SMART-HOME > ')
            
            # Condición de salida
            if data.strip().lower() in ['salir', 'exit', 'quit']:
                print("Saliendo de la consola...")
                break
                
            if not data.strip():
                continue
                
            # Pasar la entrada al lexer
            lexer.input(data)
            
            # Procesar e imprimir los tokens
            while True:
                tok = lexer.token()
                if not tok:
                    break # No hay más tokens
                # Imprimir la información requerida
                print(f"Tipo: {tok.type:<20} Valor: {tok.value:<15} Línea: {tok.lineno} Posición: {tok.lexpos}")
                
        except EOFError:
            print("\nEOF detectado. Saliendo...")
            break
        except KeyboardInterrupt:
            print("\nInterrupción del usuario. Saliendo...")
            break
