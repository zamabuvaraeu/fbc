#include once "need_ucase.bi"

Function NeedUCase( _
		byval src as const zstring ptr _
	)As Boolean
	
	Dim src1 As String = *src
	Dim Length As Integer = Len(src1)
	Dim srcU As String = UCase(src1)
	
	Scope
		If srcU = "ASSERT" Then
			Return True
		End If
		
		If srcU = "MACRO" Then
			Return True
		End If
		
		If srcU = "ALLOCATE" Then
			Return True
		End If
		
		If srcU = "CALLOCATE" Then
			Return True
		End If
		
		If srcU = "REALLOCATE" Then
			Return True
		End If
		
		If srcU = "DEALLOCATE" Then
			Return True
		End If
		
		If srcU = "ENDMACRO" Then
			Return True
		End If
		
		If srcU = "INCLIB" Then
			Return True
		End If
		
		If srcU = "UNDEF" Then
			Return True
		End If
		
		If srcU = "IFNDEF" Then
			Return True
		End If
		
		If srcU = "IFDEF" Then
			Return True
		End If
		
		If srcU = "DEFINE" Then
			Return True
		End If
		
		If srcU = "PRAGMA" Then
			Return True
		End If
		
		If srcU = "ONCE" Then
			Return True
		End If
		
		If srcU = "VA_LIST" Then
			Return True
		End If
		
		If srcU = "NAMEPACE" Then
			Return True
		End If
		
		If srcU = "FWDREF" Then
			Return True
		End If
		
		If srcU = "XMMWORD" Then
			Return True
		End If
		
		If srcU = "FALSE" Then
			Return True
		End If
		
		If srcU = "TRUE" Then
			Return True
		End If
		
		If srcU = "AND" Then
			Return True
		End If
		
		If srcU = "OR" Then
			Return True
		End If
		
		If srcU = "ANDALSO" Then
			Return True
		End If
		
		If srcU = "ORELSE" Then
			Return True
		End If
		
		If srcU = "XOR" Then
			Return True
		End If
		
		If srcU = "EQV" Then
			Return True
		End If
		
		If srcU = "IMP" Then
			Return True
		End If
		
		If srcU = "NOT" Then
			Return True
		End If
		
		If srcU = "MOD" Then
			Return True
		End If
		
		If srcU = "SHL" Then
			Return True
		End If
		
		If srcU = "SHR" Then
			Return True
		End If
		
		If srcU = "NEW" Then
			Return True
		End If
		
		If srcU = "DELETE" Then
			Return True
		End If
		
		If srcU = "REM" Then
			Return True
		End If
		
		If srcU = "DIM" Then
			Return True
		End If
		
		If srcU = "ABS" Then
			Return True
		End If
		
		If srcU = "SGN" Then
			Return True
		End If
		
		If srcU = "FIX" Then
			Return True
		End If
		
		If srcU = "FRAC" Then
			Return True
		End If
		
		If srcU = "INT" Then
			Return True
		End If
		
		If srcU = "STATIC" Then
			Return True
		End If
		
		If srcU = "SHARED" Then
			Return True
		End If
		
		If srcU = "BOOLEAN" Then
			Return True
		End If
		
		If srcU = "BYTE" Then
			Return True
		End If
		
		If srcU = "UBYTE" Then
			Return True
		End If
		
		If srcU = "SHORT" Then
			Return True
		End If
		
		If srcU = "USHORT" Then
			Return True
		End If
		
		If srcU = "INTEGER" Then
			Return True
		End If
		
		If srcU = "UINTEGER" Then
			Return True
		End If
		
		If srcU = "LONG" Then
			Return True
		End If
		
		If srcU = "ULONG" Then
			Return True
		End If
		
		If srcU = "LONGINT" Then
			Return True
		End If
		
		If srcU = "ULONGINT" Then
			Return True
		End If
		
		If srcU = "SINGLE" Then
			Return True
		End If
		
		If srcU = "DOUBLE" Then
			Return True
		End If
		
		If srcU = "STRING" Then
			Return True
		End If
		
		If srcU = "ZSTRING" Then
			Return True
		End If
		
		If srcU = "WSTRING" Then
			Return True
		End If
		
		If srcU = "UNSIGNED" Then
			Return True
		End If
		
		If srcU = "ANY" Then
			Return True
		End If
		
		If srcU = "PTR" Then
			Return True
		End If
		
		If srcU = "POINTER" Then
			Return True
		End If
		
		If srcU = "TYPEOF" Then
			Return True
		End If
		
		If srcU = "WHILE" Then
			Return True
		End If
		
		If srcU = "UNTIL" Then
			Return True
		End If
		
		If srcU = "WEND" Then
			Return True
		End If
		
		If srcU = "CONTINUE" Then
			Return True
		End If
		
		If srcU = "CBOOL" Then
			Return True
		End If
		
		If srcU = "CBYTE" Then
			Return True
		End If
		
		If srcU = "CSHORT" Then
			Return True
		End If
		
		If srcU = "CINT" Then
			Return True
		End If
		
		If srcU = "CLNG" Then
			Return True
		End If
		
		If srcU = "CLNGINT" Then
			Return True
		End If
		
		If srcU = "CUBYTE" Then
			Return True
		End If
		
		If srcU = "CUSHORT" Then
			Return True
		End If
		
		If srcU = "CUINT" Then
			Return True
		End If
		
		If srcU = "CULNG" Then
			Return True
		End If
		
		If srcU = "CULNGINT" Then
			Return True
		End If
		
		If srcU = "CSNG" Then
			Return True
		End If
		
		If srcU = "CDBL" Then
			Return True
		End If
		
		If srcU = "CSIGN" Then
			Return True
		End If
		
		If srcU = "CUNSG" Then
			Return True
		End If
		
		If srcU = "CPTR" Then
			Return True
		End If
		
		If srcU = "CAST" Then
			Return True
		End If
		
		If srcU = "CALL" Then
			Return True
		End If
		
		If srcU = "BYVAL" Then
			Return True
		End If
		
		If srcU = "BYREF" Then
			Return True
		End If
		
		If srcU = "AS" Then
			Return True
		End If
		
		If srcU = "DECLARE" Then
			Return True
		End If
		
		If srcU = "GOTO" Then
			Return True
		End If
		
		If srcU = "CONST" Then
			Return True
		End If
		
		If srcU = "FOR" Then
			Return True
		End If
		
		If srcU = "STEP" Then
			Return True
		End If
		
		If srcU = "NEXT" Then
			Return True
		End If
		
		If srcU = "TO" Then
			Return True
		End If
		
		If srcU = "TYPE" Then
			Return True
		End If
		
		If srcU = "UNION" Then
			Return True
		End If
		
		If srcU = "END" Then
			Return True
		End If
		
		If srcU = "SUB" Then
			Return True
		End If
		
		If srcU = "FUNCTION" Then
			Return True
		End If
		
		If srcU = "CDECL" Then
			Return True
		End If
		
		If srcU = "STDCALL" Then
			Return True
		End If
		
		If srcU = "THIS" Then
			Return True
		End If
		
		If srcU = "__THISCALL" Then
			Return True
		End If
		
		If srcU = "__FASTCALL" Then
			Return True
		End If
		
		If srcU = "PASCAL" Then
			Return True
		End If
		
		If srcU = "ALIAS" Then
			Return True
		End If
		
		If srcU = "LIB" Then
			Return True
		End If
		
		If srcU = "LET" Then
			Return True
		End If
		
		If srcU = "EXIT" Then
			Return True
		End If
		
		If srcU = "DO" Then
			Return True
		End If
		
		If srcU = "LOOP" Then
			Return True
		End If
		
		If srcU = "RETURN" Then
			Return True
		End If
		
		If srcU = "IF" Then
			Return True
		End If
		
		If srcU = "THEN" Then
			Return True
		End If
		
		If srcU = "ELSE" Then
			Return True
		End If
		
		If srcU = "ELSEIF" Then
			Return True
		End If
		
		If srcU = "ENDIF" Then
			Return True
		End If
		
		If srcU = "SELECT" Then
			Return True
		End If
		
		If srcU = "CASE" Then
			Return True
		End If
		
		If srcU = "IS" Then
			Return True
		End If
		
		If srcU = "USING" Then
			Return True
		End If
		
		If srcU = "LEN" Then
			Return True
		End If
		
		If srcU = "PEEK" Then
			Return True
		End If
		
		If srcU = "POKE" Then
			Return True
		End If
		
		If srcU = "SWAP" Then
			Return True
		End If
		
		If srcU = "COMMON" Then
			Return True
		End If
		
		If srcU = "ENUM" Then
			Return True
		End If
		
		If srcU = "ASM" Then
			Return True
		End If
		
		If srcU = "EXTERN" Then
			Return True
		End If
		
		If srcU = "WITH" Then
			Return True
		End If
		
		If srcU = "SCOPE" Then
			Return True
		End If
		
		If srcU = "PUBLIC" Then
			Return True
		End If
		
		If srcU = "PRIVATE" Then
			Return True
		End If
		
		If srcU = "PROTECTED" Then
			Return True
		End If
		
		If srcU = "PROCPTR" Then
			Return True
		End If
		
		If srcU = "NAMESPACE" Then
			Return True
		End If
		
		If srcU = "EXPORT" Then
			Return True
		End If
		
		If srcU = "IMPORT" Then
			Return True
		End If
		
		If srcU = "OVERLOAD" Then
			Return True
		End If
		
		If srcU = "CONSTRUCTOR" Then
			Return True
		End If
		
		If srcU = "DESTRUCTOR" Then
			Return True
		End If
		
		If srcU = "OPERATOR" Then
			Return True
		End If
		
		If srcU = "PROPERTY" Then
			Return True
		End If
		
		If srcU = "CLASS" Then
			Return True
		End If
		
		If srcU = "INTERFACE" Then
			Return True
		End If
		
		If srcU = "EXTENDS" Then
			Return True
		End If
		
		If srcU = "IMPLEMENTS" Then
			Return True
		End If
		
		If srcU = "BASE" Then
			Return True
		End If
		
		If srcU = "VIRTUAL" Then
			Return True
		End If
		
		If srcU = "ABSTRACT" Then
			Return True
		End If
		
		If srcU = "OBJECT" Then
			Return True
		End If
		
		If srcU = "OFFSETOF" Then
			Return True
		End If
		
		If srcU = "VAR" Then
			Return True
		End If
		
		If srcU = "IIF" Then
			Return True
		End If
		
		If srcU = "VA_FIRST" Then
			Return True
		End If
		
		If srcU = "DATA" Then
			Return True
		End If
		
		If srcU = "FIELD" Then
			Return True
		End If
		
		If srcU = "LOCAL" Then
			Return True
		End If
		
		If srcU = "DEFINED" Then
			Return True
		End If
		
		If srcU = "SIZEOF" Then
			Return True
		End If
		
		If srcU = "STRPTR" Then
			Return True
		End If
		
		If srcU = "VARPTR" Then
			Return True
		End If
		
		If srcU = "DYNAMIC" Then
			Return True
		End If
		
		If srcU = "INCLUDE" Then
			Return True
		End If
		
		If srcU = "GOSUB" Then
			Return True
		End If
		
		If srcU = "DEFBYTE" Then
			Return True
		End If
		
		If srcU = "DEFUBYTE" Then
			Return True
		End If
		
		If srcU = "DEFSHORT" Then
			Return True
		End If
		
		If srcU = "DEFUSHORT" Then
			Return True
		End If
		
		If srcU = "DEFINT" Then
			Return True
		End If
		
		If srcU = "DEFUINT" Then
			Return True
		End If
		
		If srcU = "DEFLNG" Then
			Return True
		End If
		
		If srcU = "DEFULNG" Then
			Return True
		End If
		
		If srcU = "DEFLONGINT" Then
			Return True
		End If
		
		If srcU = "DEFULONGINT" Then
			Return True
		End If
		
		If srcU = "DEFSNG" Then
			Return True
		End If
		
		If srcU = "DEFDBL" Then
			Return True
		End If
		
		If srcU = "DEFSTR" Then
			Return True
		End If
		
		If srcU = "OPTION" Then
			Return True
		End If
		
		If srcU = "EXPLICIT" Then
			Return True
		End If
		
		If srcU = "SADD" Then
			Return True
		End If
		
		If srcU = "ON" Then
			Return True
		End If
		
		If srcU = "ERROR" Then
			Return True
		End If
		
		If srcU = "SIN" Then
			Return True
		End If
		
		If srcU = "ASIN" Then
			Return True
		End If
		
		If srcU = "COS" Then
			Return True
		End If
		
		If srcU = "ACOS" Then
			Return True
		End If
		
		If srcU = "TAN" Then
			Return True
		End If
		
		If srcU = "ATN" Then
			Return True
		End If
		
		If srcU = "SQR" Then
			Return True
		End If
		
		If srcU = "LOG" Then
			Return True
		End If
		
		If srcU = "EXP" Then
			Return True
		End If
		
		If srcU = "ATAN2" Then
			Return True
		End If
		
		If srcU = "RESUME" Then
			Return True
		End If
		
		If srcU = "ERR" Then
			Return True
		End If
		
		If srcU = "REDIM" Then
			Return True
		End If
		
		If srcU = "ERASE" Then
			Return True
		End If
		
		If srcU = "LBOUND" Then
			Return True
		End If
		
		If srcU = "UBOUND" Then
			Return True
		End If
		
		If srcU = "STR" Then
			Return True
		End If
		
		If srcU = "CVD" Then
			Return True
		End If
		
		If srcU = "CVS" Then
			Return True
		End If
		
		If srcU = "CVI" Then
			Return True
		End If
		
		If srcU = "CVL" Then
			Return True
		End If
		
		If srcU = "CVSHORT" Then
			Return True
		End If
		
		If srcU = "CVLONGINT" Then
			Return True
		End If
		
		If srcU = "MKD" Then
			Return True
		End If
		
		If srcU = "MKS" Then
			Return True
		End If
		
		If srcU = "MKI" Then
			Return True
		End If
		
		If srcU = "MKL" Then
			Return True
		End If
		
		If srcU = "MKSHORT" Then
			Return True
		End If
		
		If srcU = "MKLONGINT" Then
			Return True
		End If
		
		If srcU = "WSTR" Then
			Return True
		End If
		
		If srcU = "MID" Then
			Return True
		End If
		
		If srcU = "INSTR" Then
			Return True
		End If
		
		If srcU = "INSTRREV" Then
			Return True
		End If
		
		If srcU = "TRIM" Then
			Return True
		End If
		
		If srcU = "RTRIM" Then
			Return True
		End If
		
		If srcU = "LTRIM" Then
			Return True
		End If
		
		If srcU = "LCASE" Then
			Return True
		End If
		
		If srcU = "UCASE" Then
			Return True
		End If
		
		If srcU = "RESTORE" Then
			Return True
		End If
		
		If srcU = "READ" Then
			Return True
		End If
		
		If srcU = "PRINT" Then
			Return True
		End If
		
		If srcU = "LPRINT" Then
			Return True
		End If
		
		If srcU = "OPEN" Then
			Return True
		End If
		
		If srcU = "CLOSE" Then
			Return True
		End If
		
		If srcU = "SEEK" Then
			Return True
		End If
		
		If srcU = "PUT" Then
			Return True
		End If
		
		If srcU = "GET" Then
			Return True
		End If
		
		If srcU = "ACCESS" Then
			Return True
		End If
		
		If srcU = "WRITE" Then
			Return True
		End If
		
		If srcU = "LOCK" Then
			Return True
		End If
		
		If srcU = "INPUT" Then
			Return True
		End If
		
		If srcU = "WINPUT" Then
			Return True
		End If
		
		If srcU = "OUTPUT" Then
			Return True
		End If
		
		If srcU = "BINARY" Then
			Return True
		End If
		
		If srcU = "RANDOM" Then
			Return True
		End If
		
		If srcU = "APPEND" Then
			Return True
		End If
		
		If srcU = "ENCODING" Then
			Return True
		End If
		
		If srcU = "NAME" Then
			Return True
		End If
		
		If srcU = "WIDTH" Then
			Return True
		End If
		
		If srcU = "COLOR" Then
			Return True
		End If
		
		If srcU = "PRESERVE" Then
			Return True
		End If
		
		If srcU = "SPC" Then
			Return True
		End If
		
		If srcU = "TAB" Then
			Return True
		End If
		
		If srcU = "LINE" Then
			Return True
		End If
		
		If srcU = "VIEW" Then
			Return True
		End If
		
		If srcU = "UNLOCK" Then
			Return True
		End If
		
		If srcU = "CHR" Then
			Return True
		End If
		
		If srcU = "WCHR" Then
			Return True
		End If
		
		If srcU = "ASC" Then
			Return True
		End If
		
		If srcU = "LSET" Then
			Return True
		End If
		
		If srcU = "RSET" Then
			Return True
		End If
		
		If srcU = "PSET" Then
			Return True
		End If
		
		If srcU = "PRESET" Then
			Return True
		End If
		
		If srcU = "POINT" Then
			Return True
		End If
		
		If srcU = "CIRCLE" Then
			Return True
		End If
		
		If srcU = "WINDOW" Then
			Return True
		End If
		
		If srcU = "PALETTE" Then
			Return True
		End If
		
		If srcU = "SCREEN" Then
			Return True
		End If
		
		If srcU = "PAINT" Then
			Return True
		End If
		
		If srcU = "DRAW" Then
			Return True
		End If
		
		If srcU = "IMAGECREATE" Then
			Return True
		End If
		
		If srcU = "THREADCALL" Then
			Return True
		End If
		
		If srcU = "CVA_START" Then
			Return True
		End If
		
		If srcU = "CVA_END" Then
			Return True
		End If
		
		If srcU = "CVA_ARG" Then
			Return True
		End If
		
		If srcU = "CVA_COPY" Then
			Return True
		End If
		
	End Scope
	
	Return False
	
End Function
