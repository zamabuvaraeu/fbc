'' misc helpers
''
''


#include once "fb.bi"
#include once "fbint.bi"
#include once "ir.bi"
#include once "lex.bi"
#include once "dstr.bi"

'':::::
function hHexUInt _
	( _
		byval value as uinteger _
	) as zstring ptr static

	static as zstring * 8 + 1 res
	dim as zstring ptr p
	dim as integer lgt, maxlen

	static as integer hexTB(0 to 15) = _
	{ _
		asc( "0" ), asc( "1" ), asc( "2" ), asc( "3" ), _
		asc( "4" ), asc( "5" ), asc( "6" ), asc( "7" ), _
		asc( "8" ), asc( "9" ), asc( "A" ), asc( "B" ), _
		asc( "C" ), asc( "D" ), asc( "E" ), asc( "F" ) _
	}

	maxlen = 4
	if( value > 65535 ) then
		maxlen = 8
	end if

	p = @res + 8-1
	lgt = 0

	do
		*p = hexTB( value and &h0000000F )

		lgt +=1
		if( lgt = maxlen ) then
			exit do
		end if

		p -= 1
		value shr= 4
	loop

	function = p

end function

function hFloatToHex _
	( _
		byval value as double, _
		byval dtype as integer _
	) as string

	'' Emit the raw bytes that make up the float
	'' x86 little-endian assumption
	if( typeGet( dtype ) = FB_DATATYPE_DOUBLE ) then
		function = "0x" + hex( *cptr( ulongint ptr, @value ), 16 )
	else
		dim as single singlevalue = value
		'' Using an intermediate uinteger to allow compiling with FB
		'' versions before the overload resolution overhaul
		function = "0x" + hex( cuint( *cptr( ulong ptr, @singlevalue ) ), 8 )
	end if
end function

function hFloatToHex_C99 _
	( _
		byval value as double _
	) as string

	'' float hex format defined in C99 spec: e.g. 0x1.fp+3

	dim as ulongint n = *cptr( ulongint ptr, @value )

	dim as integer sign = n shr 63
	dim as integer exp2 = (n shr 52) and (1u shl 11 - 1)
	dim as ulongint mantissa = n and (1ull shl 52 - 1)

	dim as string ret

	if( sign <> 0 ) then
		'' negative
		ret = "-0x"
	else
		'' positive
		ret = "0x"
	end if

	exp2 -= 1023
	if( exp2 > -1023 ) then
		'' normalized
		ret += "1." + hex( mantissa, 13 )
		if right( ret, 1 ) = "0" then ret = rtrim( ret, "0" )
	else
		if mantissa = 0 then
			'' zero
			ret += "0"
			exp2 = 0
		else
			'' denormed
			exp2 += 1
			ret += "0." + hex( mantissa, 13  )
			if right( ret, 1 ) = "0" then ret = rtrim( ret, "0" )
		end if
	end if

	ret += "p" & (*iif( exp2 >= 0, @"+", @"-" )) + str( abs( exp2 ) )

	return ret

end function

'':::::
function hFBrelop2IRrelop _
	( _
		byval tk as integer _
	) as integer static

	dim as integer op = any

	select case as const tk
	case FB_TK_EQ
		op = AST_OP_EQ
	case FB_TK_GT
		op = AST_OP_GT
	case FB_TK_LT
		op = AST_OP_LT
	case FB_TK_NE
		op = AST_OP_NE
	case FB_TK_LE
		op = AST_OP_LE
	case FB_TK_GE
		op = AST_OP_GE
	case else
		errReport( FB_ERRMSG_EXPECTEDRELOP )
		'' error recovery: fake an op
		op = AST_OP_EQ
	end select

	function = op

end function

'':::::
function hFileExists _
	( _
		byval filename as zstring ptr _
	) as integer static
	dim f as integer

	f = freefile

	if( open( *filename, for input, as #f ) = 0 ) then
		function = TRUE
		close #f
	else
		function = FALSE
	end if

end function

Function NeedUCase( _
		byval src as const zstring ptr _
	)As Boolean
	
	Dim src1 As String = *src
	Dim Length As Integer = Len(src1)
	Dim srcU As String = UCase(src1)
	
	Scope
		If srcU = "MACRO" Then
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

'':::::
sub hUcase _
	( _
		byval src as const zstring ptr, _
		byval dst as zstring ptr _
	) static
	
	If NeedUCase(src) Then
		
		dim as integer c
		dim as const zstring ptr s
		dim as zstring ptr d

		s = src
		d = dst

		do
			c = *s
			if( c >= 97 ) then
				if( c <= 122 ) then
					c -= (97 - 65)
				end if
			elseif( c = 0 ) then
				exit do
			end if

			*d = c

			s += 1
			d += 1
		loop

		'' null-term
		*d = 0
	
	Else
		dim as integer c
		dim as const zstring ptr s
		dim as zstring ptr d

		s = src
		d = dst

		do
			c = *s
			if c = 0  then
				exit do
			end if

			*d = c

			s += 1
			d += 1
		loop

		'' null-term
		*d = 0
		
	End If
end sub

sub hUcaseAllways _
	( _
		byval src as const zstring ptr, _
		byval dst as zstring ptr _
	) static
	
	dim as integer c
	dim as const zstring ptr s
	dim as zstring ptr d

	s = src
	d = dst

	do
		c = *s
		if( c >= 97 ) then
			if( c <= 122 ) then
				c -= (97 - 65)
			end if
		elseif( c = 0 ) then
			exit do
		end if

		*d = c

		s += 1
		d += 1
	loop

	'' null-term
	*d = 0
end sub

'':::::
sub hClearName _
	( _
		byval src as zstring ptr _
	) static

	dim as zstring ptr p

	p = src

	do
		select case as const *p
		case 0
			exit do

		case CHAR_AUPP to CHAR_ZUPP, CHAR_ALOW to CHAR_ZLOW, CHAR_0 to CHAR_9, CHAR_UNDER

		case else
			*p = CHAR_ZLOW
		end select

		p += 1
	loop

end sub

'' Searches backwards for the last '.' while still behind '/' or '\'.
private function hFindExtBegin( byref path as string ) as integer
	for i as integer = len( path )-1 to 0 step -1
		select case( path[i] )
		case asc( "." )
			return i
#if defined( __FB_WIN32__ ) or defined( __FB_DOS__ )
		case asc( "\" ), asc( "/" )
#else
		case asc( "/" )
#endif
			exit for
		end select
	next
	function = len( path )
end function

function hStripExt( byref path as string ) as string
	function = left( path, hFindExtBegin( path ) )
end function

'':::::
function hStripPath _
	( _
		byval filename as zstring ptr _
	) as string static

	dim as integer lp, p_found, p(1 to 2)

	lp = 0
	do
		p(1) = instr( lp+1, *filename, RSLASH )
		p(2) = instr( lp+1, *filename, "/" )
		if p(1)=0 or (p(2)>0 and p(2)<p(1)) then
			p_found = p(2)
		else
			p_found = p(1)
		end if
		if( p_found = 0 ) then
			exit do
		end if
		lp = p_found
	loop

	if( lp > 0 ) then
		function = mid( *filename, lp+1 )
	else
		function = *filename
	end if

end function

'':::::
function hStripFilename _
	( _
		byval filename as zstring ptr _
	) as string static

	dim as integer lp, p_found, p(1 to 2)

	lp = 0
	do
		p(1) = instr( lp+1, *filename, RSLASH )
		p(2) = instr( lp+1, *filename, "/" )
		if p(1)=0 or (p(2)>0 and p(2)<p(1)) then
			p_found = p(2)
		else
			p_found = p(1)
		end if
		if( p_found = 0 ) then
			exit do
		end if
		lp = p_found
	loop

	if( lp > 0 ) then
		function = left( *filename, lp )
	else
		function = ""
	end if

end function

'':::::
function hGetFileExt _
	( _
		byval fname as zstring ptr _
	) as string static

	dim as integer p, lp
	dim as string res

	lp = 0
	do
		p = instr( lp+1, *fname, "." )
		if( p = 0 ) then
			exit do
		end if
		lp = p
	loop

	if( lp = 0 ) then
		function = ""
	else
		res = lcase( mid( *fname, lp+1 ) )
		if instr( res, RSLASH ) > 0 or instr( res, "/" ) > 0 then
			'' We had a folder with a "." inside ...
			function = ""
		elseif( len(res) > 0 ) then
			'' . or .. dirs?
			if( res[0] = asc( RSLASH ) or res[0] = asc( "/" ) ) then
				function = ""
			else
				function = res
			end if
		end if
	end if

end function

sub hReplaceSlash( byval s as zstring ptr, byval char as integer )
	for i as integer = 0 to len( *s ) - 1
		if( (s[i] = CHAR_RSLASH) or (s[i] = CHAR_SLASH) ) then
			s[i] = char
		end if
	next
end sub

function pathStripDiv( byref path as string ) as string
	dim as integer length = len( path )
	if( length > 0 ) then
		length -= 1
		select case( path[length] )
#if defined( __FB_WIN32__ ) or defined( __FB_DOS__ )
		case asc("/"), asc("\")
#else
		case asc("/")
#endif
			return left( path, length )
		end select
	end if
	function = path
end function

function pathIsAbsolute( byval path as zstring ptr ) as integer
#if defined( __FB_WIN32__ ) or defined( __FB_DOS__ )
	if( (*path)[0] <> 0 ) then
		select case( (*path)[1] )
		case asc( ":" )
			'' C:...
			function = TRUE
#ifdef __FB_WIN32__
		case asc( "\" )
			'' \\... UNC path
			function = ((*path)[0] = asc( "\" ))
#endif
		end select
	end if
#else
	'' /...
	function = ((*path)[0] = asc( "/" ))
#endif
end function

function hCheckFileFormat( byval f as integer ) as integer
	dim as long BOM
	dim as FBFILE_FORMAT fmt

	'' little-endian assumptions
	fmt = FBFILE_FORMAT_ASCII

	if( get( #f, 0, BOM ) = 0 ) then
		if( BOM = &hFFFE0000 ) then
			fmt = FBFILE_FORMAT_UTF32BE

		elseif( BOM = &h0000FEFF ) then
		    fmt = FBFILE_FORMAT_UTF32LE

		else
			BOM and= &h00FFFFFF
			if( BOM = &h00BFBBEF ) then
				fmt = FBFILE_FORMAT_UTF8

			else
				BOM and= &h0000FFFF
				if( BOM = &h0000FEFF ) then
					fmt = FBFILE_FORMAT_UTF16LE

				elseif( BOM = &h0000FFFE ) then
					fmt = FBFILE_FORMAT_UTF16BE
				end if
			end if
		end if

		select case fmt
		case FBFILE_FORMAT_ASCII
			seek #f, 1

		case FBFILE_FORMAT_UTF8
			seek #f, 1+3

		case FBFILE_FORMAT_UTF16LE, _
			 FBFILE_FORMAT_UTF16BE
			seek #f, 1+2
		end select
	end if

	function = fmt
end function

function hCurDir( ) as string
	'' curdir() usually won't be terminated with a path separator,
	'' except when it points to the file system root, instead of
	'' some directory (e.g. C:\ on Win32 or / on Unix).
	function = pathStripDiv( curdir( ) )
end function

function pathStripCurdir( byref path as string ) as string
	var pwd = hCurdir( ) + FB_HOST_PATHDIV
	if( left( path, len( pwd ) ) = pwd ) then
		function = right( path, len( path ) - len( pwd ) )
	else
		function = path
	end if
end function

function hIsValidSymbolName( byval sym as zstring ptr ) as integer

	if( sym = NULL ) then exit function

	var symlen = len( *sym )

	if( symlen = 0 ) then exit function

	if( (hIsChar(sym[0]) orelse (sym[0] = asc("_"))) = FALSE ) then exit function

	for i as integer = 1 to symlen-1
		if( ((hIsChar(sym[i])) orelse (sym[i] = asc("_")) orelse (hIsCharNumeric(sym[i]))) = FALSE ) then exit function
	next

	function = TRUE

end function

'' Checks whether a string starts with and ends in [double-]quotes.
private function strIsQuoted(byref s as string) as integer
	dim as integer last = len(s) - 1
	if (last < 1) then
		return FALSE
	end if

	return (((s[0] = asc("""")) and (s[last] = asc(""""))) or _
	        ((s[0] = asc("'" )) and (s[last] = asc("'" ))))
end function

function strUnquote(byref s as string) as string
	if (strIsQuoted(s)) then
		return mid(s, 2, len(s) - 2)
	end if
	return s
end function
