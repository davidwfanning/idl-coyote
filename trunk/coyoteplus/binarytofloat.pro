function basetodecimal,fraction,base,status=status,msg=msg
; str contains a fraction (number with base and point) 
; fraction = mantissa.base^exp
; 
; example:
; binary number (base=2) with point=4
; 1101.101 	= 1.2^3 + 1.2^2 + 0.2^1 + 1.2^0 + 1.2^(-1) + 0.2^(-2) + 1.2^(-3)
;			= 1.2^m + 1.2^(m-1) + ...                             + 1.2^(m-n+1)
;			= [1.2^(n-1) + 1.2^(n-2) + ...                             + 1.2^0]*2^(m-n+1)

status=0

; Get point position, n digits, m leading digit exponent
str=fraction
point=strpos(str,'.')
if point eq -1 then begin
	n=strlen(str)
	point=n
endif else begin
	str= strsplit(str,'.',count=ct,/extract)
	case ct of 
	1: str=str[0]
	2: str=str[0]+str[1]
	else: return,0
	endcase
	n=strlen(str)
endelse
m=point-1

; Base in the correct format
n_=alog(2)/alog(base)*[64,32,16,8]
if n gt n_[0] then begin
	base_=ulong64(base)^ul64indgen(n)
	status or=1
endif else $
if n gt n_[1] then base_=ulong64(base)^ul64indgen(n) else $
if n gt n_[2] then base_=ulong(base)^ulindgen(n) else $
if n gt n_[3] then base_=uint(base)^uindgen(n) else base_=byte(base)^bindgen(n)

; Digits
str=strlowcase(str)
digits=byte(str)-(byte('0'))[0]

off=(byte('a')-byte('0'))[0]
ind=where(digits ge off and digits lt off+base-10,ct)
if ct ne 0 then digits[ind]+=10-off
	
ind=where(digits ge base,ct)
if ct ne 0 then begin
	digits[ind]=0
	status or= 2
endif

; Mantissa:
mantissa=total(base_*reverse(digits),/pres)
exp=m-n+1

case status of
0: msg='no error'
1: msg='too many digits'
2: msg='some digits set to 0'
3: msg='too many digits and some digits set to 0'
endcase

return,{mantissa:mantissa,exp:exp}
end;function basetodecimal
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function floattobinary,f
if size(f,/type) eq 5 then nbytes=8 else nbytes=4
return,string(reverse(byte(f,0,nbytes)),format='('+string(nbytes,format='(I0)')+'b08)')
end;function floattobinary,f
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function decomposefloat,fin

; Floating point:
;					Sign		Exponent	Mantissa	Bias
; Half Precision	1 [15]		5 [14-10]	10 [09-00]	15
; Single Precision	1 [31]		8 [30-23]	23 [22-00]	127
; Double Precision	1 [63]	   11 [62-52]	52 [51-00]	1023

; f = sign.fraction.base^(exponent)
;   = sign.mantissa.base^(exponent+exponent_fraction)
;
; 	- sign: 1 => negative number; 0 => positive number
;	- base(radix): 2
;	- fraction: a fractional number with a specific base and nfraction digits: e.g. 10.23 (base=10, nfraction=4)
;			fraction = mantissa.base^exponent_fraction
;	- mantissa: the integer part of the fraction (all digits without the radix point)
;			normalized representation: take radix point just after the first non-zero digit
;												=> exponent_fraction = -nfraction+1
;			normalized representation (radix-2): first non-zero digit is always 1 so we will omit this
;												=> exponent_fraction = -nfraction
;	- exponent: subtract bias from the exponent stored in the machine to see the real exponent
;	- significand: other name for mantissa
; f(binary) = sign | exponent + bias | mantissa without leading 1

; MACHAR fields
;	- ibeta: base (e.g. 2)
;	- it: number of bits for the mantissa (including the omitted bit: nfraction=it-1 )
;	- iexp: number if bits for the exponent
;	- machep: the smallest n for which (1+base^n) ne 1 (machep=-nfraction)
;		- eps: base^machep
;	- negep: the smallest n for which (1-base^n) ne 1
;		- epsneg: base^negep => this is bigger than eps, so use eps
;	- minexp: minimum exponent (take normalized representation into account)
;		- xmin: base^minexp
;	- maxexp: maximal exponent
;		- xmax: (1-epsneg).base^maxexp

; Rounding errors when storing floating point numbers:
;	freal -> fmach = sign.mantissa.base^(exponent+machep) = sign.mantissa.eps.base^exponent
;	1 ulp (unit in last place) 	= this is the distance between a stored floating point f1
;								  number and its closest neighbour f2:
;								  			- exponent1 = exponent2 = exp
;								  			- mantissa1 = mantissa2 + 1
;								= abs(f1 - f2)
;								= eps.(mantissa1.base^exponent1-mantissa2.base^exponent2)
;								= eps.base^(exp)
;	Absolute error when storing: abs(freal-fmach)
;						- truncation: abs(freal-fmach) <= eps.base^(exp)
;						- rounding: abs(freal-fmach) <= 0.5.eps.base^(exp)
;						abs(freal-fmach) <= c.eps.base^(exp) = c ulp
;	Relative error when storing: abs(freal-fmach)/abs(freal)
;						abserr  <= c.eps.base^(exp)/abs(freal)
;								 = [c.eps.base^(exp)]/[(m.eps+/-c.eps).base^(exp)]
;								 = c.eps/[m.eps+/-c.eps]
;								 = c.eps/[1.xxxxxx(y+/-c)]
;								 		for truncation and radix-2, these are the extremes:
;								 			1.1111111 + 1 =10.0000000  >1
;								 			1.1111111 - 1 = 1.1111110  >1
;								 			1.0000000 + 1 = 1.0000001  >1
;								 			1.0000000 - 1 = 0.1111111  <1  problem?
;							    <= c.eps

; Two numbers are equal within the sorting error if
;	abs(f1-f2)/(abs(f1)>abs(f2)) <= eps   (devide by the biggest number to )

f=fin
type=size(f,/type)
if type eq 6 then begin
	re=float(f)
	im=imaginary(f)
	return,[[decomposefloat(re)],[decomposefloat(im)]]
endif else if type eq 9 then begin
	re=double(f)
	im=imaginary(f)
	return,[[decomposefloat(re)],[decomposefloat(im)]]
endif else if type ne 4 and type ne 5 then $
	if type eq 14 or type eq 15 then begin
		f=double(f)
		type=5
	endif else begin
		f=float(f)
		type=4
	endelse

double=type eq 5
res=machar(double=double)

machine=floattobinary(f)
base=res.ibeta
exp=basetodecimal(strmid(machine,1,res.iexp),base)
exp=exp.mantissa-res.maxexp+1 ; bias
fraction=basetodecimal('1.'+strmid(machine,res.iexp+1,res.it-1),base)
exp+=fraction.exp
mantissa=fraction.mantissa
s=(strmid(machine,0,1) eq '1')?-1:1

; f=s*mantissa*base^exp
return,{s:s,mantissa:mantissa,base:double?double(base):float(base),exp:exp}
end;function decomposefloat
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function binarytofloat,integer,precision=precision
if n_elements(precision) eq 0 then precision=1

case precision of
0: 	begin
	res={ibeta:2l,it:11,iexp:5l,maxexp:16l}; half
	nbit=16
	double=0b
	endcase
1: 	begin
	res=machar() ;single
	nbit=32
	double=0b
	endcase
2: 	begin
	res=machar(/double) ;double
	nbit=64
	double=1b
	endcase
else: return,0
endcase

machine=string(integer,format='(b0'+string(nbit,format='(I0)')+')')

base=res.ibeta
exp=basetodecimal(strmid(machine,1,res.iexp),base)
exp=exp.mantissa-res.maxexp+1 ; bias
fraction=basetodecimal('1.'+strmid(machine,res.iexp+1,res.it-1),base)
exp+=fraction.exp
mantissa=fraction.mantissa
s=(strmid(machine,0,1) eq '1')?-1:1

return,s*mantissa*(double?double(base):float(base))^exp
end;function binarytofloat
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pro float

; Decompose floating-point to sign,mantissa,base and exponent
f=125848.516416
a=decomposefloat(f)
if a.s*a.mantissa*a.base^a.exp eq f then print,'DECOMPOSITION: OK'

f=5546.6468d
a=decomposefloat(f)
if a.s*a.mantissa*a.base^a.exp eq f then print,'DECOMPOSITION: OK'

; Convert binary to floating-point
integer='3555'x
f=binarytofloat(integer,precision=0)
print,f,' (should be 0.33325...)'

integer='3eaaaaab'xl
f=binarytofloat(integer,precision=1)
print,f,' (should be 0.33333...)'

integer='3fd5555555555555'xll
f=binarytofloat(integer,precision=2)
print,f,' (should be 0.33333...)'
end;pro float
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

