FUNCTION Date, axis, index, value

; Format an axis for showing dates.

monthStr = ['Jan','Feb','Mar', 'Apr', 'May', 'Jun', 'Jul', $
'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
CalDat, Long(value), month, day, year
year = StrMid(StrTrim(year,2), 2, 2)

RETURN, StrTrim(day, 2) + ' ' + monthStr(month-1) + ' ' + year
END
