#Nama: Hafizul Damar Laya
#NIM: 120450028
#Kelas: Komputasi Statistik - RB

#butterfly curvve
#nilai t[0,12*pi]
t = seq(0,12*pi, 0.01)

#fungsi garis exp(cos(t)) - 2*cos(4*t) +sin(t/12)^5
funcs_ = exp(cos(t)) - 2*cos(4*t) +sin(t/12)^5
#fungsi koordinat x y
x = sin(t)*funcs_
y = cos(t)*funcs_


plot(x,y, col =('lightblue'),pch=17,cex=1, main = 'Tugas Butterfly Curve')