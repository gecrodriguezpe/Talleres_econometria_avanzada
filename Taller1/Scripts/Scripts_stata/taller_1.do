
// Taller 1
use "/Users/federicoduenas/Desktop/Econometría_ Avz_/taller 1/manufacturaCol.dta"

foreach var of varlist producto_acum producto costos {
	gen ln_`var'=ln(`var'+1)
}


// relacion de log_c con log_n controlando por log_y
// usando Partialling out: 

reg ln_producto_acum ln_producto

gen aux_1 = 9.256792 + 0.1120002*ln_producto

// para tener la parte de N que no es explicada por Y
gen e_1 = ln_producto_acum - aux_1

// ahora para tener los C_i no explicada por Y_i

reg ln_costos ln_producto

gen aux_2_1 = -.019997 + .0052894*ln_producto

// los residuales: parte de C_i no explicada por Y_i
gen e_1_2 = ln_costos - aux_2_1

// sactter de la relación C_i con N_i una vez se removió el efecto de Y_i
**#

scatter e_1_2 e_1 || lfit e_1_2 e_1

/*
	

// ahora si calculo la relacion entre N y C, controlando por Y

reg ln_costos e_1

// Que, por teorema de Waugh-Frisch-Lovell, es equivalente al parámetro de log_N de la reg lineal: 
reg ln_costos ln_producto_acum ln_producto

// gráfica de la relación de C y N: 
scatter ln_costos e_1  || lfit ln_costos e_1
*/
// ahora la relacion de log_C y log_Y controlando por log_N

reg ln_producto ln_producto_acum

gen aux_2 = .3859203 + .4743226*ln_producto_acum

gen e_2 = ln_producto - aux_2


// ahora remuevo el efecto de N_i sobre C_i

reg ln_costos ln_producto_acum
gen aux_2_2 = -.013807 + .0020865*ln_producto_acum
gen e_2_2 = ln_costos - aux_2_2


// sactter de la relación de C y Y removiendo el efecto de N_i

scatter e_2_2 e_2 || lfit e_2_2 e_2

/*
reg ln_costos e_2

reg ln_costos ln_producto_acum ln_producto


scatter ln_costos e_2  || lfit ln_costos e_2

// estimar la ecuación del modelo: 

reg ln_costos ln_producto_acum ln_producto
*/
reg ln_costos ln_producto_acum ln_producto


matlist e(b)
matlist e(V)









