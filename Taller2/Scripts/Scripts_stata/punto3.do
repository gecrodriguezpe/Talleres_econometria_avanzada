/* Taller 2 - Punto 3 */ 

global bases_datos "/home/germankux/Documents/GitHub/semestre8_git/Econometría_avanzada/Talleres_econometria_avanzada/Taller2/Bases_datos"

cd $bases_datos

import delimited "SPP_Base.csv", delimiter("$") clear 

de 


// Ser elegible (básicamente es estimar un RDN)

// Matrícula 

rdrobust ies_cualquiera puntaje_saber11, p(1) c(310) all

rdrobust ies_cualquiera puntaje_saber11, p(2) c(310) all

// Alta calidad 

rdrobust ies_altacalidad puntaje_saber11, p(1) c(310) all

rdrobust ies_altacalidad puntaje_saber11, p(2) c(310) all

// Privada

rdrobust ies_altacalidad_priv puntaje_saber11, p(1) c(310) all

rdrobust ies_altacalidad_priv puntaje_saber11, p(2) c(310) all



// Recibir el programa (básicamente es estimar un RDB)

// Matrícula 

rdrobust ies_cualquiera puntaje_saber11, fuzzy(beneficiario_spp) p(1) c(310) all

rdrobust ies_cualquiera puntaje_saber11, fuzzy(beneficiario_spp) p(2) c(310) all

// Alta calidad 

rdrobust ies_altacalidad puntaje_saber11, fuzzy(beneficiario_spp) p(1) c(310) all

rdrobust ies_altacalidad puntaje_saber11, fuzzy(beneficiario_spp) p(2) c(310) all

// Privada

rdrobust ies_altacalidad_priv puntaje_saber11, fuzzy(beneficiario_spp) p(1) c(310) all

rdrobust ies_altacalidad_priv puntaje_saber11, fuzzy(beneficiario_spp) p(1) c(310) all


 
