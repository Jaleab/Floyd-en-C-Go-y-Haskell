#!/bin/bash

files=$( ls *txt )
languages=(C Go Haskell)
declare -A pairs
counter=0

# Crea parejas grafo,lenguaje para las ejecuciones
for f in $files 
do
	for l in ${languages[@]}
	do
		pairs[$counter]=$l,$f
		counter=$((counter+1))
	done
done

# Nuevo array con las parejas en orden al azar
random=( $(shuf -e "${pairs[@]}") )

# Generar el csv y tomar tiempos
for r in ${random[@]}
do
	# Separar en variables
	lang=${r%,*}
	graph=${r#*,}

	printf $lang","$graph"," >> time.csv

	# Revisa que tipo de grafo es
	if [[ $graph == "dense"* ]] ;
	then
		printf "Dense," >> time.csv
	else
		printf "Sparse," >> time.csv
	fi

	# Ejecucion de cada implementacion
	case $lang in
		"C")
			./FloydC $graph >> time.csv
			echo "" >> time.csv
			;;
		"Go")
			./FloydGo -file=$graph >> time.csv
			echo "" >> time.csv
			;;
		"Haskell")
			echo "time Haskell" >> time.csv
			;;
	esac
done