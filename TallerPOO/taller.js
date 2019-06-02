//Escriban acá su código.
//1 
let qf = { esFinal: true, 
		   transiciones: {},
		   aceptar: function(s) 
		   {
		   	return (s === "")
		   }
		 }



// ej 1.2
let q3 = {
	esFinal: true,
	transiciones:{
	}
}

let q2 = {
	esFinal: true,
	transiciones:{
		c: q3,
	}
}

let q1 = {
	esFinal: false,
	transiciones:{
		
		b: q2,
		c: q3
	}
}

q1.transiciones["a"] = q1;

// ej 2




String.prototype.head = function () 
{
	return this.slice(0,1);
}
String.prototype.tail = function () 
{
	return this.slice(1,this.length);
}


// ej 3


function acepta(s){
	if (s == "" && this.esFinal){
		return true
	} else if(this.transiciones[s.head()] != undefined ){
		return this.transiciones[s.head()].acepta(s.tail());
	} else {
		return false
	}
}

function Estado(esFinal, transiciones){
		this.esFinal= esFinal,
		this.transiciones= transiciones,
		this.acepta= acepta
}


// ej 3.2

let z = new Estado()
for (let q of [q1,q2,q3]){
	Object.setPrototypeOf(q, z);
}

// ej 4

function nuevaTransicion(etiqueta, destino){
	let t = Object.assign({},this.transiciones)
	this.transiciones = t;
	this.transiciones[etiqueta] = destino;
}

Estado.prototype.nuevaTransicion = nuevaTransicion

// ej 5

function algunoAcepta(s, qs){
	if(!Array.isArray(qs)){
		return qs.acepta(s)
	} else {
		return qs.filter(q => q.acepta(s)).length > 0
	}
}

// ej 6

function aceptaND(s) {
	if (s == "" && this.esFinal){
		return true
	} else if(this.transiciones[s.head()] != undefined ){
		return algunoAcepta(s.tail(), this.transiciones[s.head()])
	} else {
		return false
	}
}

function nuevaTransicionND(e, destino){
	if (this.transiciones[e] == undefined){
		this.transiciones[e] = destino
	}
	else if (Array.isArray(this.transiciones[e])){
		if (!this.transiciones[e].includes( destino))
		{
			this.transiciones[e].push(destino)
		}	
	}
	else {
		if (this.transiciones[e] !== destino)
		{
			this.transiciones[e] = [this.transiciones[e], destino]
			this.acepta = aceptaND
		}
	}
	
}


Estado.prototype.nuevaTransicionND = nuevaTransicionND

function esDeterministico(q)
{
	let qs = []
	return esDeterministicoNoRecorrido(q, qs)
}

function esDeterministicoNoRecorrido(q, qs)
{
	console.log(qs)
	if (qs.includes(q))
		return true;
	else	
	{
		let esDeterministico = true
		const keys = Object.keys(q.transiciones)

		for (let p of keys)
		{
			if (Array.isArray(q.transiciones[p]))
			{
				return false		
			}
			qs.push(q)
			esDeterministico = esDeterministico && esDeterministicoNoRecorrido(q.transiciones[p], qs)
		}
		return esDeterministico
	}
	
}

function calcularResultado(){
	//Editen esta función para que devuelva lo que quieran ver. Pueden escribir acá sus tests.
	// 1.return qf.aceptar("");
	// 1.return qf.aceptar("pepe");
	
	/*
	let e3 = new Estado(true, {})
	let e2 = new Estado(true, {c:e3})
	let e1 = new Estado(false, {a:this, b:e2})
	return q1.acepta("b");
	*/
	
	/*
	let e2 = Object.create(q1)
	let e1 = new Estado(true, {a:e2})

	console.log(q1)
	e2.nuevaTransicion("a", e1)
	console.log(q1)
	return e2.transiciones;
	*/

	/*
	let e3 = new Estado(true, {})
	let e2 = new Estado(true, {c:e3})
	let e1 = new Estado(false, {b:e2})
	e1.nuevaTransicion("a", e1)

	return algunoAcepta("ab", [e2, e3, e1]);
	*/
	
	/*let e3 = new Estado(true, {})
	let e2 = new Estado(true, {c:e3})
	let e1 = new Estado(false, {b:e2})
	e1.nuevaTransicionND("b", e1)
	return e1.acepta("bbbbba")*/

	let e3 = new Estado(true, {})
	let e2 = new Estado(true, {c:e3})
	let e1 = new Estado(false, {b:e2})
	//e1.nuevaTransicionND("b", e1)
	return esDeterministico(e1)
}
