x:int=(2+3)*6+2
square=(x:int)=>x*x
fnFactory=(y:int)=>(z:int)=>y+z
doesComplexStuff=(x:int,y:int,z:int)=>{
	n=square(x)+square(y)+square(z)
	print(n)
	n
}

main=()=>{
	while 0<x {
		print(square(x))
		x=x-1
	}
	print(doesComplexStuff(4,2,7))
	y=4+3
	print(y)
	fn=fnFactory(5)
	print(fn( 2))
	print(fn( 6))
	fn2=fnFactory(3)
	print(fn2( 6))
	print(fn( 6))
}
