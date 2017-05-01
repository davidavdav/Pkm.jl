module pkm

using DataFrames
using DataArrays
using DataFramesMeta

statsfile = joinpath(dirname(dirname(@__FILE__)), "data", "stats.txt")
stats = readtable(statsfile, separator='\t')

function unpercent(a::DataArrays.DataArray)
	r = similar(a, Float64)
	percent_re = r"(\d+)%"
	for i in 1:length(a)
		if !isna(a[i])
			r[i] = parse(Float64, match(percent_re, a[i]).captures[1]) / 100
		end
	end
	return r
end

for col in [:capture, :flee]
	stats[col] = unpercent(stats[col])
end

immutable IV
	stamina::Int8
	attack::Int8
	defense::Int8
end

type Pokebeast
	nr::Int16
	twicelevel::Int8 ## 1..79
	iv::IV
	function Pokebeast(nr::Integer, twicelevel::Integer, iv::IV)
		1 ≤ nr ≤ nrow(stats) || throw(DomainError())
		1 ≤ twicelevel ≤ length(cpmtab) || throw(DomainError)
		new(nr, twicelevel, iv)
	end
end

twicelevel(level::Real) = Int8(clamp(round(Int, 2level - 1), 1, length(cpmtab)))
name2nr = Dict(row[:name] => Int16(row[:nr]) for row in eachrow(stats))

name(p::Pokebeast) = stats[p.nr, :name]
nr(name::String) = get(name2nr, name, KeyError("Beast not found"))

## constructor from name and level
Pokebeast(name::String, level::Real, iv::IV) = Pokebeast(nr(name), twicelevel(level), iv)

## initialize the cp modifier tables
cpmstep = [0.009426125469, 0.008919025675, 0.008924905903, 0.00445946079]

function cpmhalf(level, cpm)
	i = round(Int, div(level, 10)) + 1
	return sqrt(cpm^2 + cpmstep[i])
end

c = 0.094
cpmtab = [c]
for i in 1:0.5:39.5
	c = cpmhalf(i, c)
	push!(cpmtab, c)
end

stardust =  repmat([200, 400, 600, 800, 1000, 1300, 1600, 1900, 2200, 2500, 3000, 35000, 4000, 4500, 5000, 6000, 7000, 8000, 9000, 10000]', 4)[1:length(cpmtab)]

## cp modifier table lookup
cpm(level::Real) = cpmtab[twicelevel(level)]

level(p::Pokebeast) = (p.twicelevel + 1) / 2
cpm(p::Pokebeast) = cpmtab[clamp(p.twicelevel, 1, length(cpmtab))]

function hp(p::Pokebeast)
	beast = stats[p.nr, :]
	return floor(Int, (beast[1, :stamina] + p.iv.stamina) * cpm(p))
end

function ivm(p::Pokebeast)
	"""IV modifier"""
	beast = stats[p.nr, :]
	return (beast[1, :attack] + p.iv.attack) * sqrt((beast[1, :stamina] + p.iv.stamina) * (beast[1, :defense] + p.iv.defense))
end

cp(p::Pokebeast) = floor(Int, ivm(p) * cpm(p)^2 / 10)

hp(name::String, level::Real, iv::IV) = hp(Pokebeast(name, level, iv))
cp(name::String, level::Real, iv::IV) = cp(Pokebeast(name, level, iv))

function allivms(nr::Integer, fast=true)
	beasts = Pokebeast[]
	ivms = Float64[]
	for a in 0:15, d in 0:15, s in 0:15
		p = Pokebeast(nr, 1, IV(s, a, d))
		push!(beasts, p)
		fast || push!(ivms, ivm(p))
	end
	if fast
		s, a, d = convert(Array, pkm.stats[nr, [:stamina, :attack, :defense]])
		r = collect(0:15)
		ivms = vec(broadcast(*, vec(sqrt(broadcast(*, s+r, d+r'))), a+r'))
	end
	return DataFrame(beast=beasts, ivm=ivms)
end

## This takes a while---it should not be too hard, though
function allivms()
	df = []
	for i in 1:nrow(stats)
		push!(df, allivms(i))
	end
	return vcat(df...)
end

using Distributions

Pei = Distributions.Categorical([0, 115, 0, 0, 161, 0, 0, 0, 0, 39] / 315)

function pei(dist=10)
	d = 0.0
	for i in 1:1000
		ei = rand(Pei)
		if ei == dist
			return d
		end
		d += ei
	end
end

export Pokebeast, cp, hp

end
