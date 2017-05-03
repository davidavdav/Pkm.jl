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

name2nr = Dict(row[:name] => Int16(row[:nr]) for row in eachrow(stats))

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
		1 ≤ twicelevel ≤ length(cpmtab) || throw(DomainError())
		new(nr, twicelevel, iv)
	end
end

Base.show(io::IO, p::Pokebeast) = @printf(io, "%s (%d) l:%3.1f iv:%d,%d,%d", name(p), p.nr, level(p), p.iv.stamina, p.iv.attack, p.iv.defense)
Base.show(io::IO, ::MIME"text/plain", p::Pokebeast) = show(io, p)

twicelevel(level::Real) = Int8(clamp(round(Int, 2level - 1), 1, length(cpmtab)))
level(twicelevel::Integer) = (twicelevel+1)/2

name(p::Pokebeast) = stats[p.nr, :name]
nr(name::String) = get(name2nr, name, KeyError("Beast not found"))
level(p::Pokebeast) = level(p.twicelevel)

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

stardust =  repmat([200, 400, 600, 800, 1000, 1300, 1600, 1900, 2200, 2500, 3000, 3500, 4000, 4500, 5000, 6000, 7000, 8000, 9000, 10000]', 4)[1:length(cpmtab)]

## cp modifier table lookup
cpm(level::Real) = cpmtab[twicelevel(level)]
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

function allivms(nr::Integer, twicelevel=1, fast=true)
	beasts = Pokebeast[]
	ivms = Float64[]
	for a in 0:15, d in 0:15, s in 0:15 ## loop order depends on broadcasts below
		p = Pokebeast(nr, twicelevel, IV(s, a, d))
		push!(beasts, p)
		fast || push!(ivms, ivm(p))
	end
	s, a, d = convert(Array, pkm.stats[nr, [:stamina, :attack, :defense]])
	r = collect(0:15)
	hps = floor(Int, cpmtab[twicelevel] * repmat(s+r, 16*16))
	if fast
		ivms = vec(broadcast(*, vec(sqrt(broadcast(*, s+r, d+r'))), a+r'))
	end
	return DataFrame(beast=beasts, ivm=ivms, hp=hps)
end

## This takes a while---it should not be too hard, though
function allivms()
	df = []
	for i in 1:nrow(stats)
		push!(df, allivms(i))
	end
	return vcat(df...)
end

function maxstat(p::Pokebeast)
	iv = [p.iv.stamina, p.iv.attack, p.iv.defense]
	maxiv = maximum(iv)
	s = sum(iv)
	val = 0
	for thres in [8, 13, 15]
		if maxiv < thres break end
		val += 1
	end
	overall = 0
	for thres in [23, 30, 37]
		if s < thres break end
		overall += 1
	end
	return overall, val, "sad"[find(iv.==maxiv)], round(Int, 100* s / 45)
end

function addstats(df::AbstractDataFrame)
	nr = df[1,:beast].nr
	minivm = ivm(Pokebeast(nr, 1, IV(0,0,0)))
	maxivm = ivm(Pokebeast(nr, 1, IV(15,15,15)))
	return @byrow! df begin
	    @newcol overall::DataArray{Int}
		@newcol best::DataArray{String}
	    @newcol val::DataArray{Int}
		@newcol iv::DataArray{Int}
		@newcol ivp::DataArray{Int}
		:overall, :val, :best, :iv = maxstat(:beast)
		:ivp = round(Int, 1000 * (:ivm - minivm) / (maxivm - minivm))
	end
end

function search(nr::Integer, c::Integer, h::Integer, s::Integer, best::String="", val::Integer=-1, overall::Integer=-1)
	twicelevels = find(s .== stardust)
	length(twicelevels) > 0 || error("Stardust value not found")
	df = []
	for tw in twicelevels
		ivms = allivms(nr, tw)
		ivms[:cp] = floor(Int, ivms[:ivm] * cpmtab[tw]^2 / 10)
		push!(df, ivms)
	end
	x = @where(vcat(df...), :hp .== h, :cp .== c)
	x = addstats(x)
	if length(best) > 0
		best = "sad"[find([contains(best, s) for s in split("sad", "")])]
		x = @where(x, :best .== best)
	end
	if val ≥ 0
		x = @where(x, :val .== val)
	end
	if overall ≥ 0
		x = @where(x, :overall .== overall)
	end
	return x
end

search(name::String, args...) = search(nr(name), args...)

## catchall, not necessarily correct for every beast.
evolve(nr::Integer, times::Integer=1) = nr + times
evolve(p::Pokebeast) = Pokebeast(evolve(p.nr), p.twicelevel, p.iv)
function evolve(df::AbstractDataFrame)
	df = @byrow! df begin
	    :beast = evolve(:beast)
		:ivm = ivm(:beast)
		:hp = hp(:beast)
		:cp = cp(:beast)
	end
	return addstats(df)
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
