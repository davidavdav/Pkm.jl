module Pkm

export Pokebeast, IV, cp, hp, search, setlevel, evolve, boss, candycost, stardustcost, maxout

using DataFrames
using DataArrays
using DataFramesMeta

import Base.search, Base.cp

## Static data preparation
statsfile = joinpath(dirname(dirname(@__FILE__)), "data", "stats.csv")
"The basics statistics"
stats = readtable(statsfile, separator=',')

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

## turn column evolve into an (empty) array of pokebeast numbers
stats[:evolve] = [[x for x in [parse(Int, x) for x in split(s)] if x > 0] for s in stats[:evolve]]

## initialize the cp modifier tables
cpmtab = let
	c = 0.094
	tab = [c]
	cpmstep = [0.009426125469, 0.008919025675, 0.008924905903, 0.00445946079]
	function cpmhalf(level, cpm)
		i = round(Int, div(level, 10)) + 1
		return sqrt.(cpm^2 + cpmstep[i])
	end
	for i in 1:0.5:39.5
		c = cpmhalf(i, c)
		push!(tab, c)
	end
	tab
end

stardusttab =  repmat([200, 400, 600, 800, 1000, 1300, 1600, 1900, 2200, 2500, 3000, 3500, 4000, 4500, 5000, 6000, 7000, 8000, 9000, 10000]', 4)[1:length(cpmtab)]
candytab = vcat(fill(1, 20), fill(2, 20), fill(3, 10), fill(4, 10), vec(repmat([6, 8, 10, 12, 15]', 4)))[1:length(cpmtab)]
bosshptab = [600, 600, 600, 1000, 1250] ## hp for cp computation
bosshp2tab = [600, 1800, 3000, 7500, 10000] ## hp in boss raids, last one is a guess
bosscpmtab = [√√x for x in [1, 2.5, 5, 7.5, 10]]

## types
"Type to hold the Individual Value"
struct IV
	stamina::Int8
	attack::Int8
	defense::Int8
end

const minIV = IV(0, 0, 0)
const maxIV = IV(15, 15, 15)

"Struct to hold a beast"
mutable struct Pokebeast
	nr::Int16
	twicelevel::Int8 ## 1..79
	iv::IV
	boss::Int8
	function Pokebeast(nr::Integer, twicelevel::Integer, iv::IV, boss::Integer=0)
		1 ≤ nr ≤ nrow(stats) || throw(DomainError())
		1 ≤ twicelevel ≤ length(cpmtab) || throw(DomainError())
		new(nr, twicelevel, iv, boss)
	end
end

Base.show(io::IO, p::Pokebeast) = if p.boss == 0
	@printf(io, "%s (%d) l:%3.1f iv:%d,%d,%d; cp: %d, hp: %d", name(p), p.nr, level(p), p.iv.stamina, p.iv.attack, p.iv.defense, cp(p), hp(p))
else
	@printf(io, "%s (%d) Raid boss; cp: %d, hp: %d", name(p), p.nr, cp(p), hp(p))
end
Base.show(io::IO, ::MIME"text/plain", p::Pokebeast) = show(io, p)

## twicelevel is an integer 1..79 corresponding to the normal levels 1.0--40.0
twicelevel(level::Real) = Int8(clamp(round(Int, 2level - 1), 1, length(cpmtab)))
level(twicelevel::Integer) = (twicelevel+1)/2

name(nr) = stats[nr, :name]
name(p::Pokebeast) = name(p.nr)
name2nr = Dict(row[:name] => Int16(row[:nr]) for row in eachrow(stats))
nr(name::String) = name2nr[name]
nr(p::Pokebeast) = p.nr
level(p::Pokebeast) = level(p.twicelevel)

## constructor from name and normal level
Pokebeast(name::String, level::Real, iv::IV) = Pokebeast(nr(name), twicelevel(level), iv)
Pokebeast(name::String, level::Real, stamina::Integer, attack::Integer, defense::Integer) = Pokebeast(name, level, IV(stamina, attack, defense))
Pokebeast(name::String, bosslevel::Int) = Pokebeast(nr(name), 79, maxIV, bosslevel)

## cp modifier table lookup
cpm(level::Real) = cpmtab[twicelevel(level)]
function cpm(p::Pokebeast)
	if p.boss > 0
		return bosscpmtab[p.boss]
	else
		return cpmtab[clamp(p.twicelevel, 1, length(cpmtab))]
	end
end
cpm{T<:Real}(levels::Array{T}) = [cpm(level) for level in levels]

hp(stamina, cpm) = floor.(Int, stamina * cpm)
hp(p::Pokebeast) = p.boss > 0 ? bosshp2tab[p.boss] : hp(stats[p.nr, :stamina] + p.iv.stamina, cpm(p))
hp(name::String, level::Real, iv::IV) = hp(Pokebeast(name, level, iv))

"IV modifier"
function ivm(p::Pokebeast)
	beast = stats[p.nr, :]
	if p.boss > 0
		return (beast[1, :attack] + 15) * sqrt.((beast[1, :defense] + 15) * bosshptab[p.boss])
	else
		return (beast[1, :attack] + p.iv.attack) *
		        sqrt((beast[1, :stamina] + p.iv.stamina) * (beast[1, :defense] + p.iv.defense))
	end
end

"Combat power"
cp(ivm, cpm) =  floor.(Int, ivm * cpm^2 / 10)
cp(p::Pokebeast) = floor.(Int, ivm(p) * cpm(p)^2 / 10)
cp(name::String, level::Real, iv::IV) = cp(Pokebeast(name, level, iv))

## these functions should no longer be necessary
function allivms(nr::Integer, twicelevel=1, fast=true)
	beasts = Pokebeast[]
	ivms = Float64[]
	for a in 0:15, d in 0:15, s in 0:15 ## loop order depends on broadcasts below
		p = Pokebeast(nr, twicelevel, IV(s, a, d))
		push!(beasts, p)
		fast || push!(ivms, ivm(p))
	end
	s, a, d = convert(Array, stats[nr, [:stamina, :attack, :defense]])
	r = collect(0:15)
	hps = floor.(Int, cpmtab[twicelevel] * repmat(s+r, 16*16))
	if fast
		ivms = vec(broadcast(*, vec(sqrt.(broadcast(*, s+r, d+r'))), a+r'))
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

"""Computes the IV stats as reported by the game"""
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
	return @byrow! df begin
	    @newcol overall::Array{Int}
		@newcol best::Array{String}
	    @newcol val::Array{Int}
		@newcol iv::Array{Int}
		@newcol ivp::Array{Int}
		:overall, :val, :best, :iv = maxstat(:beast)
		minivm = ivm(Pokebeast(nr(:beast), 1, minIV))
		maxivm = ivm(Pokebeast(nr(:beast), 1, maxIV))
		:ivp = round(Int, 1000 * (:ivm - minivm) / (maxivm - minivm))
	end
end


function search(nr::Integer, c::Integer, h::Integer, s::Integer, overall::Integer=-1, best::String="", val::Integer=-1)
	twicelevels = find(s .== stardusttab)
	length(twicelevels) > 0 || error("Stardust value not found")
	ivrange = 0:15
	s, a, d = convert(Array, stats[nr, [:stamina, :attack, :defense]])
	hps = floor.(Int, broadcast(*, s + ivrange, cpmtab[twicelevels]')) ## hp matrix
	sti, twi = ind2sub((16,length(twicelevels)), find(hps .== h))
	res = []
	for (st, tw) in zip(ivrange[sti], twicelevels[twi])
		ivms = vec(broadcast(*, sqrt.((s+st) * (d+ivrange)), a + ivrange'))
		cps = cp(ivms, cpmtab[tw])
		cpi = find(cps .== c)
		di, ai = ind2sub((16,16), cpi)
		for (i, j, k) in zip(ai, di, cpi)
			push!(res, (Pokebeast(nr, tw, IV(st, ivrange[i], ivrange[j])), ivms[k]))
		end
	end
	x = addstats(DataFrame(beast=[r[1] for r in res], ivm=[r[2] for r in res], hp=h, cp=c))
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

function search0(nr::Integer, c::Integer, h::Integer, s::Integer, best::String="", val::Integer=-1, overall::Integer=-1)
	twicelevels = find(s .== stardusttab)
	length(twicelevels) > 0 || error("Stardust value not found")
	df = []
	for tw in twicelevels
		ivms = allivms(nr, tw)
		ivms[:cp] = floor.(Int, ivms[:ivm] * cpmtab[tw]^2 / 10)
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

## old interface
search(nr::Integer, c::Integer, h::Integer, s::Integer, best::String, val::Integer, overall::Integer) = search(nr, c, h, s, overall, best, val)
search(name::String, args...) = search(nr(name), args...)

## catchall, not necessarily correct for every beast.  This evolves until no longer possible.
## This function returns an array of Inr
function evolve(nr::Integer, times::Integer=1)::Array{Integer}
	0 < nr < nrow(stats) || throw(DomainError())
	nrs = stats[nr, :evolve]
	if length(nrs) > 0
		if times > 1
			return vcat([evolve(n, times-1) for n in nrs]...)
		else
			return nrs
		end
	else
		return [nr]
	end
end

evolve(p::Pokebeast, times::Integer=1) = [Pokebeast(nr, p.twicelevel, p.iv) for nr in evolve(p.nr, times)]

function update(df::AbstractDataFrame, func::Function, args...)
	df = @byrow! df begin
	    :beast = func(:beast, args...)
		:ivm = ivm(:beast)
		:hp = hp(:beast)
		:cp = cp(:beast)
	end
	return addstats(df)
end

function evolve(df::AbstractDataFrame, times::Integer=1)
	beasts = Pokebeast[]
	ivms = Float64[]
	hps = Int[]
	cps = Int[]
	for row in eachrow(df)
		for beast in evolve(row[:beast], times)
			push!(beasts, beast)
			push!(ivms, ivm(beast))
			push!(hps, hp(beast))
			push!(cps, cp(beast))
		end
	end
	return addstats(DataFrame(beast=beasts, ivm=ivms, hp=hps, cp=cps))
end

boss(p::Pokebeast, bosslevel::Integer) = Pokebeast(p.nr, p.twicelevel, maxIV, bosslevel)
boss(df::AbstractDataFrame, bosslevel::Integer) = update(df, boss, bosslevel)

up(p::Pokebeast, times::Integer=1) = Pokebeast(p.nr, p.twicelevel + times, p.iv)
up(df::AbstractDataFrame, times::Integer=1) = update(df, up, times)

setlevel(p::Pokebeast, level::Real) = Pokebeast(p.nr, twicelevel(level), p.iv)
setlevel(df::AbstractDataFrame, level::Real) = update(df, setlevel, level)

stardustcost(level::Real) = cumsum(stardusttab)[twicelevel(level)]
candycost(level::Real) = cumsum(candytab)[twicelevel(level)]

stardustcost(p::Pokebeast, level::Real) = twicelevel(level) ≤ p.twicelevel ? 0 : sum(stardusttab[p.twicelevel:twicelevel(level)-1])
candycost(p::Pokebeast, level::Real) = twicelevel(level) ≤ p.twicelevel ? 0 : sum(candytab[p.twicelevel:twicelevel(level)-1])

stardustcost(from::Real, to::Real) = stardustcost(to) - stardustcost(from)
candycost(from::Real, to::Real) = candycost(to) - candycost(from)

mylevel = 30.0
function setlevel(level::AbstractFloat)
	global mylevel = level
end

maxout(p::Pokebeast) = [setlevel(x, mylevel) for x in evolve(p, 2)]

function maxout(df::AbstractDataFrame)
	cc = [candycost(level(p), mylevel) for p in df[:beast]]
	sc = [stardustcost(level(p), mylevel) for p in df[:beast]]
    beasts = vcat([maxout(p) for p in df[:beast]]...)
	df = DataFrame(beast=beasts, ivm=[ivm(p) for p in beasts], hp=[hp(p) for p in beasts], cp=[cp(p) for p in beasts])
	df = addstats(df)
	df[:candy] = cc
	df[:stardust] = sc
	return df
end

end
