module Exp

using Distributions
using ..Pkm

Pegg = Distributions.Categorical([0, 115, 0, 0, 161, 0, 0, 0, 0, 39] / 315)

## how long until finding a 10K egg?  We know the exact answer: (2Pegg[2] + 5Pegg[5]) / Pegg[10]
function pegg(dist=10)
	d = 0.0
	while true
		egg = rand(Pegg)
		if egg == dist
			return d
		end
		d += egg
	end
end

## how long until we have 9 10k eggs?
function p9egg(;dist=10, ntarget=9, spindist=0.0, logging=false)
	d = 0.0
	notfound = trues(ntarget)
	togo = zeros(Float64, ntarget)
	inc = 0
	infinci = 0
	while any(notfound)
		## walk
		mindist = minimum(togo)
		togo[notfound] -= mindist
		d += mindist
		if infinci != 0 && togo[infinci] ≤ 0
			infinci = 0
		end
		## spin eggs
		newspots = Int[]
		for i in find(togo .≤ 0)
			egg = rand(Pegg)
			togo[i] = egg
			if egg == dist
				notfound[i] = false
			else
				push!(newspots, i)
				inc += 1
			end
			d += spindist
		end
		## account for infinite incubator
		if infinci == 0 && length(newspots) > 0
			infinci = newspots[indmin(togo[newspots])]
			inc -= 1
		end
	end
	return [d, inc]
end

p9egg(N::Integer; kwargs...) = vec(mean(vcat([p9egg(;kwargs...)' for i in 1:N]...), 1))

## What is the distribution of egg-IVs?  Implement a presumed generating algorithm
import Base.rand
rand(::Type{Pkm.IV}) = Pkm.IV(maximum(reshape(rand(0:15, 9), 3, 3), 2)...)
rand(::Type{IV}, N::Integer) = [rand(IV) for i in 1:N]
randiv(N::Integer) = [iv(x) for x in rand(IV, N)]

end
