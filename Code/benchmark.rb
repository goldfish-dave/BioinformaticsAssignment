require 'benchmark'

test = lambda do |forks,algorithm,length,file,display,cores|
	if display
		d = "-d"
	else
		d = ""
	end
	Benchmark.measure do
		`./main -f #{(forks*cores).to_s} -l #{length.to_s} -o #{file.to_s} -a #{algorithm.to_s} #{d} +RTS -N#{cores.to_s}`
	end
end

nums = [1,2,4,8]
algorithms = ["stm","locking","bounding"]
motifLength = 8
file = "Data/aps_ref_Acyr_2.0_chrMT.fa"

algorithms.each { |a|
	nums.each { |i|
		nums.each { |j|
			puts "Algorithm \t #{a}, forks per core \t #{i.to_s}, cores \t #{j.to_s}"
			puts test.call(i,a,motifLength,file,false,j)
		}
	}
}
