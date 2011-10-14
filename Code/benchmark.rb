require 'benchmark'

class Array
	def average
		self.inject(0) { |sum,x| sum += x }
	end
	def stdev
		avg = self.average
		var = self.inject(0) { |variance,x| variance += (x-avg) ** 2 }
		Math.sqrt(var)
	end
end

run_main = lambda do |forks,algorithm,length,file,display,cores|
	if display
		d = "-d"
	else
		d = ""
	end
	Benchmark.measure do
		`./main -f #{(forks*cores).to_s} -l #{length.to_s} -o #{file.to_s} -a #{algorithm.to_s} #{d} +RTS -N#{cores.to_s}`
	end
end


def test1(n=1,l=8,file)
# in this test we compare the different algorithms on 1 core
#   for the algorithms which can run in multiple forks we test 
#   them on [1,2,4,8] forks
#
# output format is a csv:
# 	algorithm,forks,cores,time, stdev
	algorithms = ["simple","bounding","locking","stm"]
	forks = [1,2,4,8]
	
	algorithms.each do |a|
		forks.each do |f|
			results = []
			n.times { results << test.call(f,a,l,file,false,1).real }
			puts "#{a},#{f},1,#{results.average},#{results.stdev}"
		end
	end
end

def test2(n,l,file)
# in this test we pit the locking algorithm against the stm algorithm
# 	by changing the number of cores, using the fastest forking ratio
#
# output format is a csv:
# 	algorithm,forks,cores,time, stdev
	algorithms = ["locking","stm"]
	forks = 
	cores = [1,2,4,8]

	algorithm.each do |a|
		cores.each do |c|
			results = []
			n.times { results << run_main(f,a,l,file,c) }
			puts "#{a},#{f},1,#{results.average},#{results.stdev}"
		end
	end
end			

def test3(n,l,file)
# in this test we take the most scalable algorithm and test it
# over all fork and core combinations, in order to find the
# speedup and efficiency
	algorithms = 
	forks = [1,2,4,8]
	cores = [1,2,4,8]

	cores.each do |c|
		forks.each do |f|
			results = []
			n.times { results << run_main(f,a,l,file,c) }
			puts "#{a},#{f},1,#{results.average},#{results.stdev}"
		end
	end
end			

def main
	file = "Data/aps_ref_Acyr_2.0_chrMT.fa"
	test1(10,8,file)
end