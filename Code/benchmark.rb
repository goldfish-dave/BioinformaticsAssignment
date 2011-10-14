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

def run_test(forks,algorithm,length,file,cores)
	Benchmark.measure do
		`./main -f #{(forks*cores).to_s} -l #{length.to_s} -o #{file.to_s} -a #{algorithm.to_s} +RTS -N#{cores.to_s}`
	end
end


def testA(n,l,file)
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
			n.times { results << run_test(f,a,l,file,1).real }
			puts "#{a},#{f},1,#{results.average},#{results.stdev}"
		end
	end
end

## main ##
#
file = "Data/aps_ref_Acyr_2.0_chrMT.fa"
testA(10,8,file)
