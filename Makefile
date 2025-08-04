plum.soil: $(shell find compiler -type f)
	soil ../martinaise/martinaise.soil compile compiler/plum.mar
	mv compiler/plum.soil plum.soil

playground.ground: $(shell find plum -type f) plum.soil
	soil plum.soil plum/test/playground
	mv plum/test/playground.ground playground.ground
run-playground: playground.ground
	ground playground.ground

fibonacci.ground: $(shell find plum -type f) plum.soil
	soil plum.soil plum/test/fibonacci
	mv plum/test/fibonacci.ground fibonacci.ground
run-fibonacci: fibonacci.ground
	ground fibonacci.ground
