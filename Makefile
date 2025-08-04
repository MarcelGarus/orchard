plum.soil: $(shell find martinaise/plum -type f)
	soil ../martinaise/martinaise.soil compile martinaise/plum/plum.mar
	mv martinaise/plum/plum.soil plum.soil

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
