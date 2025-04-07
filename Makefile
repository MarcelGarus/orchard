plum.soil: $(shell find compiler -type f)
	soil ../martinaise/martinaise.soil compile compiler/plum.mar
	mv compiler/plum.soil plum.soil

playground.ground: $(shell find workspace -type f) plum.soil
	soil plum.soil workspace/test/playground
	mv workspace/test/playground.ground playground.ground
run-playground: playground.ground
	ground playground.ground

fibonacci.ground: $(shell find workspace -type f) plum.soil
	soil plum.soil workspace/test/fibonacci
	mv workspace/test/fibonacci.ground fibonacci.ground
run-fibonacci: fibonacci.ground
	ground fibonacci.ground
