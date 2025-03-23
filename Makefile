plum.soil: $(shell find compiler -type f)
	soil ../martinaise/martinaise.soil compile compiler/plum.mar
	mv compiler/plum.soil plum.soil

output.ground: $(shell find workspace -type f) plum.soil
	soil plum.soil

run: output.ground
	ground output.ground
