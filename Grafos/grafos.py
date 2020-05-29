#Example: python grafos.py -n 5 -e 8 -d -num 5
import argparse
import networkx as nx
import random

def generate_graphs(nodes, edges, graphType, outputFile):
	file = open(outputFile, "w")
	if graphType == True:
		graph = nx.dense_gnm_random_graph(nodes, edges)
	else:
		graph = nx.gnm_random_graph(nodes, edges, directed=False)

	for u, v in graph.edges():
		graph.add_weighted_edges_from([(u, v, random.randint(0, 100))])

	file.write(str(nodes) + " " + str(edges) + "\n")
	for line in nx.generate_edgelist(graph,data=['weight']):
		file.write(line + "\n")

if __name__ == '__main__':
	parser = argparse.ArgumentParser()
	parser.add_argument("--nodes", "-n", help="Set number of nodes", required=True, type=int)
	parser.add_argument("--edges", "-e", help="Set number of edges", required=True, type=int)
	parser.add_argument("--densegraph","-d", help="If true uses dense function", action="store_true")
	parser.add_argument("--number", "-num", help="Number of graphs", required=True, type=int)

	args = parser.parse_args()
	if args.densegraph == True:
		graphType = "dense"
	else:
		graphType = "sparse"

	for x in range(args.number):
		outputName = graphType + str(x) + ".txt"
		generate_graphs(args.nodes, args.edges, args.densegraph, outputName)