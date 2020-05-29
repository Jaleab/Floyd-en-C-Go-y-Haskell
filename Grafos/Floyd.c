// gcc Floyd.c -o Floyd
// ./Floyd prueba.txt

#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
 
typedef struct{
	int sourceVertex, destVertex;
	int edgeWeight;
}edge;
 
typedef struct{
	int vertices, edges;
	edge* edgeMatrix;
}graph;
 
graph loadGraph(char* fileName){
	FILE* fp = fopen(fileName,"r");
 
	graph G;
	int i;
 
	fscanf(fp,"%d%d",&G.vertices,&G.edges);
 
	G.edgeMatrix = (edge*)malloc(G.edges*sizeof(edge));
 
	for(i=0;i<G.edges;i++)
		fscanf(fp,"%d%d%d",&G.edgeMatrix[i].sourceVertex,&G.edgeMatrix[i].destVertex,&G.edgeMatrix[i].edgeWeight);
 
	fclose(fp);
 
	return G;
}
 
void floydWarshall(graph g){
	int processWeights[g.vertices][g.vertices], processedVertices[g.vertices][g.vertices];
	int i,j,k;
 
	for(i=0;i<g.vertices;i++)
		for(j=0;j<g.vertices;j++){
			processWeights[i][j] = SHRT_MAX;
			processedVertices[i][j] = (i!=j)?j:0;
		}
 
	for(i=0;i<g.edges;i++)
		processWeights[g.edgeMatrix[i].sourceVertex][g.edgeMatrix[i].destVertex] = g.edgeMatrix[i].edgeWeight;
 
	for(i=0;i<g.vertices;i++)
		for(j=0;j<g.vertices;j++)
			for(k=0;k<g.vertices;k++){
				if(processWeights[j][i] + processWeights[i][k] < processWeights[j][k]){
					processWeights[j][k] = processWeights[j][i] + processWeights[i][k];
					processedVertices[j][k] = processedVertices[j][i];
				}
			}
 
	// printf("pair    dist   path");
	// for(i=0;i<g.vertices;i++)
	// 	for(j=0;j<g.vertices;j++){
	// 		if(i!=j){
	// 			printf("\n%d -> %d %3d %5d",i,j,processWeights[i][j],i);
	// 			k = i;
	// 			do{
	// 				k = processedVertices[k][j];
	// 				printf("->%d",k);
	// 			}while(k!=j);
	// 		}
	// 	}
	// printf("\n");
}
 
int main(int argC,char* argV[]){
	if(argC!=2)
	{
		printf("Usage : Floyd <file containing graph data>");
	}
	else
	{
		struct timespec start;
		struct timespec finish;

		graph g = loadGraph(argV[1]);

		clock_gettime(CLOCK_MONOTONIC, &start);

		floydWarshall(g);
		
		clock_gettime(CLOCK_MONOTONIC, &finish);

		double elapsed = (finish.tv_sec - start.tv_sec);
		elapsed += (finish.tv_nsec - start.tv_nsec) / 1000000000.0;

		printf("Elapsed: %.9lf\n", elapsed);
	}
	return 0;
}