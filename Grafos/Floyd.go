// go build Floyd.go
// ./Floyd -file=prueba.txt
package main
 
import (
	"bufio"
	"fmt"
	"flag"
	"log"
	"os"
	"strconv"
	"time"
)
 
// A Graph is the interface implemented by graphs that
// this algorithm can run on.
type Graph interface {
	Vertices() []Vertex
	Neighbors(v Vertex) []Vertex
	Weight(u, v Vertex) int
}
 
// Nonnegative integer ID of vertex
type Vertex int
 
// ig is a graph of integers that satisfies the Graph interface.
type ig struct {
	vert  []Vertex
	edges map[Vertex]map[Vertex]int
}
 
func (g ig) edge(u, v Vertex, w int) {
	if _, ok := g.edges[u]; !ok {
		g.edges[u] = make(map[Vertex]int)
	}
	g.edges[u][v] = w
}

func (g ig) Vertices() []Vertex { 
	return g.vert 
}

func (g ig) Neighbors(v Vertex) (vs []Vertex) {
	for k := range g.edges[v] {
		vs = append(vs, k)
	}
	return vs
}

func (g ig) Weight(u, v Vertex) int {
	return g.edges[u][v] 
}

func (g ig) path(vv []Vertex) (s string) {
	if len(vv) == 0 {
		return ""
	}
	s = strconv.Itoa(int(vv[0]))
	for _, v := range vv[1:] {
		s += " -> " + strconv.Itoa(int(v))
	}
	return s
}
 
const Infinity = int(^uint(0) >> 1)
 
func FloydWarshall(g Graph) (dist map[Vertex]map[Vertex]int, next map[Vertex]map[Vertex]*Vertex) {
	vert := g.Vertices()
	dist = make(map[Vertex]map[Vertex]int)
	next = make(map[Vertex]map[Vertex]*Vertex)
	for _, u := range vert {
		dist[u] = make(map[Vertex]int)
		next[u] = make(map[Vertex]*Vertex)
		for _, v := range vert {
			dist[u][v] = Infinity
		}
		dist[u][u] = 0
		for _, v := range g.Neighbors(u) {
			v := v
			dist[u][v] = g.Weight(u, v)
			next[u][v] = &v
		}
	}
	for _, k := range vert {
		for _, i := range vert {
			for _, j := range vert {
				if dist[i][k] < Infinity && dist[k][j] < Infinity {
					if dist[i][j] > dist[i][k]+dist[k][j] {
						dist[i][j] = dist[i][k] + dist[k][j]
						next[i][j] = next[i][k]
					}
				}
			}
		}
	}
	return dist, next
}

func main() {
	// Argument
	argPtr := flag.String("file", "", "File name")
	flag.Parse()

	if *argPtr == ""{
		log.Fatalf("Missing required -file flag\n")
		os.Exit(3)
	}

	// Open file
	file, err := os.Open(*argPtr)
	if err != nil {
		log.Fatalf("Open file: %s", err)
		os.Exit(2)
	}
	defer file.Close()

	// Split into spaces
	scanner := bufio.NewScanner(file)
	scanner.Split(bufio.ScanWords)

	// Read # nodes
	scanner.Scan()
	num_nodes, _ := strconv.Atoi(scanner.Text())
	// fmt.Println("LOAD")
	// fmt.Println("# Nodes:", num_nodes)

	// Read # edges
	scanner.Scan()
	// num_edges := scanner.Text()
	// fmt.Println("# Edges:", num_edges)

	// Create nodes 0 to n
	nodes := make([]Vertex, num_nodes)
	for i := 0; i < num_nodes; i++{
		nodes[i] = Vertex(i)
	}

	// Create graph
	g := ig{nodes, make(map[Vertex]map[Vertex]int)}
	// Load edges from file
	for scanner.Scan() {
		src, _ := strconv.Atoi(scanner.Text())

		scanner.Scan()
		dest, _ := strconv.Atoi(scanner.Text())

		scanner.Scan()
		weight, _ := strconv.Atoi(scanner.Text())

		g.edge(Vertex(src), Vertex(dest), weight)

		// fmt.Println(src, dest, weight)  
	}
	// fmt.Println("END LOAD")

	if err != nil {
		log.Fatalf("LoadGraph: %s", err)
	}
 
	// dist, _ := FloydWarshall(g)
	
	start := time.Now()
	
	_, _ = FloydWarshall(g)

	finish := time.Now()

	// Print dist
	// fmt.Println("pair\tdist")
	// for u, m := range dist {
	// 	for v, d := range m {
	// 		if u != v {
	// 			fmt.Printf("%d -> %d\t%3d\n", u, v, d)
	// 		}
	// 	}
	// }	

	// Posix time
	elapsed := (float64(finish.UnixNano()) - float64(start.UnixNano()) ) / 1000000000 
	fmt.Printf("%.9f",elapsed)
}