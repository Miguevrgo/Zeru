package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Vector3 struct {
	x, y, z float32
}

func createVector(x, y float32) Vector3 {
	return Vector3{x: x, y: y, z: 0.0}
}

func main() {
	pos := createVector(10.5, 20.0)
	numbers := [3]int32{1, 2, 3}
	dynNumbers := make([]uint32, 0)
	dynNumbersExplicit := make([]int32, 0)

	_ = numbers
	_ = dynNumbers
	_ = dynNumbersExplicit

	count := 100

	if pos.x < 50.0 {
		fmt.Printf("Position is small: %v\n", pos.x)
	} else if pos.x > 100.0 {
		fmt.Printf("Position is big: %v\n", pos.x*2.0)
	} else {
		fmt.Fprintf(os.Stderr, "Position should not be inside [50-100]\n")
	}

	for count > 0 {
		count = count - 1
		// Clamp method not available on primitives (requires logic)
		val := float32(count)
		if val < 0 { val = 0 }
		if val > 200 { val = 200 }
		count -= int(val)
	}

	content, _ := os.ReadFile("input.txt")
	fileContent := string(content)
	outputFile, _ := os.Create("output.txt")
	defer outputFile.Close()
	
	lines := strings.Split(fileContent, "\n")

	for _, line := range lines {
		if strings.Contains(line, "=") {
			_, part1, found := strings.Cut(line, "=")
			
			if found {
				// Option type not available (uses multi-return)
				key, err := strconv.Atoi(strings.TrimSpace(part1))
				
				if err == nil {
					fmt.Fprintf(outputFile, "%d", key)
				}
			}
		}
	}
}
