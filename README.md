# Impact of Data Discretization on Causal Inference for a Real-World Environment-Health Scenario
This is the supplementary repository for the research on discretization on real world applicatin scenarios conducted as part of the lecture "Causal Inference - Theory and Applications in Enterprise Computing" at the Hasso Plattner Institute in Potsdam.

## Package Specifications
1. R Version 4.0.0 (2020-04-24)
2. causaleffect 1.3.10
3. bnlearn 4.5
4. ckmeans 4.3.2

## Pipeline

### Setup
`install-packages.R`

### Data generation
`data-generator.R`

### Causal Strucute Learning
`discretized-csl.R`

### Causal Inference
`effect-estimation.R`

## Helpful commands
### Interactive graph (igraph)
`tkplot(graph, vertex.size=35, vertex.label.family="sans", vertex.color="white")`
