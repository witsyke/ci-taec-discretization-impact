source("./helper.R")
load("./nets/mehra-complete.rda")

igraph.from.bngraph <- igraph.from.graphNEL(bnlearn::as.graphNEL(bn))

adj.matrix.from.bngraph <- amat(bn)

igraph.from.bnmatrix<- graph_from_adjacency_matrix(adj.matrix.from.bngraph)


igraph.from.hand <- graph.formula(Zone -+ co, Year -+ co, Type -+ co, Zone -+ so2, Type -+ so2, Year -+ so2, co -+ so2, Zone -+ o3, Year -+ o3, Type -+ o3, Zone -+ no2, o3 -+ no2, co -+ no2, Altitude -+ no2, pm10 -+ no2, co -+ ssr, no2 -+ ssr, pm10 -+ ssr, o3 -+ ssr, Hour -+ ssr, blh -+ ssr, t2m -+ ssr, Longitude -+ ssr, ws -+ ssr, wd -+ ssr, Latitude -+ ssr, Altitude -+ ssr, Month -+ tp, ws -+ tp, Year -+ tp, Hour -+ tp, blh -+ o3, blh -+ no2, ws -+ blh, Longitude -+ blh, t2m -+ blh, Hour -+ blh, Month -+ blh, Region -+ blh, t2m -+ ws, Latitude -+ t2m, wd -+ t2m, Month -+ t2m, Year -+ t2m, Hour -+ t2m, Day -+ wd, Month -+ wd, Year -+ wd, Region -+ wd, Month -+ CVD60, Season -+ CVD60, Year -+ CVD60, Region -+ CVD60, pm10 -+ pm2.5, Type -+ pm2.5, Year -+ pm2.5, Region -+ pm2.5, Year -+ pm10, Zone -+ pm10, Type -+ pm10, Day -+ ws, Month -+ ws, Latitude -+ ws, wd -+ ws, Year -+ ws, Longitude -+ ws, Type -+ no2, Year -+ no2, so2 -+ ssr, ssr -+ tp, wd -+ blh)


difference(igraph.from.bngraph, igraph.from.bnmatrix)
difference(igraph.from.bnmatrix, igraph.from.bngraph)

difference(igraph.from.bngraph, igraph.from.hand)
difference(igraph.from.hand, igraph.from.bngraph)

difference(igraph.from.bnmatrix, igraph.from.hand)
difference(igraph.from.hand, igraph.from.bnmatrix)


tkplot(igraph.from.bngraph, vertex.size=35, vertex.label.family="sans", vertex.color="white")
tkplot(igraph.from.bnmatrix, vertex.size=35, vertex.label.family="sans", vertex.color="white")
tkplot(igraph.from.hand, vertex.size=35, vertex.label.family="sans", vertex.color="white")

#str(igraph.from.bngraph)
#str(igraph.from.bnmatrix)
#str(igraph.from.hand)

effect.igraph.from.bngraph <- causal.effect(y = "so2", x = "co", z = NULL, G = igraph.from.bngraph, expr = T)
effect.igraph.from.bnmatrix <- causal.effect(y = "so2", x = "co", z = NULL, G = igraph.from.bnmatrix, expr = T)
effect.igraph.from.hand <- causal.effect(y = "so2", x = "co", z = NULL, G= igraph.from.hand, expr = T)

cat(eff)
cat(eff2)