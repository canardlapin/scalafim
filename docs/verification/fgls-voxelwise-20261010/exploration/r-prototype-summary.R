fs <- commandArgs(TRUE); d <- do.call(rbind, lapply(fs, readRDS)); d$key <- paste(d$rep, d$vox, sep=":")
d$key <- paste(fs[1], d$key)
eng <- unique(d$eng)
s <- do.call(rbind, lapply(eng, function(e) { a <- d[d$eng == e & d$vox == 0 & d$ok, ]; b <- d[d$eng == e & d$vox == 1 & d$ok, ]
  data.frame(engine = e, n = nrow(a), calib = mean(a$var)/var(a$est), varEst = var(a$est), meanVar = mean(a$var), rejT = mean(a$rejT), rejF = mean(a$rejF), cover = mean(b$cover), rmse = sqrt(mean(c(a$se2,b$se2))), df = mean(a$df)) }))
print(s, digits = 4)
