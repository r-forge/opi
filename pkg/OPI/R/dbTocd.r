    # Convert HFA dB to cd/m^2
dbTocd <- function(db) {
    asb <- 10^((40-db)/10) 
    return (asb/pi)
}

    # Convert cd/m^2 to HFA dB
cdTodb <- function(cd) {
    asb <- cd * pi
    return (40 - 10*log10(asb))
}
