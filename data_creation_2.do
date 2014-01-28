clear
log using "Z:\home\david\Dropbox\Grad School\RAWork\Compliance Paper\data\compliance\final_reverse_lags.smcl", replace

insheet using "Z:\home\david\Dropbox\Grad School\RAWork\Compliance Paper\data\compliance\pre_stata_slim.csv", clear

sort dyad year
duplicates drop dyad year, force
tsset dyad year

by dyad: gen mid1 = dispnum3[_n+1]
by dyad: gen mid2 = dispnum3[_n+2]
by dyad: gen mid3 = dispnum3[_n+3]
by dyad: gen mid4 = dispnum3[_n+4]
by dyad: gen mid5 = dispnum3[_n+5]
by dyad: gen mid6 = dispnum3[_n+6]
by dyad: gen mid7 = dispnum3[_n+7]
by dyad: gen mid8 = dispnum3[_n+8]
by dyad: gen mid9 = dispnum3[_n+9]
by dyad: gen mid10 = dispnum3[_n+10]
by dyad: gen mid11 = dispnum3[_n+11]
by dyad: gen mid12 = dispnum3[_n+12]
by dyad: gen mid13 = dispnum3[_n+13]
by dyad: gen mid14 = dispnum3[_n+14]
by dyad: gen mid15 = dispnum3[_n+15]
by dyad: gen mid16 = dispnum3[_n+16]
by dyad: gen mid17 = dispnum3[_n+17]
by dyad: gen mid18 = dispnum3[_n+18]
by dyad: gen mid19 = dispnum3[_n+19]
by dyad: gen mid20 = dispnum3[_n+20]

by dyad: gen claim1 = claimdy[_n+1]
by dyad: gen claim2 = claimdy[_n+2]
by dyad: gen claim3 = claimdy[_n+3]
by dyad: gen claim4 = claimdy[_n+4]
by dyad: gen claim5 = claimdy[_n+5]
by dyad: gen claim6 = claimdy[_n+6]
by dyad: gen claim7 = claimdy[_n+7]
by dyad: gen claim8 = claimdy[_n+8]
by dyad: gen claim9 = claimdy[_n+9]
by dyad: gen claim10 = claimdy[_n+10]
by dyad: gen claim11 = claimdy[_n+11]
by dyad: gen claim12 = claimdy[_n+12]
by dyad: gen claim13 = claimdy[_n+13]
by dyad: gen claim14 = claimdy[_n+14]
by dyad: gen claim15 = claimdy[_n+15]
by dyad: gen claim16 = claimdy[_n+16]
by dyad: gen claim17 = claimdy[_n+17]
by dyad: gen claim18 = claimdy[_n+18]
by dyad: gen claim19 = claimdy[_n+19]
by dyad: gen claim20 = claimdy[_n+20]

outsheet using "Z:\home\david\Dropbox\Grad School\RAWork\Compliance Paper\data\compliance\post_stata_slim.csv", comma replace
