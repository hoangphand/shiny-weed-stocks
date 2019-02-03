database = read.csv('export-3-months.csv')

# change symbol id to symbol names
database$symbol_id[database$symbol_id == 3] <- 'SBUX'
database$symbol_id[database$symbol_id == 6] <- 'N.V'
database$symbol_id[database$symbol_id == 7] <- 'HMMJ.TO'
database$symbol_id[database$symbol_id == 9] <- 'APHA.TO'

sbux = database[database$symbol_id == 'SBUX', c('date', 'high')]
colnames(sbux) = c('date', 'sbux')
nv = database[database$symbol_id == 'N.V', c('date', 'high')]
colnames(nv) = c('date', 'nv')
hmmj = database[database$symbol_id == 'HMMJ.TO', c('date', 'high')]
colnames(hmmj) = c('date', 'hmmj')
apha = database[database$symbol_id == 'APHA.TO', c('date', 'high')]
colnames(apha) = c('date', 'apha')

merged_data = merge(sbux, merge(nv, merge(hmmj, apha, all = TRUE, by = 'date'), all = TRUE, by = 'date'), all = TRUE, by = 'date')
# merged_data = merge(sbux, nv, all = TRUE, by = 'date')
# pairs(merged_data[, -1])
round(cor(merged_data[,-1], use='pair'), 2)

