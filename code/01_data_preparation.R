

# : -----------------------------------------------------------------------


# SETUP =================================================================================================

## Load packages: -----------------------------------------------------------------------
box::use(data.table[...])
box::use(DBI[dbGetQuery, dbConnect, dbDisconnect])
box::use(RSQLite[SQLite])
box::use(jsonlite[write_json, fromJSON, toJSON])

source(file.path('code', 'functions.R'))


## Paths: -----------------------------------------------------------------------

lid_company = 'LID_50_dimensionesanitariasrl'

base_pathr = file.path(getwd())
clients_pathr = file.path(base_pathr, 'data', 'clients', lid_company)
clients_pathr = latest_date(clients_pathr)


# PREPARATION =================================================================================================

# unique(dt_cat_base$CATEGORIA_fornitore)


## Data retrieval: ---------------------------

dt_inv = fread(file.path(clients_pathr, 'internal', 'ricevute.csv'))
# dt_inv_pag = fread(file.path(clients_pathr, lid_company, '2023-07-28', 'internal', 'ricevute_pag.csv'))
dt_inv_beni = fread(file.path(clients_pathr, 'internal', 'ricevute_beni.csv'))



## Cleaning: ---------------------------
df_recieved = dt_inv[, .(invoice_id, entity_id, entity_name, entity_vat = cf_cliente, sender_vat = cf_emittente, sender_name = nome_emittente, type = tipo_docum, date, amount_net, amount_vat, sender_prov = cedente_sede_provincia)]
df_recieved_desc = dt_inv_beni[, .(invoice_id, descrizione, num_linea, quantity, unita_misura, prezzo_unitario, prezzo_totale, aliquota_iva)]

rm(dt_inv, dt_inv_beni)



### Feature Engineering --------

df_recieved[, year_inv := year(date)]

# Frequency Number: How many invoices from this sender?
df_recieved[, n_inv := .N, by = list(sender_vat, year_inv)]
df_recieved[, n_inv_perc := n_inv / .N, by = list(year_inv)]


# Size Distribution: how do invoices size change?
hexiles = quantile(df_recieved$amount_net, probs = seq(0, 1, by = 1/6))
df_recieved[, hexiles := hexile(amount_net, hexiles), by = list(sender_vat)]
df_recieved[, hexiles := mean(amount_net), by = list(hexiles)]
# plot(unique(df_recieved[order(hexiles)]$hexiles))

# INVOICE - Size VAT % :
df_recieved[, vat_perc := amount_vat / amount_net]

# COMPANY & INVOICE - Size Share: whats the size of the invoice in the year?
df_recieved[, amount_perc_y := amount_net / sum(amount_net, na.rm = TRUE), by = list(year_inv)]


## Nested description and meta data: ---------------------------
df_result = merge(df_recieved, df_recieved_desc, by = "invoice_id", all.x = TRUE)
dts_recieved = df_result[, list(description = list(embedded = data.table(invoice_id, descrizione, num_linea, quantity, unita_misura, prezzo_unitario, prezzo_totale, aliquota_iva))), by = 'invoice_id']

df_recieved = df_recieved[dts_recieved, on = "invoice_id"]

rm(df_result, df_recieved_desc, dts_recieved)



## Counterpart info: ---------------------------
conn = DBI::dbConnect(RSQLite::SQLite(), file.path('data', 'italian_companies.db'))

vec_suppliers = unique(df_recieved[, .(sender_name, sender_vat)])

base = "SELECT * FROM landscape"
search_parameters = list("vat" = vec_suppliers$sender_vat)
dts_ateco_suppliers = assembleQuery(conn, base, search_parameters)

dts_ateco_suppliers |> setDT()
dts_ateco_suppliers = dts_ateco_suppliers[, .(sender_name_db = entity_name, sender_vat = vat, sender_ateco_m = ateco_m, sender_ateco_desc = ateco_m_desc, sender_ateco = ateco_code, sender_employees_group = employees_group, sender_region = geo_region, geo_region)]


## company info
vec_company = unique(as.character(df_recieved[!is.na(entity_vat)]$entity_vat))
vec_company = sprintf("%011s", vec_company)
search_parameters = list("vat" = vec_company)

company_ateco = assembleQuery(conn, base, search_parameters)
company_ateco |> setDT()
company_ateco_desc = company_ateco$ateco_desc
company_ateco = company_ateco$ateco_code

DBI::dbDisconnect(conn)

df_recieved = merge(df_recieved, dts_ateco_suppliers, by = 'sender_vat', all.x = TRUE)
# df_recieved = df_recieved[dts_ateco_suppliers, on = "sender_vat"]

if (substr(company_ateco, 1, 1) == "0") { company_ateco <- substr(company_ateco, 2, nchar(company_ateco))} else {company_ateco = company_ateco}

df_recieved$reciever_ateco = company_ateco

df_recieved[, sender_ateco := ifelse(substr(sender_ateco, 1, 1) == "0",
                         substr(sender_ateco, 2, nchar(sender_ateco)),
                         sender_ateco)]


df_recieved[, pair_ateco := paste(sender_ateco, reciever_ateco, sep = '-')]
df_recieved[, reciever_ateco_desc := company_ateco_desc]


rm(vec_suppliers, vec_company, dts_ateco_suppliers, search_parameters, conn)



## I/O Prob, Iteration 1: ---------------------------

dt_cat_base = fread(file.path(getwd(), 'data', 'categorizzatore_fatture_v0.csv'))

dt_cat_base[, Codice_ateco_fornitore := gsub("\\.", "", Codice_ateco_fornitore)]
dt_cat_base[, Codice_ateco := gsub("\\.", "", Codice_ateco)]
dt_cat_base = dt_cat_base[, .(reciever_ateco = Codice_ateco_fornitore, reciever_supplychain = FILIERA_fornitore, sender_ateco = Codice_ateco, sender_supplychain = FILIERA, category = CATEGORIA, prob)]

dt_cat_base[, pair_ateco := paste(sender_ateco, reciever_ateco, sep = '-')]

df_recieved = merge(df_recieved, dt_cat_base, by = c('pair_ateco', 'sender_ateco', 'reciever_ateco'), all.x = TRUE)

## I/O Prob, Iteration 2: ---------------------------

### Divide 
df_recieved_merged = df_recieved[!is.na(sender_supplychain)]
df_recieved_notmerged = df_recieved[is.na(sender_supplychain) & !is.na(sender_ateco)]
df_recieved_cantmerged = df_recieved[is.na(sender_ateco)]

nrow(df_recieved) == nrow(df_recieved_merged) + nrow(df_recieved_notmerged) + nrow(df_recieved_cantmerged)


df_recieved_notmerged[, sender_ateco := paste0('0', sender_ateco)]
df_recieved_notmerged[, reciever_supplychain := NULL]
df_recieved_notmerged[, sender_supplychain := NULL]
df_recieved_notmerged[, category := NULL]
df_recieved_notmerged[, prob := NULL]
df_recieved_notmerged[, pair_ateco := NULL]
df_recieved_notmerged[, pair_ateco := paste(sender_ateco, reciever_ateco, sep = '-')]

df_recieved_notmerged = merge(df_recieved_notmerged, dt_cat_base, by = c('pair_ateco', 'sender_ateco', 'reciever_ateco'), all.x = TRUE)

df_recieved = rbindlist(list(df_recieved_merged, df_recieved_notmerged, df_recieved_cantmerged))

rm(df_recieved_merged, df_recieved_notmerged, df_recieved_cantmerged, dt_cat_base)
gc()



# EXPORT =================================================================================================

df_recieved = df_recieved[, .(invoice_id, type, year_inv, date, amount_net, amount_vat, description, entity_id, entity_name, entity_vat, sender_name, sender_vat, pair_ateco, category, sender_supplychain, n_inv, n_inv_perc, hexiles, vat_perc, amount_perc_y, sender_employees_group, sender_region, sender_prov, sender_ateco, sender_ateco_desc, reciever_ateco, reciever_ateco_desc, prob)]

dir.create(file.path('data', 'output', lid_company))

jsonlite::write_json(jsonlite::toJSON(df_recieved, pretty = TRUE), path = file.path('data', 'output', lid_company, paste0(lid_company, '.json')))

# dtw = jsonlite::fromJSON(jsonlite::fromJSON(paste0(lid_company, '.json')))

