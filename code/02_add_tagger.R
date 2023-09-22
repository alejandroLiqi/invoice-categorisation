

# : -----------------------------------------------------------------------


# A. SETUP =================================================================================================

## Load packages: -----------------------------------------------------------------------
box::use(data.table[...])
box::use(jsonlite[fromJSON, toJSON])
box::use(reticulate[...])


## Select data: -----------------------------------------------------------------------

lid_company = 'LID_50_dimensionesanitariasrl'


## Load data: -----------------------------------------------------------------------

### Prepare dataset -----------
dts = jsonlite::fromJSON(jsonlite::fromJSON(file.path('data', 'output', lid_company, paste0(lid_company, '.json'))))
setDT(dts)

# dts = dts[1001:nrow(dts)]

### Company Parameters ------------
company = unique(dts$entity_name)
ateco = unique(dts$reciever_ateco_desc)
status = 'recieved'

### OpenAI keys ------------
openai_org = 'org-7BBQhy5LmDBwUMZl8RhYcWX8'
openai_key = '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'


# B. TAGGER =================================================================================================

## Init list: -----------------------------------------------------------------------

product_list_result = list()

for (i in 1:length(dts$description)) {
    
    invoice_id = dts$invoice_id[[i]]
    invoice_description = paste(dts$description[[i]]$descrizione, collapse = ", ")
    system_prompt = paste0(
        "You are assisting a company called ", company, " operating in the sector: ", ateco, ". ",
        "The company has ", status, " an Italian invoice. It contains many products.",
        "Your job is to extract a list of tags indicating the type of products you find in the invoice. This tags have to be a bit more abstract than just the name of the products.",
        "Product tags must be short. In Italian only.")[1]
    
    python_code = '
    
import openai
import json

openai.organization = "org-7BBQhy5LmDBwUMZl8RhYcWX8"
openai.api_key = "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"

functions = [{
    "name": "validate_tags",
    "description": "validate tags",
    "parameters": {
        "type": "object",
        "properties": {
            "tags": {
                "type": "array",
                "description": "list of tags",
                "items": {
                    "type": "string",
                    "description": "product name",
                }
            },
        },
        "required": ["product_list"]
    }
}]

messages = [{"role": "system", "content": r.system_prompt}] 
messages.append({"role": "user", "content": r.invoice_description})

resp = openai.ChatCompletion.create(
            model="gpt-3.5-turbo-0613",
            messages=messages,
            functions=functions,
            function_call={"name": "validate_tags"},
            max_tokens=500,
            n = 1,
            temperature=0,
)

n_tokens = resp["usage"]["total_tokens"]
product_list = json.loads(resp["choices"][0]["message"]["function_call"]["arguments"])
'

tryCatch({
    
    print(i)
    py_run_string(python_code)
    print(paste0(invoice_id, ' correctly tagged'))
    product_list_result[[as.character(invoice_id)]] = list(py$product_list)
    print(list(py$product_list))
    saveRDS(product_list_result, file.path('data', 'output', lid_company, "product_list_result_2.RDS"))
    
}, error = function(e) {
    
    cat("Error occurred at index", i, ": ", conditionMessage(e), "\n")
    product_list_result[[as.character(invoice_id)]] = 'No tags due to error with OpenAI'
    
})

}


# C. PREP EXPORT =================================================================================================

## Merge list with dt: -----------------------------------------------------------------------

result_list_str = sapply(product_list_result, function(x) toString(unlist(x)))

nrow(dts) - length(result_list_str)

dts = dts[as.character(invoice_id) %chin% as.character(names(result_list_str))]
dts[, tags := result_list_str]


## Export: -----------------------------------------------------------------------

saveRDS(dts, file.path('data', 'output', lid_company, paste0(lid_company, '_tagged.RDS')))


