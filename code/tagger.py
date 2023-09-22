import openai

openai.organization = "org-7BBQhy5LmDBwUMZl8RhYcWX8"
openai.api_key = "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"

company = "dimensione sanitaria"
ateco = "farmacia"
received_issued = "received"


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
        'required': ["product_list"]
    }
}
]



system_prompt = f"""
You are assisting a company called "{company}" operating in the sector: {ateco}. 
The company has {received_issued} an Italian invoice. It contains many products.
Your job is to extract a list of tags indicating the type of products you find in the invoice.
Product tags must be short. In Italian only.
"""

### Codice per aggregare descrizioni relative a diverse fatture presenti su diverse linee

# ricevute_beni = pd.read_csv(BASE_PATH + "ricevute_beni.csv")
# df_descrizioni = (
#     ricevute_beni
#     .assign(descrizione=ricevute_beni.descrizione.apply(str))
#     .groupby(["id", "invoice_id"])  
#     .descrizione
#     .agg(lambda x: " ".join(x))
#     .reset_index()
# )


description = "CERAVE Crema Idra177ml (componente) CVE ACNE CLEANSER 8OZ CVE AHA BHA SERUM 40ML CRV F.MOIS SPF30 1.75o deterg idratant 473ml CVE SA Smooth Cream 12"

messages = [{"role": "system", "content": system_prompt}] 
messages.append({"role": "user", "content": description})

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
#print(resp)
product_list = json.loads(resp["choices"][0]["message"]["function_call"]["arguments"])
