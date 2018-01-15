function calculate(){
    const url = "http://www.riboja.me/api/limits"; 

    let data = {
        function: document.getElementById("function").value,
        point: document.getElementById("xTo").value
    }

    let params = {
        method: 'POST',
        body: JSON.stringify(data),
        headers: new Headers({
            "Content-Type": "applicication/json"
        })
    }

    fetch(url, params)
    .then(resp => resp.json())
    .then(response =>handleReponse(response))
}

function handleReponse(response){
    switch(response.result) {
        case "OK":{
            output = getLimitValue(response);
            break;
        }
        case "FunctionParseError": {
            output = "Klaida: Neteisingai įvesta funkcija.";
            break;
        }
        case "PointParseError": {
            output = "Klaida: Neteisingai įvestas taškas.";
            break;
        }
        case "UnknownLimit": {
            output = "Klaida: Nepavyko išanazlizuoti ribos.";
            break;
        }
        case "RanOutOfFuel": {
            output = "Klaida: Baigėsi kuras.";
            break;
        }
        case "UnsupportedOperation": {
            output = "Klaida: Nepalaikoma operacija.";
            break;
        }
        case "FunctionUndefined":  {
            output = "Klaida: Egzistuoja taško aplinka, kurioje funkcija neapibrėžta.";
            break;
        }
        default: {
            output = "TODO!!!!!!! SOMETHING WAS NOT CAUGHT!";
            break;
        }
    }
    setResult(output, response);
}

function setResult(output, response){
    document.getElementById("output").innerHTML = output;
    document.getElementById("error").innerHTML =  response.errorMessage ? "Iš sistemos gautas išsamesnis klaidos pranešimas: <br> <br>" + response.errorMessage : "";
}

function getLimitValue(response){
    if (response.hasLimit == true) {
        if(response.limit == "+inf"){
            return  "+∞";
        }else if(response.limit == "+inf"){
            return "-∞";
        }else{
            return round(response.limit);
        }
    } else{
        return "Riba neegzistuoja";
    }
}

function round(numberString) {
    const precision = 8;
    let factor = Math.pow(10, precision);
    return Math.round(parseFloat(numberString)*factor)/factor;
}