function calculate() {
    const url = "/api/limits"; 

    let data = {
        function: document.getElementById("function").value,
        point: document.getElementById("xTo").value
    };

    let params = {
        method: 'POST',
        body: JSON.stringify(data),
        headers: new Headers({
            "Content-Type": "application/json"
        })
    };

    fetch(url, params)
        .then(resp => resp.json())
        .then(response => handleReponse(response));
}

function handleReponse(response) {
    switch (response.result) {
        case "OK": {
            if (response.hasLimit) {
                output = "$$" + response.latex + " = " + getLimitValue(response) + "$$";
            } else {
                output = "Riba $$" + response.latex + "$$ neegzistuoja.";
            }
            break;
        }
        case "FunctionParseError": {
            output = "Neteisingai įvesta funkcija.";
            break;
        }
        case "PointParseError": {
            output = "Neteisingai įvestas taškas.";
            break;
        }
        case "UnknownLimit": {
            // output = "Klaida: Nepavyko išanalizuoti ribos.";
            output = "Nepavyko išanalizuoti $$" + response.latex + "$$";
            break;
        }
        case "RanOutOfFuel": {
            // output = "Klaida: Baigėsi kuras.";
            output = "Nepavyko išanalizuoti $$" + response.latex + "$$ (baigėsi kuras)";
            break;
        }
        case "UnsupportedOperation": {
            output = "Klaida: Nepalaikoma operacija.";
            break;
        }
        case "FunctionUndefined": {
            output = "Klaida: Egzistuoja taško aplinka, kurioje funkcija neapibrėžta.";
            break;
        }
        default: {
            output = "Nežinomas statusas: " + response.result;
            break;
        }
    }
    setResult(output, response);
}

function setResult(output, response) {
    const outputDiv = document.getElementById("output");
    const paddingDiv = document.getElementById("padding");
    paddingDiv.innerHTML = "Skaičiuojama..."
    outputDiv.style.display = "none";
    document.getElementById("output").innerHTML = output;
    MathJax.Hub.Queue(["Typeset",MathJax.Hub,"output"]);
    setTimeout(function() {
        outputDiv.style.display = "block";
        paddingDiv.innerHTML = "";
    }, 500);
    // document.getElementById("error").innerHTML = response.errorMessage ? "Iš sistemos gautas išsamesnis klaidos pranešimas: <br> <br>" + response.errorMessage : "";
}

function getLimitValue(response) {
    if (response.hasLimit === true) {
        if (response.limit == "+inf") {
            return "+\\infty";
        } else if (response.limit == "+inf") {
            return "-\\infty";
        } else {
            return round(response.limit);
        }
    } else {
        return "Riba neegzistuoja";
    }
}

function round(numberString) {
    const precision = 8;
    let factor = Math.pow(10, precision);
    return Math.round(parseFloat(numberString) * factor) / factor;
}