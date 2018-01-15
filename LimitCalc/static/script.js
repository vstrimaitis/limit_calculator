function calculate(){
    const goesToField = document.getElementById("xTo");
    let goesTo = goesToField.value;

    const funcField = document.getElementById("function");
    let func = funcField.value;

    const outputField = document.getElementById("output");
    let output = "NOT YET CALCULATED.";

    const errorField = document.getElementById("error");
    let error = "";

    const url = "http://www.riboja.me/api/limits"; 

    let data = {
        function: func,
        point: goesTo
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
    .then(response =>{
        if(response.result == "OK"){
            if(response.hasLimit == true){
                if(response.limit == "+inf"){
                    output = "+∞";
                }else if(response.limit == "+inf"){
                    output = "-∞";
                }else{
                    output = round(response.limit);
                }
            }else{
                output = "Riba neegzistuoja";
            }
        }else if(response.result == "FunctionParseError"){
            output = "Klaida: Neteisingai įvesta funkcija.";
            error = response.errorMessage;
        }else if(response.result == "PointParseError"){
            output = "Klaida: Neteisingai įvestas taškas.";
            error = response.errorMessage;
        }else if(response.result == "UnknownLimit"){
            output = "Klaida: Nepavyko išanazlizuoti ribos.";
        }else if(response.result == "RanOutOfFuel"){
            output = "Klaida: Baigėsi kuras.";
        }else if(response.result == "UnsupportedOperation"){
            output = "Klaida: Nepalaikoma operacija.";
            error = response.errorMessage;
        }else if(response.result == "FunctionUndefined"){
            output = "Klaida: Egzistuoja taško aplinka, kurioje funkcija neapibrėžta.";
        }
        
        if(output == "NOT YET CALCULATED."){
            let output = "TODO!!!!!!! SOMETHING WAS NOT CAUGHT!";
        }

        outputField.innerHTML = output;
        if(response.errorMessage != undefined){
            errorField.innerHTML = "Iš sistemos gautas išsamesnis klaidos pranešimas: <br> <br>" + error;
        }else{
            errorField.innerHTML = "";
        }
    })

}


function round(numberString) {
    const precision = 8;
    let factor = Math.pow(10, precision);
    return Math.round(parseFloat(numberString)*factor)/factor;
}