function init() {
    $("#calcButton").prop("disabled", false);
}

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

    $("#padding").text("Skaičiuojama...");
    $("#output").hide();
    fetch(url, params)
        .then(resp => resp.json())
        .then(response => handleReponse(response))
        // .catch(_ => showStatus([text("Nepavyko susisiekti su serveriu.")]));
}

function handleReponse(response) {
    const limitLatex = "\\lim_{x \\to " + response.pointLatex + "} " + response.exprLatex;
    switch (response.result) {
        case "OK": {
            if (response.hasLimit) {
                showStatus([
                    latex(limitLatex + " = " + response.limitLatex)
                ]);
            } else {
                showStatus([
                    text("Riba neegzistuoja:"),
                    idented(latex(limitLatex))
                ]);
            }
            break;
        }
        case "FunctionParseError": {
            showStatus([
                text("Neteisingai įvesta funkcija:"),
                idented(text("TODO"))
            ]);
            break;
        }
        case "PointParseError": {
            showStatus([
                text("Neteisingai įvestas taškas:"),
                idented(text("TODO"))
            ]);
            break;
        }
        case "UnknownLimit": {
            showStatus([
                text("Nepavyko išanalizuoti ribos:"),
                idented(latex(limitLatex))
            ]);
            break;
        }
        case "RanOutOfFuel": {
            showStatus([
                text("Nepavyko išanalizuoti ribos (baigėsi kuras):"),
                idented(latex(limitLatex))
            ]);
            break;
        }
        case "FunctionUndefined": {
            showStatus([
                text("Taškas"),
                idented(latex(response.pointLatex)),
                text("nėra funkcijos"),
                idented(latex(response.exprLatex)),
                text("ribinis taškas.")
            ]);
            break;
        }
        default: {
            showStatus([
                text("Vidinė klaida: nežinomas statusas (" + response.result + ")")
            ]);
            break;
        }
    }
}

function setResult(output, response) {
    const outputDiv = document.getElementById("output");
    const paddingDiv = document.getElementById("padding");
    paddingDiv.innerHTML = "Skaičiuojama...";
    outputDiv.style.display = "none";
    document.getElementById("output").innerHTML = output;
    MathJax.Hub.Queue(["Typeset",MathJax.Hub,"output"]);
    setTimeout(function() {
        outputDiv.style.display = "block";
        paddingDiv.innerHTML = "";
    }, 500);
    // document.getElementById("error").innerHTML = response.errorMessage ? "Iš sistemos gautas išsamesnis klaidos pranešimas: <br> <br>" + response.errorMessage : "";
}

function round(numberString) {
    const precision = 8;
    let factor = Math.pow(10, precision);
    return Math.round(parseFloat(numberString) * factor) / factor;
}

function text(str) {
    return {
        elem: $("<p>").text(str),
        hasLatex: false,
    };
}

function idented(thing) {
    thing.elem.addClass("identedBlock");
    return thing;
}

function latex(str) {
    return {
        elem: $("<p>").text("$$" + str + "$$"),
        hasLatex: true,
    };
}

function showStatus(items) {
    console.log('showing status');
    console.log(items);
    const outputDiv = $("#output");
    const paddingDiv = $("#padding");
    outputDiv.empty();
    var hasLatex = false;
    for (var i = 0; i < items.length; i++) {
        outputDiv.append(items[i].elem);
        if (items[i].hasLatex) {
            var id = 'contentElem' + i;
            items[i].elem.attr('id', id);
            MathJax.Hub.Queue(["Typeset",MathJax.Hub,id]);
            hasLatex = true;
        }
    }
    if (hasLatex) {
        setTimeout(function() {
            outputDiv.show();
            paddingDiv.text('');
        }, 500);
    } else {
        paddingDiv.text('');
        outputDiv.show();
    }
}
