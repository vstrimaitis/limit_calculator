function init() {
    $("#calcButton").prop("disabled", false);
}

function calculate() {
    const url = "/api/limits"; 

    let functionQuery = document.getElementById("function").value;
    let pointQuery = document.getElementById("xTo").value;

    let data = {
        function: functionQuery,
        point: pointQuery
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
        .catch(_ => {
            showStatus([text("Nepavyko susisiekti su serveriu.")]);
            return null;
        })
        .then(response => handleReponse(response, functionQuery, pointQuery));
}

function handleReponse(response, exprText, pointText) {
    if (response === null) {
        return;
    }
    const limitLatex = "\\lim_{x \\to " + response.pointLatex + "} " + response.exprLatex;
    switch (response.result) {
        case "OK": {
            if (response.hasLimit) {
                showStatus([
                    latex(limitLatex + " = " + limitToLatex(response.limit))
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
                idented(markedCode(exprText, response.errorLocation))
                // text(response.errorMessage)
            ]);
            break;
        }
        case "PointParseError": {
            showStatus([
                text("Neteisingai įvestas taškas:"),
                idented(markedCode(pointText, response.errorLocation))
                // text(response.errorMessage)
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
                text("Taškas $$" + response.pointLatex + "$$ nėra funkcijos $$" + response.exprLatex + "$$ ribinis taškas.", true)
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

function limitToLatex(limit) {
    if (limit === "+inf") {
        return "+\\infty";
    } else if (limit === "-inf") {
        return "-\\infty";
    } else {
        return round(limit);
    }
}

function round(numberString) {
    const precision = 8;
    let factor = Math.pow(10, precision);
    return Math.round(parseFloat(numberString) * factor) / factor;
}

function text(str, withLatex) {
    var elem = $("<p>").text(str);
    if (withLatex) {
        elem.addClass('inlineLatex');
    }
    return {
        elem: elem,
        hasLatex: withLatex || false
    };
}

function markedCode(str, pos) {
    pos -= 1;
    let before = $("<span>").text(str.substring(0, pos));
    let markedText = str.substring(pos, pos + 1).trim();
    if (markedText.length == 0) {
        markedText = "\xa0";
    }
    let marked = $("<span>").addClass("marked").text(markedText);
    let after = $("<span>").text(str.substring(pos + 1));
    return {
        elem: $("<p>")
            .addClass("codeBlock")
            .append(before)
            .append(marked)
            .append(after),
        hasLatex: false
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
