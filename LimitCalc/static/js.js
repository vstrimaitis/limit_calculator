function calculate(){
    const goesToField = document.getElementById("xGoesTo");
    let goesTo = goesToField.value;

    const funcField = document.getElementById("function");
    let func = funcField.value;

    const outputField = document.getElementById("output");
    let output;

    const url = "http://www.riboja.me/"; 

    let data = {
        function: goesTo,
        point: func
    }

    let params = { //add whats missing
        method: 'GET',
        body: JSON.stringify(data),
        headers: new Headers()
    }

    let answer;

    fetch(url, params)
    .then((resp) => resp.json())
    .then(function(response){ // parse the last version of response json format
        //console.log(response);
        answer = "TODO";


        outputField.innerHTML = answer;
    })
    .catch(function(error){ // print error if there is one 
        //console.log(error);
        answer = "TODO";


        outputField.innerHTML = answer;
        // make input func error letter color red
    })

}