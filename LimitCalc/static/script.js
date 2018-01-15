function calculate(){
    const goesToField = document.getElementById("xTo");
    let goesTo = goesToField.value;

    const funcField = document.getElementById("function");
    let func = funcField.value;

    const outputField = document.getElementById("output");
    let output;

    const url = "http://www.riboja.me/api/limits"; 

    let data = {
        function: func,
        point: goesTo
    }

    let params = { //add whats missing
        method: 'POST',
        body: JSON.stringify(data),
        headers: new Headers({
            "Content-Type": "applicication/json"
        })
    }

    fetch(url, params)
    .then(resp => resp.json())
    .then(response =>{ // parse the last version of response json format
        console.log(response);
        console.log(response.result == "FunctionParseError");
        console.log(response.errorMessage);
        let answer = "NOT YET CALCULATED.";
        if(response.result == "OK"){
            if(response.hasLimit == true){
                if(response.limit == "+inf"){
                    answer = "+∞";
                }else if(response.limit == "+inf"){
                    answer = "-∞";
                }else{
                    answer = response.limit;
                }
            }else{
                answer = "Riba neegzistuoja";
            }
        }else if(response.result == "FunctionParseError"){
            answer = response.errorMessage;
        }else if(response.result == "PointParseError"){
            answer = response.errorMessage;
        }else if(response.result == "UnknownLimit"){
            answer = response.errorMessage;
        }else if(response.result == "RanOutOfFuel"){
            answer = response.errorMessage;
        }else if(response.result == "UnsupportedOperation"){
            answer = response.errorMessage;
        }else if(response.result == "FunctionUndefined"){
            answer = response.errorMessage;
        }
        
        if(answer == "NOT YET CALCULATED."){
            let answer = "TODO!!!!!!! SOMETHING WAS NOT CAUGHT!";
        }


        outputField.innerHTML = answer;
    })
    /*.catch(function(error){ // print error if there is one 
        //console.log(error);
        answer = "TODO";


        outputField.innerHTML = answer;
        // make input func error letter color red
    })*/

}