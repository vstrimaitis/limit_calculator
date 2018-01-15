# LimitCalc

## Building

```
stack build
```

## Running tests

```
stack test :limits-basic
stack test :limits-unstable
stack test :limits
...
```

## Web server

The web server can be found at `riboja.me` (IP `165.227.134.200`).

To launch the service on port 80, run the following command:

```
stack exec LimitServer 80 &
```

This will run the limit server as a seperate process without blocking the terminal.

To check the PID of the process, run the `ps` command. To kill the process (if needed), run

```
kill -9 <pid>
```

## Web API

`GET /` serves the main HTML page for interacting with the page.

`POST /api/limits` invokes the actual calculation of a limit. The body of the request should be of this format:

```
{
    "function": "string",
    "point": "string",
    "isPrecise?": boolean
}
```

| Field       | Meaning                                                                             |
|-------------|-------------------------------------------------------------------------------------|
| `function`  | The string representation of the function which will be used to calculate the limit |
| `point`     | The point at which to calculate the limit                                           |
| `isPrecise` | An optional field to enable precise calculations. Default value if `false`          |

The response data structure is as follows:

```
{
    "result": "string",
    "errorMessage?": "string",
    "errorLocation?": number,
    "hasLimit?": boolean,
    "limit?": "string"
}
```

| Field           | Meaning                                                                                                                                                                                                                              |
|-----------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `result`        | An enum representing the result of the calculation. If everything went fine, the value will be `OK`. Otherwise the possible values are `FunctionParseError`, `PointParseError`, `UnknownLimit`, `OutOfFuel`, `UnsupportedOperation`. |
| `errorMessage`  | If result is not `OK`, this field might contain additional information about the error which occurred.                                                                                                                               |
| `errorLocation` | In case of a parse error, this field will contain a single integer - the position in the input where the parse failed.                                                                                                               |
| `hasLimit`      | If the result is `OK`, this field will tell, whether the limit of the given function at the given point exists or not.                                                                                                               |
| `limit`         | If the limit exists, this field will contain the actual value. If the limit is negative or positive infinity, the value of this field will be `-inf` or `+inf`, accordingly.                                                         |
