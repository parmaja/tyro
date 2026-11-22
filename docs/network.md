# Network

[<< Tyro docs index](README.md)

Scripts can talk to web APIs with `http_get`/`http_post` and read the answer with
`json_decode`. The request runs on the script thread, so the render loop keeps
drawing while the call is in flight; the script itself blocks until the answer
(or the failure) arrives.

| Function | Description |
|----------|-------------|
| `http_get(url)` | Fetch a URL over HTTP/HTTPS; returns the body, or `nil` plus a message |
| `http_post(url, body [, content_type])` | POST a body to a URL and return the answer, or `nil` plus a message. A table body is encoded with `json_encode` and sent as `application/json`; a string body goes out as-is (`text/plain` by default). The third argument overrides the `Content-Type` |
| `json_decode(text)` | Parse a JSON document into the matching Lua value: object→table (keyed by member name), array→table (1..n), number, boolean, `null`→`nil`; returns `nil` plus the parser message on invalid input |
| `json_encode(value)` | Turn a Lua value into JSON text: a 1..n sequence becomes an array, any other table an object keyed by its member names, `nil` becomes `null`. Functions and userdata are skipped inside a table and refused at the top level, and a table that holds itself answers `nil` plus a message |

```lua
local body, err = http_get("https://ipapi.co/json/")   -- IP-based geolocation
if body then
  local geo = json_decode(body)
  println("you are in " .. geo.city .. ", " .. geo.country_code)
end

-- a POST with a table body: json_encode does the packing
local answer = http_post("https://api.example.com/scores", { name = "ana", score = 4200 })
```

`json_decode` answers `nil` plus a message for a bare top-level number or
boolean (`"42"`, `"false"`) — the underlying parser reads objects, arrays and
strings. See `games/weather/weather.tyro` for a full app built on both
functions, `tests/test_http_json.tyro` and `tests/test_json_http.tyro` for
runnable checks (the live POST skips itself when the network is dark).

