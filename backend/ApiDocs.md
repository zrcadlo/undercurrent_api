## Undercurrent API

For an up-to-date version of this documentation, visit the `/docs` endpoint of the API.For all `JWT` protected endpoints, you must provide it in the `Authorization` header, with a value of `Bearer THE_TOKEN`(where `THE_TOKEN` is what's returned in the `token` property after logging in or creating a user.)

## POST /api/login

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"email":"charlie@alpaca.net","password":"somePassword"}
```

### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"token":"some-long-token","user":{"email":"nena@alpaca.com","birthday":null,"gender":"Female","name":"Nena Alpaca","birthplace":"Tokyo, Japan"}}
```

## GET /api/user

### Authentication

This part of the API is protected by JSON Web Tokens ([JWTs](https://en.wikipedia.org/wiki/JSON_Web_Token))


Clients must supply the following data
A JWT Token signed with this server's key


### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"email":"nena@alpaca.com","birthday":null,"gender":"Female","name":"Nena Alpaca","birthplace":"Tokyo, Japan"}
```

## PUT /api/user

### Authentication

This part of the API is protected by JSON Web Tokens ([JWTs](https://en.wikipedia.org/wiki/JSON_Web_Token))


Clients must supply the following data
A JWT Token signed with this server's key


### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"email":"new.email@alpaca.net","birthday":null,"gender":"NonBinary","name":"New Alpaca Name","birthplace":null}
```

### Response:

- Status code 204
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript

```

## GET /api/user/dreams

### Authentication

This part of the API is protected by JSON Web Tokens ([JWTs](https://en.wikipedia.org/wiki/JSON_Web_Token))


Clients must supply the following data
A JWT Token signed with this server's key


### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[]
```

- A dream with the dream id and dreamer id included (`application/json;charset=utf-8`, `application/json`):

```javascript
[{"nightmare":false,"lucid":false,"private":false,"emotions":["joy","intimidated"],"recurring":true,"date":"2020-07-07T00:00:00Z","starred":true,"dreamer_id":42,"dream_id":42,"title":"I dreamed of our alpacas","description":"Some alpacas were wearing sunglasses"}]
```

- A dream with the dream id and dreamer id included, A dream with the dream id and dreamer id included (`application/json;charset=utf-8`):

```javascript
[{"nightmare":false,"lucid":false,"private":false,"emotions":["joy","intimidated"],"recurring":true,"date":"2020-07-07T00:00:00Z","starred":true,"dreamer_id":42,"dream_id":42,"title":"I dreamed of our alpacas","description":"Some alpacas were wearing sunglasses"},{"nightmare":false,"lucid":false,"private":false,"emotions":["joy","intimidated"],"recurring":true,"date":"2020-07-07T00:00:00Z","starred":true,"dreamer_id":42,"dream_id":42,"title":"I dreamed of our alpacas","description":"Some alpacas were wearing sunglasses"}]
```

## POST /api/user/dreams

### Authentication

This part of the API is protected by JSON Web Tokens ([JWTs](https://en.wikipedia.org/wiki/JSON_Web_Token))


Clients must supply the following data
A JWT Token signed with this server's key


### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"nightmare":false,"lucid":false,"private":false,"emotions":["joy","intimidated"],"recurring":true,"date":"2020-07-07T00:00:00Z","starred":true,"title":"I dream of Alpacas","description":"Some alpacas were wearing sunglasses"}
```

### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- A dream with the dream id and dreamer id included (`application/json;charset=utf-8`, `application/json`):

```javascript
{"nightmare":false,"lucid":false,"private":false,"emotions":["joy","intimidated"],"recurring":true,"date":"2020-07-07T00:00:00Z","starred":true,"dreamer_id":42,"dream_id":42,"title":"I dreamed of our alpacas","description":"Some alpacas were wearing sunglasses"}
```

## DELETE /api/user/dreams/:dreamId

### Authentication

This part of the API is protected by JSON Web Tokens ([JWTs](https://en.wikipedia.org/wiki/JSON_Web_Token))


Clients must supply the following data
A JWT Token signed with this server's key


### Captures:

- *dreamId*: ID of the dream to update, as returned when creating it.

### Response:

- Status code 204
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript

```

## PUT /api/user/dreams/:dreamId

### Authentication

This part of the API is protected by JSON Web Tokens ([JWTs](https://en.wikipedia.org/wiki/JSON_Web_Token))


Clients must supply the following data
A JWT Token signed with this server's key


### Captures:

- *dreamId*: ID of the dream to update, as returned when creating it.

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- All fields are optional; if emotions are sent, they will replace current ones. (`application/json;charset=utf-8`, `application/json`):

```javascript
{"nightmare":false,"lucid":true,"private":true,"emotions":["acceptance"],"recurring":false,"date":null,"starred":false,"title":"I dreamed a dream","description":null}
```

### Response:

- Status code 204
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript

```

## PUT /api/user/password

### Authentication

This part of the API is protected by JSON Web Tokens ([JWTs](https://en.wikipedia.org/wiki/JSON_Web_Token))


Clients must supply the following data
A JWT Token signed with this server's key


### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"newPassword":"anotherPassword","currentPassword":"sample"}
```

### Response:

- Status code 204
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript

```

## POST /api/users

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"email":"paco@alpaca.net","birthday":"2017-02-14T00:00:00Z","gender":"Male","name":"Paco Alpaco","password":"somePassword","birthplace":"Shenzhen, China"}
```

### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"token":"some-long-token","user":{"email":"nena@alpaca.com","birthday":null,"gender":"Female","name":"Nena Alpaca","birthplace":"Tokyo, Japan"}}
```

## GET /api/users/:userId/dreams

### Authentication

This part of the API is protected by JSON Web Tokens ([JWTs](https://en.wikipedia.org/wiki/JSON_Web_Token))


Clients must supply the following data
A JWT Token signed with this server's key


### Captures:

- *userId*: ID of the user to inspect, as returned when creating it.

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[]
```

- A dream with the dream id and dreamer id included (`application/json;charset=utf-8`, `application/json`):

```javascript
[{"nightmare":false,"lucid":false,"private":false,"emotions":["joy","intimidated"],"recurring":true,"date":"2020-07-07T00:00:00Z","starred":true,"dreamer_id":42,"dream_id":42,"title":"I dreamed of our alpacas","description":"Some alpacas were wearing sunglasses"}]
```

- A dream with the dream id and dreamer id included, A dream with the dream id and dreamer id included (`application/json;charset=utf-8`):

```javascript
[{"nightmare":false,"lucid":false,"private":false,"emotions":["joy","intimidated"],"recurring":true,"date":"2020-07-07T00:00:00Z","starred":true,"dreamer_id":42,"dream_id":42,"title":"I dreamed of our alpacas","description":"Some alpacas were wearing sunglasses"},{"nightmare":false,"lucid":false,"private":false,"emotions":["joy","intimidated"],"recurring":true,"date":"2020-07-07T00:00:00Z","starred":true,"dreamer_id":42,"dream_id":42,"title":"I dreamed of our alpacas","description":"Some alpacas were wearing sunglasses"}]
```

## GET /docs

### Response:

- Status code 200
- Headers: []

- No response body

