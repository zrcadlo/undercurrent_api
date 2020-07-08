## Undercurrent API

For an up-to-date version of this documentation, visit the `/docs` endpoint of the API.

For all `JWT` protected endpoints, you must provide it in the `Authorization` header, with a value of `Bearer THE_TOKEN` 
(where `THE_TOKEN` is what's returned in the `token` property after logging in or creating a user.)

## GET /

### Response:

- Status code 200
- Headers: []

- No response body

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