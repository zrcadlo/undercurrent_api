## Undercurrent API

For an up-to-date version of this documentation, visit the `/docs` endpoint of the API.For all `JWT` protected endpoints, you must provide it in the `Authorization` header, with a value of `Bearer THE_TOKEN`(where `THE_TOKEN` is what's returned in the `token` property after logging in or creating a user.)

## GET /api/dreams

### Authentication

This part of the API is protected by JSON Web Tokens ([JWTs](https://en.wikipedia.org/wiki/JSON_Web_Token))


Clients must supply the following data
A JWT Token signed with this server's key


### GET Parameters:

- mine
     - **Description**: If specified, will only search the current user's dreams. Because this is a flag, you can call it like this: /api/dreams?mine
     - This parameter is a **flag**. This means no value is expected to be associated to this parameter.

- username
     - **Values**: *nena.alpaca*
     - **Description**: A username. Checks existence. If you provide your own, we'll search private dreams too. If none is provide, search all public dreams.

- city
     - **Values**: *Queens*
     - **Description**: Filter by city (if not provided, won't affect the filtering.)

- region
     - **Values**: *New York*
     - **Description**: Filter by administrative region (state or province) (if not provided, won't affect the filtering.)

- country
     - **Values**: *USA*
     - **Description**: Filter by country name (if not provided, won't affect the filtering.)

- gender
     - **Values**: *male, female, nonBinary*
     - **Description**: Filter by dreamer's gender (if not provided, won't affect the filtering.)

- zodiac_sign
     - **Values**: *capricorn, sagittarius, ...*
     - **Description**: Filter by dreamer's zodiac sign (if not provided, won't affect the filtering.)

- lucid
     - **Values**: *true, false*
     - **Description**: Filter by: is lucid or not  (if not provided, won't affect the filtering.)

- nightmare
     - **Values**: *true, false*
     - **Description**: Filter by: is nightmare or not (if not provided, won't affect the filtering.)

- recurring
     - **Values**: *true, false*
     - **Description**: Filter by: is recurring or not (if not provided, won't affect the filtering.)

- emotions
     - **Values**: *joy*
     - **Description**: Filter by emotions: requires a list, will return dreams that have all the given emotions (if not provided, won't affect the filtering.)
     - This parameter is a **list**. All GET parameters with the name emotions[] will forward their values in a list to the handler.

- keywords
     - **Values**: *some cats are scary*
     - **Description**: Filter by keyword, free text search. (if not provided, won't affect the filtering.)

- before
     - **Values**: *2017-02-14T00:00:00Z*
     - **Description**: Filter by dreamed at date: will returns any dreams before the given date, inclusive. (if not provided, won't affect the filtering.)

- after
     - **Values**: *2017-02-14T00:00:00Z*
     - **Description**: Filter by dreamed at date: will returns any dreams after the given date, inclusive. (if not provided, won't affect the filtering.)

- limit
     - **Values**: *101, 2, ...*
     - **Description**: Limit the number of results. Defaults to 100 if not provided.

- last_seen_id
     - **Values**: *42*
     - **Description**: The id of the last dream seen, for pagination. Omit for the first page.


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
[{"nightmare":false,"lucid":false,"dreamer_location":{"country":"USA","latitude":null,"city":"Queens","region":null,"longitude":null},"private":false,"emotions":["joy","intimidated"],"recurring":true,"dreamer_zodiac_sign":"Scorpio","date":"2020-07-07T00:00:00Z","starred":true,"dream_id":42,"dreamer_gender":"Female","title":"I dreamed of our alpacas","description":"Some alpacas were wearing sunglasses","dreamer_username":"alpaca.cool69420"}]
```

- A dream with the dream id and dreamer id included, A dream with the dream id and dreamer id included (`application/json;charset=utf-8`):

```javascript
[{"nightmare":false,"lucid":false,"dreamer_location":{"country":"USA","latitude":null,"city":"Queens","region":null,"longitude":null},"private":false,"emotions":["joy","intimidated"],"recurring":true,"dreamer_zodiac_sign":"Scorpio","date":"2020-07-07T00:00:00Z","starred":true,"dream_id":42,"dreamer_gender":"Female","title":"I dreamed of our alpacas","description":"Some alpacas were wearing sunglasses","dreamer_username":"alpaca.cool69420"},{"nightmare":false,"lucid":false,"dreamer_location":{"country":"USA","latitude":null,"city":"Queens","region":null,"longitude":null},"private":false,"emotions":["joy","intimidated"],"recurring":true,"dreamer_zodiac_sign":"Scorpio","date":"2020-07-07T00:00:00Z","starred":true,"dream_id":42,"dreamer_gender":"Female","title":"I dreamed of our alpacas","description":"Some alpacas were wearing sunglasses","dreamer_username":"alpaca.cool69420"}]
```

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
{"token":"some-long-token","user":{"email":"nena@alpaca.com","location":{"country":"Japan","latitude":null,"city":"Tokyo","region":null,"longitude":null},"username":"nena.alpaca","zodiac_sign":"Scorpio","birthday":null,"gender":"Female"}}
```

## GET /api/stats

### Authentication

This part of the API is protected by JSON Web Tokens ([JWTs](https://en.wikipedia.org/wiki/JSON_Web_Token))


Clients must supply the following data
A JWT Token signed with this server's key


### GET Parameters:

- mine
     - **Description**: If specified, will only search the current user's dreams. Because this is a flag, you can call it like this: /api/dreams?mine
     - This parameter is a **flag**. This means no value is expected to be associated to this parameter.

- username
     - **Values**: *nena.alpaca*
     - **Description**: A username. Checks existence. If you provide your own, we'll search private dreams too. If none is provide, search all public dreams.

- city
     - **Values**: *Queens*
     - **Description**: Filter by city (if not provided, won't affect the filtering.)

- region
     - **Values**: *New York*
     - **Description**: Filter by administrative region (state or province) (if not provided, won't affect the filtering.)

- country
     - **Values**: *USA*
     - **Description**: Filter by country name (if not provided, won't affect the filtering.)

- gender
     - **Values**: *male, female, nonBinary*
     - **Description**: Filter by dreamer's gender (if not provided, won't affect the filtering.)

- zodiac_sign
     - **Values**: *capricorn, sagittarius, ...*
     - **Description**: Filter by dreamer's zodiac sign (if not provided, won't affect the filtering.)

- lucid
     - **Values**: *true, false*
     - **Description**: Filter by: is lucid or not  (if not provided, won't affect the filtering.)

- nightmare
     - **Values**: *true, false*
     - **Description**: Filter by: is nightmare or not (if not provided, won't affect the filtering.)

- recurring
     - **Values**: *true, false*
     - **Description**: Filter by: is recurring or not (if not provided, won't affect the filtering.)

- emotions
     - **Values**: *joy*
     - **Description**: Filter by emotions: requires a list, will return dreams that have all the given emotions (if not provided, won't affect the filtering.)
     - This parameter is a **list**. All GET parameters with the name emotions[] will forward their values in a list to the handler.

- keywords
     - **Values**: *some cats are scary*
     - **Description**: Filter by keyword, free text search. (if not provided, won't affect the filtering.)

- before
     - **Values**: *2017-02-14T00:00:00Z*
     - **Description**: Filter by dreamed at date: will returns any dreams before the given date, inclusive. (if not provided, won't affect the filtering.)

- after
     - **Values**: *2017-02-14T00:00:00Z*
     - **Description**: Filter by dreamed at date: will returns any dreams after the given date, inclusive. (if not provided, won't affect the filtering.)

- top
     - **Values**: *10, 100*
     - **Description**: Get the top N keywords and emotions. Returns the top 10 if unspecified, max is 100.


### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Totals represent the sample taken, there could be more! (`application/json;charset=utf-8`, `application/json`):

```javascript
{"top_emotions":[{"recurring_count":0,"nightmare_count":0,"lucid_count":1,"total_dreams":1,"name":"sadness"}],"top_keywords":[{"recurring_count":0,"nightmare_count":0,"lucid_count":1,"total_dreams":1,"top_emotion":"sadness","keyword":"winter"}]}
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
{"email":"nena@alpaca.com","location":{"country":"Japan","latitude":null,"city":"Tokyo","region":null,"longitude":null},"username":"nena.alpaca","zodiac_sign":"Scorpio","birthday":null,"gender":"Female"}
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
{"email":"new.email@alpaca.net","location":null,"username":"New.Alpaca.Name","zodiac_sign":null,"birthday":null,"gender":"NonBinary"}
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
{"nightmare":false,"lucid":false,"dreamer_location":{"latlng":{"lat":1,"lng":0.1},"country_code":"us","country":"USA","administrative":"New York","name":"Manhattan","city":null,"type":"city"},"private":false,"emotions":["joy","intimidated"],"recurring":true,"date":"2020-07-07T00:00:00Z","starred":true,"title":"I dream of Alpacas","description":"Some alpacas were wearing sunglasses"}
```

### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- A dream with the dream id and dreamer id included (`application/json;charset=utf-8`, `application/json`):

```javascript
{"nightmare":false,"lucid":false,"dreamer_location":{"country":"USA","latitude":null,"city":"Queens","region":null,"longitude":null},"private":false,"emotions":["joy","intimidated"],"recurring":true,"dreamer_zodiac_sign":"Scorpio","date":"2020-07-07T00:00:00Z","starred":true,"dream_id":42,"dreamer_gender":"Female","title":"I dreamed of our alpacas","description":"Some alpacas were wearing sunglasses","dreamer_username":"alpaca.cool69420"}
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
{"nightmare":false,"lucid":true,"dreamer_location":{"latlng":{"lat":1,"lng":0.1},"country_code":"us","country":"USA","administrative":"New York","name":"Manhattan","city":null,"type":"city"},"private":true,"emotions":["acceptance"],"recurring":false,"date":null,"starred":false,"title":"I dreamed a dream","description":null}
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
{"new_password":"anotherPassword","current_password":"sample"}
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
{"email":"paco@alpaca.net","location":{"latlng":{"lat":1,"lng":0.1},"country_code":"us","country":"USA","administrative":"New York","name":"Manhattan","city":null,"type":"city"},"username":"Paco.Alpaco","zodiac_sign":"Capricorn","birthday":"2017-02-14T00:00:00Z","gender":"Male","password":"somePassword"}
```

### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"token":"some-long-token","user":{"email":"nena@alpaca.com","location":{"country":"Japan","latitude":null,"city":"Tokyo","region":null,"longitude":null},"username":"nena.alpaca","zodiac_sign":"Scorpio","birthday":null,"gender":"Female"}}
```

## GET /docs

### Response:

- Status code 200
- Headers: []

- No response body

