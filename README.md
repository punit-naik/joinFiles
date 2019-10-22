# joinFiles

Hasura Programming Challenge

**NOTE**: You will require to install `stack` and `ghc` in order to work on this project

## Build

```
stack build
```

## Run

```
stack run -- authors.json articles.json filter=first5 filter=ratingGreater3
```

The output will be written to a JSON file named `authors_with_articles.json` in the current directory

**NOTE**: 
* First argument is the path to the authors json file
* Second argument is the path to the articles json file
* Remaining two arguments can be passed as `filter=first5` or `filter=ratingGreater3` which limits the articles per user to 5 and only fetches articles which have a rating greater than 3 respectively. 

## Test

```
stack test
```
