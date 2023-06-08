# Nosock


Some experiments on the implementation of the websocket client protocol on OCaml.

## Testing

To testing implementation i use [autobahn-testsuite](https://github.com/crossbario/autobahn-testsuite). Сurrent version passes all tests.

To run tests we can use the following commands
```bash
cd path/to/this/repo
docker run -it --rm -v "${PWD}/test:/config" -v "${PWD}/test/reports:/reports" -p 9001:9001 --name fuzzingserver crossbario/autobahn-testsuite
dune test
```

## TODO

- [ ] Refactoring. The current code is a bit dirty.
- [ ] Documentaion.
- [ ] SSL.
