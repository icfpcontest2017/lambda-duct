# Lambda Duct

Lambda Duct is an offline mode simulator for the Lambda Punter
game. It acts as a bridge between a client in offline mode and an online
game server.

## Instructions

### Installing

The program can either be automatically installed via [OPAM](https://opam.ocaml.org)

```
$ opam pin add lambda-duct https://github.com/icfpcontest2017/lambda-duct.git
```

or built from source using [OCamlbuild](https://github.com/ocaml/ocamlbuild)

```
$ make
```

Either approach produces a binary `lamduct`. If built from source this
binary must be manually installed into a suitable directory.

### Running

The Lambda Duct binary takes as input the name of client program, i.e.

```
$ lamduct ./my_punter_program
```

This invokes `my_punter_program` expecting it to follow the offline
mode protocol. By default the simulator connects to the remote host
`punter.inf.ed.ac.uk:9999`. You can change this by passing a hostname
and a port as arguments to the appropriate command line options. To
see the list of options invoke `lamduct --help`.

```
$ lamduct --help
usage: lamduct [options] <client program>
Options are:
  --client-instance-logfile  Logging client instance stderr               (default: /dev/null)
  --client-instance-timeout  Maximum lifetime per client program instance (default: 10 seconds)
  --game-hostname            Hostname of the game server                  (default: punter.inf.ed.ac.uk)
  --game-port                Port to connect to on the game server        (default: 9999)
  --log-level                Logging level for lamduct (values: 0 to 3)   (default: 0)
  --version                  Print version and exit
  -help                      Display this list of options
  --help                     Display this list of options
```

Lambda Duct communicates with the client program via its standard in
and standard out, meaning that the client program must only write
well-formed Lambda Punter messages to standard out. In other words: do
not write any debugging information to standard out. Instead use
standard error to print debugging information. By default the client
program's standard error is redirected to `/dev/null`, however, this
behaviour can be changed by providing a file name to
`--client-instance-logfile`, e.g.

```
$ lamduct --client-instance-logfile punt.log my_punter_program
```

Lambda Duct can itself be made to produce some debugging
information. The log level option (`--log-level`) determines the degree of information produced:

  * Level 0 is the least logging level which only prints errors
  * Level 1 prints (some possibly helpful) warnings.
  * Level 2 produces information about regarding client program instances and communication with the online server.
  * Level 3 does the same as level 2, but more verbosely.

## Timeouts

Lambda Duct does not simulate offline mode timeouts. The timeout
policy is determined by the online game server.
