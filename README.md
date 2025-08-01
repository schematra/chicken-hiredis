# Redis Client for CHICKEN Scheme

A Redis client library for CHICKEN Scheme that provides a simple interface to interact with Redis servers using the hiredis C library.

## Features

- Execute Redis commands with automatic reply parsing
- Support for most Redis reply types (strings, integers, arrays, etc.)
- Simple Scheme-friendly API
- Support for pubsub

## Dependencies

- hiredis C library

## Installation

Make sure you have the hiredis library installed on your system:

```bash
# On macOS with Homebrew
brew install hiredis

# On Ubuntu/Debian
sudo apt-get install libhiredis-dev

# On CentOS/RHEL
sudo yum install hiredis-devel
```

## Building & Installing

Just run chicken-install in the repo root.

```sh
chicken-install
```

## Usage

### Basic Example

```scheme
(import hiredis)

;; Connect to Redis server
(define ctx (redis-connect "127.0.0.1" 6379))
(redis-context ctx)

;; Execute Redis commands
(redis-command "SET" "mykey" "myvalue")
(redis-command "GET" "mykey")
(redis-command "HSET" "myhash" "field1" "value1")
(redis-command "HGET" "myhash" "field1")
(redis-command "KEYS" "*")
```

### API Reference

#### `redis-connect`
Connect to a Redis server.

**Parameters:**
- `hostname`: Redis server hostname or IP address (string, optional, defaults to "localhost")
- `port`: Redis server port number (integer, optional, defaults to 6379)

**Returns:** Redis connection context pointer for use with redis-command

**Examples:**
```scheme
(define ctx (redis-connect "127.0.0.1" 6379))
(define ctx (redis-connect "localhost" 6379))
(define ctx (redis-connect))  ; Uses defaults: localhost:6379
(define ctx (redis-connect "redis.example.com"))  ; Uses default port 6379
```

After connecting you will need to set the context for all the redis
functions. This ensures that your local thread has a context
available.

```scheme
(redis-context ctx)
```

#### `redis-command`
Execute a Redis command with optional arguments.

**Parameters:**
- `command`: Redis command string (e.g., "GET", "SET", "HGET")
- `rest`: Optional command arguments

**Returns:** Scheme object representation of Redis reply

**Examples:**
```scheme
(redis-command "GET" "mykey")
(redis-command "SET" "mykey" "myvalue")
(redis-command "HGET" "myhash" "field")
```

#### `redis-subscribe`
Subscribe to a Redis channel using pattern matching (internally it
uses the `PSUBSCRIBE` command).

**Parameters:**
- `channel`: Channel pattern to subscribe to (string, supports wildcards like `*` and `?`)
- `callback`: Callback function that receives each message

**Callback Function:**
The callback receives a list with 4 elements for pattern messages:
`("pmessage" "pattern" "channel" "message")`

The callback should return `#t` to continue listening, or `#f` to unsubscribe.

**Examples:**
```scheme
;; Subscribe to all channels starting with "test"
(redis-subscribe ctx "test*" 
  (lambda (reply)
    (let ((msg (list-ref reply 3)))
      (format #t "Received: ~A\n" msg)
      (not (string=? msg "quit")))))

;; Subscribe to a specific channel
(redis-subscribe ctx "notifications"
  (lambda (reply)
    (let ((channel (list-ref reply 2))
          (message (list-ref reply 3)))
      (format #t "Channel ~A: ~A\n" channel message)
      #t))) ; Continue listening indefinitely
```

### Reply Types

The library automatically converts Redis replies to appropriate Scheme objects:

- **String replies** → Scheme strings
- **Integer replies** → Scheme integers
- **Array replies** → Scheme lists
- **Nil replies** → `#f`
- **Status replies** → Scheme strings
- **Error replies** → `(error . "error message")`
- **Double replies** → Scheme floats

## Files

- `hiredis.scm` - Main Redis client module
- `test.scm` - Example usage and tests
- `redis_helpers.c` - C helper functions for hiredis integration

## License

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

See the source code for the complete license text.
