# net

```zeru,ignore
import std.net;
```

Addresses are text: `"127.0.0.1:8080"` for IPv4, `"[::1]:8080"` for IPv6,
or a file path for Unix sockets. Port 0 lets the system pick a free port,
which `local_addr()` then tells. Every socket closes when it goes out of
scope.

## TCP

A `TcpListener` waits for connections, and each one becomes a `TcpStream`:

```zeru
import std.net;

var listener = net::TcpListener.bind("127.0.0.1:0").unwrap();
var client = net::TcpStream.connect(listener.local_addr().unwrap()).unwrap();
var server = listener.accept().unwrap();

client.write("ping").unwrap();
var request = String.new();
server.read(&var request, 64).unwrap();
server.write("pong").unwrap();
var reply = String.new();
client.read(&var reply, 64).unwrap();
println("{} {}", request, reply); // ping pong
```

| Function | Does |
|---|---|
| `TcpListener.bind(addr)` | listens on `addr`; `"0.0.0.0:80"` listens on every address |
| `listener.accept() TcpStream!` | waits for the next connection |
| `TcpStream.connect(addr)`, `connect_timeout(addr, ms)` | connects to a server |
| `s.read(&var buf, count) usize!` | up to `count` bytes added to `buf`; 0 when the other end closed |
| `s.peek(&var buf, count) usize!` | the same, leaving the bytes to be read again |
| `s.write(text) !` | sends all of `text` |
| `s.set_timeout(ms) !` | reads and writes give up with `ETIMEDOUT` after `ms`; `None` waits forever |
| `s.set_nodelay(on) !` | sends small writes at once |
| `local_addr()`, `peer_addr()` | this end's and the other end's address |

## UDP

A `UdpSocket` sends and receives single messages, datagrams:

```zeru
import std.net;

var a = net::UdpSocket.bind("127.0.0.1:0").unwrap();
var b = net::UdpSocket.bind("127.0.0.1:0").unwrap();
a.send_to(b.local_addr().unwrap(), "hello").unwrap();
var got = b.recv().unwrap();
println("{} from a: {}", got.data, got.from == a.local_addr().unwrap()); // hello from a: true
```

| Function | Does |
|---|---|
| `UdpSocket.bind(addr)` | opens a socket on `addr` |
| `u.send_to(addr, data) !` | sends one datagram |
| `u.connect(addr) !`, `u.send(data) !` | fixes the other end, then sends without an address |
| `u.recv() Datagram!`, `u.peek() Datagram!` | the next datagram, its `data` and who it is `from` |
| `u.set_timeout(ms)`, `u.set_broadcast(on)` | options |

## Unix sockets

`UnixListener.bind(path)` and `UnixStream.connect(path)` work like their
TCP versions, between programs on the same machine. The listener removes
its socket file when it goes out of scope.

## Many connections at once

A Zeru program runs on one thread. To serve many clients, it asks which
sockets have something to read and handles just those. `handle()` gives a
socket's `Handle`, and `net::wait(handles, ms)` waits until some of them
are ready and returns their indexes:

```zeru,ignore
var handles: Vec<net::Handle> = Vec.new();
handles.push(listener.handle());
for client in clients {
    handles.push(client.handle());
}
var ready = net::wait(&handles, None).unwrap();
```

Index 0 ready means a new connection waits to be accepted; any other means
that client sent data or closed. The [Echo Server](../examples/echo-server.md)
is a complete example.
