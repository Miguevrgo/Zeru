# Echo Server

This server sends every message it receives back to whoever sent it. It
serves any number of clients at once, from one thread:

```sh
$ zeru run echo.zr
listening on 127.0.0.1:7000
127.0.0.1:59156 connected
127.0.0.1:59162 connected
127.0.0.1:59156 left
```

Try it from another terminal with `nc 127.0.0.1 7000`, or several at once.

```zeru,check
import std.net;

fn main() {
    var listener = net::TcpListener.bind("127.0.0.1:7000").unwrap();
    println("listening on {}", listener.local_addr().unwrap());
    var clients: Vec<net::TcpStream> = Vec.new();
    while true {
        var handles: Vec<net::Handle> = Vec.new();
        handles.push(listener.handle());
        for client in clients {
            handles.push(client.handle());
        }
        var ready = net::wait(handles[..], None).unwrap();
        var closed: Vec<usize> = Vec.new();
        for index in ready {
            if index == 0 {
                var client = listener.accept().unwrap();
                println("{} connected", client.peer_addr().unwrap());
                clients.push(client);
                continue;
            }
            var client = &clients[index - 1];
            var data = String.new();
            var got = client.read(&var data, 1024);
            if got.is_err() || got.unwrap() == 0 || client.write(data).is_err() {
                closed.push(index - 1);
            }
        }
        while closed.len() > 0 {
            var gone = clients.remove(closed.pop().unwrap());
            println("{} left", gone.peer_addr().unwrap());
        }
    }
}
```

## How it works

**One loop, many clients.** Each turn of the loop builds the list of
sockets to watch: the listener first, then every client. `net::wait`
sleeps until at least one of them has something, and returns which. The
program never blocks on a single client, so a slow one cannot hold up the
rest.

**What ready means.** Index 0 is the listener: a new client is waiting, and
`accept` will not block. Any other index is a client that sent data, or
closed its end. `read` then returns the data, or 0 when the client left.

**Borrowing a client.** `var client = &clients[index - 1];` reads and
writes through a reference, without taking the stream out of the `Vec`.

**Removing clients.** Closed clients are not removed inside the loop, where
removing would shift the indexes of the ones not handled yet. Their
indexes are collected in `closed`, in increasing order, and removed from
the last one back. `clients.remove` gives the stream back, and the stream
closes its socket when it goes out of scope, at the end of that turn.

## Try it

- Prefix every echoed message with the number of clients connected.
- Send each message to every other client too, for a small chat server.
- Disconnect a client that stays silent for 30 seconds: keep an
  `os::Clock` per client and pass a timeout to `net::wait`.
