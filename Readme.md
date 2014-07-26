Erlang Mikrotik client
======================
### Connecting
#### Api connection
To connect to mikrotik start new connection process with `erotik:api_connect/5`. __Note__, that connection process registers locally.   
Example:

    Name = "My super router",
    Host = "127.0.0.1",
    Port = 8728,
    Login = "admin",
    Password = "password",
    {ok, Worker} = erotik:api_connect(Name, Host, Port, Login, Password).
When you start connection - it automatically connects to router and tries to authorize. That's why connections are locally registered. 
It is cheaper to use one connection per router, than start and auth new connection each time. __Note__ authorization may not succeed 
at the first attempt, I don't know why router behaves such way, but advice to put connection under `supervisor`, as permanent or transient.  

#### SSH connection
To connect to mikrotik through ssh - use `erotik:ssh_connect/5`. __Note__, that connection process registers locally.  
Example:

    Name = "My super router",
    Host = "127.0.0.1",
    Port = 8728,
    Login = "admin",
    Password = "password",
    {ok, Worker} = erotik:ssh_connect(Name, Host, Port, Login, Password).
__Note__, for ssh connection to work - all dependend application should be started!  
    
    ok = application:start(crypto).
    ok = application:start(asn1).
    ok = application:start(public_key).
    ok = application:start(ssh).

### Executing commands
Now you can use api module `erotik` to call commands. There are several ready commands and `command/2` function, to which 
you can pass your own commands.  
Command `command` takes router's pid or atom name as first argument and list of string words as second arguments.  
Example:

    erotik:command('My super router', ["/ip/firewall/filter/set", "=disabled=no", "=.id=*D"]).
    
### Configuring
Note, that default wait for response time is infinity. To avoid this - you can set env `me_connector_wait_time` in your `application.conf` file,
providing positive integer number, which will be a new delay value.