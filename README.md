# pushbullet-automation-haskell

**Note: This branch interfaces with the Pushbullet API and is no longer maintained. See the `master` branch for the updated version (not compatible with the Pushbullet API).**

A Haskell re-implementation of [Pushbullet Automation.](https://github.com/timTam97/pushbullet-automation) Allows you to remotely control your computer by making web requests.

## Getting Started

### Things needed
- A [Pushbullet](https://www.pushbullet.com/) access token
- [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

### Setup
Add a new system environment variable called `PB_TOKEN_SYS` and set its value as your Pushbullet access token. Clone this repository (or download the source and extract) and run `stack install`. Stack will download the GHC compiler as well as all the required dependencies and create an executable. This will take some time. After the build is finished, take this executable and place it somewhere convenient.

## Running / Usage
Simply double click the executable to run the program. The program runs silently, so no console window will pop up. In the folder where you started the program, there should be a text file called `run.log`. Open this file to verify that the program is successfully connected to the Pushbullet API.

Currently, the program supports locking, sleeping, hibernating and shutting down the computer remotely. To execute commands, simply send a web request to the Pushbullet API using your access token. Here's an example:
```bash
curl --header 'Access-Token: <your_access_token_here>' \
     --header 'Content-Type: application/json' \
     --data-binary '{"body":"lock","title":"COMPUTER-NAME-HERE","type":"note"}' \
     --request POST \
     https://api.pushbullet.com/v2/pushes
```
### Important bits
In the JSON body of the web request, you should specify the action you want to take:
- `shut down` to shut down the computer
- `hibernate` to hibernate
- `sleep` to sleep
- `lock` to lock

For the title, this should be the name of your computer.

Pusbullet has a limit of 500 pushes a month for free plans. This should be fairly hard to exceed given normal use.

The program will automatically attempt to reconnect to the Pushbullet API if the connection is disrupted (eg. waking from sleep), so you won't need to start it again every time. For added convenience, place a shortcut to the executable in `C:\ProgramData\Microsoft\Windows\Start Menu\Programs\StartUp` to make the program run when your computer starts up.
