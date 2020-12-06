# compcontrol-client-hs

A re-implementation of [Pushbullet Automation,](https://github.com/timTam97/pushbullet-automation) written in Haskell and re-engineered for a [custom API](https://github.com/timTam97/compcontrol-api) deployed on AWS. Allows you to remotely control your computer by making web requests.

## Getting Started

### Setup

If you don't have an API key already, visit https://command.timsam.live/getkey to get an API key. Then, create a system environment variable called `WSS_TOKEN` and set its value as your API key.

Then, either download a pre-built [release](https://github.com/timTam97/compcontrol-client-hs/releases) executable or download the source and build with `stack build`.

## Running / Usage
Simply double click the executable to run the program. The program runs silently, so no console window will pop up. In the folder where you started the program, there should be a text file called `run.log`. Open this file to verify that the program is successfully connected to the deployed API.

See the [REST API](https://github.com/timTam97/compcontrol-api#rest-api-for-sending-commands) section in the API repository for instructions on how to remotely send commands to your machine. You will need your API key for this.

### Other bits

The program will automatically attempt to reconnect to the API if the connection is disrupted (eg. waking from sleep), so you won't need to start it again every time. For added convenience, place a shortcut to the executable in `C:\ProgramData\Microsoft\Windows\Start Menu\Programs\StartUp` to make the program run when your computer starts up.
