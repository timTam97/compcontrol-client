# compcontrol-client-hs

A re-implementation of [Pushbullet Automation,](https://github.com/timTam97/pushbullet-automation) written in Haskell and re-engineered for a [custom API](https://github.com/timTam97/compcontrol-api) deployed on AWS. Allows you to remotely control your computer by making web requests.

## Getting Started

### Things needed
- A deployment of [compcontrol-api](https://github.com/timTam97/compcontrol-api)
- [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

### Setup

The following instructions assume you have already deployed [compcontrol-api](https://github.com/timTam97/compcontrol-api) on AWS and have access to the outputs.

Grab the `WebSocketURI` output from the deployed stack in CloudFormation. Add a new system environment variable called `AWS_WSS_URI` and set its value as this.

Then, clone this repository (or download the source and extract) and run `stack install`. Stack will download the GHC compiler as well as all the required dependencies and create an executable. This will take some time. After the build is finished, take this executable and place it somewhere convenient.

## Running / Usage
Simply double click the executable to run the program. The program runs silently, so no console window will pop up. In the folder where you started the program, there should be a text file called `run.log`. Open this file to verify that the program is successfully connected to the deployed API.

See the [REST API](https://github.com/timTam97/compcontrol-api#rest-api-for-sending-commands) section in the API repository for instructions on how to remotely send commands to your machine.

### Other bits

The program will automatically attempt to reconnect to the API if the connection is disrupted (eg. waking from sleep), so you won't need to start it again every time. For added convenience, place a shortcut to the executable in `C:\ProgramData\Microsoft\Windows\Start Menu\Programs\StartUp` to make the program run when your computer starts up.
