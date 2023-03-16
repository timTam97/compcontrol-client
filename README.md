# compcontrol-client

This app allows you to remotely control your Windows machine by making web requests. You can lock, sleep, hibernate and shut down your PC from your phone, smartwatch, or any other connected device.

- [Setup](#setup)
  - [Get your API key](#get-your-api-key)
  - [Download/build the executable](#downloadbuild-the-executable)
- [Running the client](#running-the-client)
- [Sending commands](#sending-commands)
  - [The easy way](#the-easy-way)
  - [The slightly-less-easy way](#the-slightly-less-easy-way)
- [Other bits](#other-bits)
- [Some FAQs no one cares about](#some-faqs-no-one-cares-about)
  - [How did this project come about?](#how-did-this-project-come-about)
  - [Linux support?](#linux-support)

## Setup

### Get your API key

If you don't have an API key already, visit https://command.timsam.au/getkey to get an API key.

Then, create a system environment variable called `WSS_TOKEN` and set its value as your API key.

### Download/build the executable

You can either download a pre-built [release](https://github.com/timTam97/compcontrol-client-hs/releases) executable or download the source and build with `stack build`.

Goes without saying but if you're building from source you'll need the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/).

## Running the client

Simply double click the executable to run the program. The program runs silently, so no console window will pop up. In the folder where you started the program, there should be a text file called `run.log`. Open this file to verify that the program is successfully connected to the deployed API.

## Sending commands

### The easy way

[Here's](https://www.icloud.com/shortcuts/af276db458e5453489a61991d12dcdda) a shortcut for your iPhone that will enable you to control your computer from your phone. Have your API key handy when you're setting the shortcut up.

[Here's](https://github.com/timTam97/compcontrol-alexa) some code for an Alexa skill that you can deploy to control your computer by talking to Alexa.

### The slightly-less-easy way

The URL to send commands to is `https://command.timsam.au/send/<command>`.

-   Replace `<command>` with the command you want to send to the client.
    -   ❗ The client will only respond to `sleep`, `shutdown`, `lock` and `hibernate` commands.
-   Place your API key in the `auth` part of the header.
-   Send a `POST` request.

For example, if you wanted to send a sleep command and your token was `fwolXHYtGMbmAg`:

```
curl -H "auth: fwolXHYtGMbmAg" \
     -X POST https://command.timsam.au/send/sleep
```

The API will return a status code and JSON indicating success or failure.

## Other bits

The program will automatically attempt to reconnect to the API if the connection is disrupted (eg. waking from sleep or hibernation), so you won't need to start it again every time.

**Known limitation:** If the connection is forcibly interrupted (ie. connecting to VPN, switching networks), then the client will fatally error out and you'll need to relaunch. This issue is under investigation.

If you want to make the program run when your computer starts up, place a shortcut to the executable in `C:\ProgramData\Microsoft\Windows\Start Menu\Programs\StartUp`.

## Some FAQs no one cares about

### How did this project come about?

This project is a re-implementation of [Pushbullet Automation](https://github.com/timTam97/pushbullet-automation), which is a Python project. I took a class on functional programming and decided it would be a good idea to re-write some of my older projects in Haskell.

This project orginally interfaced with the Pushbullet API but when I realised I could build out my own backend on AWS I re-wrote the client to make use of it. [Here's](https://github.com/timTam97/compcontrol-client/releases/tag/v1.0.1) the final release of this client that used the Pushbullet API. It should still work, but don't consider it supported. Any changes to the Pushbullet API would stand a high chance of breaking things (probably).

You can find the backend API infrastructure I built on AWS [here](https://github.com/timTam97/compcontrol-api).

### Linux support?

For executing system commands I make use of the Windows API. This necessitates the use of the Win32 package, which is obviously limited to Windows systems. My main problem is that the Haskell Stack isn't very good at specifying conditional dependencies (see [here](https://github.com/commercialhaskell/stack/issues/2048), [here](https://github.com/commercialhaskell/stack/issues/3369)), and any potential workarounds seem a bit iffy to me (like generating `package.yaml` on the fly after detecting the OS).

To be fair though, doing the above workaround isn't challenging at all, and neither is writing platform-specific code in Haskell, so Linux support is definitely something that could be easily implemented. I'll just have to get off my high horse. Eventually.
