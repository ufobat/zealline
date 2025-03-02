# zealline

`zealline` is a readline-like library written in Z80 assembly for the Zeal 8-bit operating system. It provides interactive line editing and history features, making command-line interactions more user-friendly.

## Features

* **Line Editing:**
    * **Ctrl+a:** Move cursor to the beginning of the line.
    * **Ctrl+e:** Move cursor to the end of the line.
    * **Ctrl+c:** Cancel the current line input.

## TODO

* **History Support:**
    * **Ctrl+r** Search in history.
    * **Cursor Up/Down** Browse the history.
* **Prompt Placeholders**
    * Details are TBD, but so far it could be: CURDIR, TIME, DATE
* **Prompt Colors**
    * Details are TBD, support for the Zeal 8bit OS foreground and background color codes.
* **Multiple Keyboard Layout**
    * Details are TBD

## Goals

* Provide a robust and efficient readline-like interface for the Zeal OS.
* Offer a user-friendly experience with intuitive command-line editing features.
* Implement a reliable command history mechanism.
* Easy integration into Zeal OS applications.
* Be well documented and easy to understand for other developers.

## Usage

To integrate `zealline` into your Zeal OS application, you will need to:

1.  Include the `zealline` assembly source files in your project.
2.  Call the appropriate `zealline` routines to initialize and use the line editing and history features.
3.  Handle the returned input line from `zealline` in your application.

## Contributing

Contributions are welcome! If you find a bug, have a feature request, or want to contribute code, please open an issue or submit a pull request.
