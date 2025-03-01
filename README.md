# zealline

`zealline` is a readline-like library written in Z80 assembly for the Zeal 8-bit operating system. It provides interactive line editing and history features, making command-line interactions more user-friendly.

## Features

* **Line Editing:**
    * **Ctrl+A:** Move cursor to the beginning of the line.
    * **Ctrl+E:** Move cursor to the end of the line.
    * **Ctrl+C:** Cancel the current line input.
* **History Support:**
    * Allows users to recall and reuse previously entered commands.
    * Improves workflow by minimizing repetitive typing.

## Goals

* Provide a robust and efficient readline-like interface for the Zeal OS.
* Minimize memory footprint to accommodate the limited resources of 8-bit systems.
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
