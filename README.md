# HTML Resource Embedder

HTML Resource Embedder (`htmlresemb`) tries to embed all external resources like CSS, JavaScript and 
image files into an HTML document. It reads the content of those files and modifies the corresponding 
tags (`link`, `style` and `img`) accordingly. CSS and JavaScript files are embedded into a CDATA
section, so in most cases there shouldn't be a problem with characters like `>` in the resource files.
Image files will be converted to a Base64 encoded data URI.

The resulting document is in most cases "portable", that is, it can be copied/moved to other locations 
without breaking the layout. Please note that currently embedding of font files isn't supported. This
may be added in the future but can be a license problem depending on the font.

## Usage

```bash
htmlresemb -i <infile> -o <outfile> [options]
```

- `<infile>`\
  Use <infile> for input. If `-i <infile>` is ommitted, input is read from stdin. In this case a 
  document root path must be supplied via `-docroot`, otherwise loading of resources referenced in 
  the document is impossible.

- `<outfile>`\
  Use `<outfile>` for output. If `-o <outfile>` is ommitted, output goes to stdout.

**Available options (others are silently ignored):**

- `-h, --help`\
  Print help message and exit.

- `-docroot <path>`\
  A path which is used to resolve relative filenames. Relative paths and environment variables can be 
  used in `path`.

- `-css`\
  Embed CSS files.

- `-js`\
  Embed JavaScript files.

- `-img`\
  Embed image files.


If none of the switches `-css`, `-js` and `-img` is used, *all* resources will be embedded.

**Return values:**
    
  - **0** &emsp;Success.
  - **1** &emsp;Invalid/incomplete options or `-h` / `--help` was used.
  - **2** &emsp;An error occured.

Warning or error messages during processing are written to stderr.


## Status

`htmlresemb` is by no means a perfect solution, it's a very quick spin off from one of our other 
internal projects. In general it does it's best and should just work, but it hasn't been tested with 
many different documents. For example encodings other than UTF-8 will fail (mostly). Reading of the 
source document is done with an XML parser, so this also may fail completely (although `htmlresemb` 
tries to fix broken HTML to some extent). Also the output document might contain slightly different 
whitespace.


## Development

`htmlresemb` is written in Free Pascal, the Mac and Windows binaries (see [Releases](../../releases/)) 
were created with compiler version 2.6.4 and Lazarus 1.4.4. You also need the `LazUtils` package 
(part of FPC) and [Internet Tools](https://github.com/benibela/internettools). The Internet Tools 
version used for `htmlresemb` is available as a Zip file in the [Releases](../../releases/) section.

The source code contains a slightly modified 
version of `xmlwriter.pas` to overcome indenting and whitespace problems when creating the result 
document. If you want to use a newer compiler version this maybe no longer necessary, but it's very 
likely that you have to change larger parts of the source code due to the new Unicode string 
handling in FPC 3.x. The Lazarus project file depends on `strip` as a post compile command, on the Mac
this should be available if the Apple developer tools are installed, on Windows using Cygwin/MinGW is 
one possible solution.


## Credits

- [Free Pascal](https://www.freepascal.org)
- [Lazarus](https://www.lazarus-ide.org)
- [Benito van der Zander](http://www.benibela.de) (Internet Tools)

## License

MIT © [idesis GmbH](http://www.idesis.de), Rellinghauser Straße 334F, D-45136 Essen

See the `LICENSE` file for details.
